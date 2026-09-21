options(cli.num_colors = 256)

library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)
library(yaml)

# -------------------------------------------------------------------------
# DATA PREP ENGINE
# -------------------------------------------------------------------------

# Transform text columns and map categorical values (non-destructive)
recast_l1_data <- function(df, cfg) {

  # Recast & Clean Character Columns
  for (col in cfg$type_checks$character) {
    if (col %in% names(df)) {
      vals <- str_to_upper(str_squish(as.character(df[[col]])))

      junk_patterns <- c(
        "^0+$",                 # "0", "00", etc.
        "^\\?+$",               # "?", "???", etc.
        "^SEE\\s+",             # Cross-references like "SEE TOK", "SEE SLANA"
        "^(N/A|NA|NONE|NULL)$"  # Standard null strings
      )

      is_junk <- str_detect(vals, paste(junk_patterns, collapse = "|"))
      vals[is_junk | vals == ""] <- NA_character_

      df[[col]] <- vals
    }
  }

  # Map Categorical Overrides
  if ("category_mappings" %in% names(cfg)) {
    for (col in names(cfg$category_mappings)) {
      if (col %in% names(df)) {
        mapping_vec <- unlist(cfg$category_mappings[[col]])
        names(mapping_vec) <- str_to_upper(str_trim(names(mapping_vec)))
        df[[col]] <- unname(mapping_vec[df[[col]]])
      }
    }
  }

  # Recast Numeric
  for (col in cfg$type_checks$numeric) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(str_replace_all(df[[col]], "[$,\\s]", ""))
    }
  }

  # Recast Logical
  for (col in cfg$type_checks$logical) {
    if (col %in% names(df)) df[[col]] <- as.logical(df[[col]])
  }

  # Calculate derived columns
  if (all(c("actual_rate", "residential_rate") %in% names(df))) {
    df <- df %>% mutate(effective_residential_rate = residential_rate - actual_rate)
  }

  if (all(c("fuel_used_gallons", "diesel_kwh_generated") %in% names(df))) {
    df <- df %>%
      mutate(
        diesel_efficiency = if_else(
          is.na(fuel_used_gallons) | is.na(diesel_kwh_generated) | diesel_kwh_generated == 0,
          NA_real_,
          fuel_used_gallons / diesel_kwh_generated
        )
      )
  }

  return(df)
}

# Flag values that fall outside YAML boundaries WITHOUT mutating them to NA
enforce_l1_bounds <- function(df, cfg) {
  if (!"bounds" %in% names(cfg)) return(list(data = df, violations = tibble::tibble()))

  cli_h2("Detecting Range Boundary Violations")

  violation_list <- list()
  has_id <- "identifier" %in% names(df)

  for (col in names(cfg$bounds)) {
    limits <- cfg$bounds[[col]]

    target_cols <- if ("target_columns" %in% names(limits)) {
      as.character(unlist(limits$target_columns))
    } else if ("target_column" %in% names(limits)) {
      as.character(unlist(limits$target_column))
    } else {
      col
    }

    if (!col %in% names(df)) {
      cli_alert_warning(
        "Configured bounds column {.var {col}} was not found in dataset. Skipping rule."
      )
      next
    }

    val_vector <- df[[col]]
    col_has_violations <- FALSE

    low_mask  <- rep(FALSE, length(val_vector))
    high_mask <- rep(FALSE, length(val_vector))
    rule_desc <- ""

    # Lower bound check
    if ("min" %in% names(limits) && !is.null(limits$min) && !is.na(limits$min)) {
      min_val <- as.numeric(limits$min)
      allow_z <- if ("allow_zero" %in% names(limits)) as.logical(limits$allow_zero) else TRUE

      if (!allow_z && min_val == 0) {
        low_mask <- !is.na(val_vector) & val_vector <= min_val
        rule_desc <- paste0("<= ", min_val)
      } else {
        low_mask <- !is.na(val_vector) & val_vector < min_val
        rule_desc <- paste0("< ", min_val)
      }
    }

    # Upper bound check
    if ("max" %in% names(limits) && !is.null(limits$max) && !is.na(limits$max)) {
      max_val <- as.numeric(limits$max)
      high_mask <- !is.na(val_vector) & val_vector > max_val
    }

    for (t_col in target_cols) {
      if (!t_col %in% names(df)) {
        cli_alert_warning(
          "Target column {.var {t_col}} (configured for {.var {col}}) was not found. Skipping."
        )
        next
      }

      target_vector <- df[[t_col]]

      # Record low violations
      if (any(low_mask, na.rm = TRUE)) {
        col_has_violations <- TRUE
        bad_rows <- which(low_mask)
        bad_ids  <- if (has_id) as.character(df$identifier[low_mask]) else NA_character_

        cli_alert_warning(
          "Column {.var {t_col}} (via {.var {col}}): Flagged {sum(low_mask)} value(s) below min of {limits$min}."
        )

        violation_list[[length(violation_list) + 1]] <- tibble::tibble(
          identifier          = bad_ids,
          row_index           = bad_rows,
          target_column       = t_col,
          observed_value      = as.character(target_vector[low_mask]),
          eval_column         = col,
          eval_value_observed = as.character(val_vector[low_mask]),
          rule_broken         = rule_desc,
          violation_level     = "L1_BOUNDS"
        )
      }

      # Record high violations
      if (any(high_mask, na.rm = TRUE)) {
        col_has_violations <- TRUE
        bad_rows <- which(high_mask)
        bad_ids  <- if (has_id) as.character(df$identifier[high_mask]) else NA_character_

        cli_alert_warning(
          "Column {.var {t_col}} (via {.var {col}}): Flagged {sum(high_mask)} value(s) exceeding max of {limits$max}."
        )

        violation_list[[length(violation_list) + 1]] <- tibble::tibble(
          identifier          = bad_ids,
          row_index           = bad_rows,
          target_column       = t_col,
          observed_value      = as.character(target_vector[high_mask]),
          eval_column         = col,
          eval_value_observed = as.character(val_vector[high_mask]),
          rule_broken         = paste0("> ", limits$max),
          violation_level     = "L1_BOUNDS"
        )
      }
    }

    if (!col_has_violations && ("min" %in% names(limits) || "max" %in% names(limits))) {
      cli_alert_success("Column(s) {.var {target_cols}}: All values within bounds.")
    }
  }

  return(list(data = df, violations = list_rbind(violation_list)))
}

# -------------------------------------------------------------------------
# COORDINATOR & RUNNER
# -------------------------------------------------------------------------

l1_check_quality <- function(path_in, config, path_out, path_log_out) {
  file_name <- path_file(path_in)
  cli_h1("Running Data Quality Flags: {.file {file_name}}")

  cfg_whole <- read_yaml(config)
  config_key <- path_ext_remove(file_name) %>% str_remove("_\\d{4}-\\d{2}(-\\d{2})?$")

  if (!config_key %in% names(cfg_whole)) {
    stop(paste("Target config key", config_key, "not found in", config, "YAML."))
  }
  cfg <- cfg_whole[[config_key]]

  df <- read_csv(
    path_in,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  # Process data (Recast -> Detect L1 Bounds without setting to NA)
  df <- recast_l1_data(df, cfg)
  
  bounds_result <- enforce_l1_bounds(df, cfg)
  df <- bounds_result$data
  violations_df <- bounds_result$violations

  # Write log to file for the review tool
  dir_create(dirname(path_log_out))
  write_csv(violations_df, file = path_log_out)
  cli_alert_info("Quality log saved to {.file {path_file(path_log_out)}} ({nrow(violations_df)} entries).")

  # Write full un-scrubbed dataset to file (ready for L2 detection)
  dir_create(dirname(path_out))
  write_csv(df, file = path_out)
  cli_alert_success("Success! Prepared {file_name} for L2 evaluation at {.file {path_file(path_out)}}.")
}