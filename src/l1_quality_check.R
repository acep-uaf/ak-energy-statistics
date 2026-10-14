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

recast_l1_data <- function(df, cfg) {

  # Recast & Clean Character Columns
  for (col in cfg$type_checks$character) {
    if (col %in% names(df)) {
      vals <- str_to_upper(str_squish(as.character(df[[col]])))

      junk_patterns <- c(
        "^\\?+$",               # "?", "???", etc.
        "^SEE\\s+",             # Cross-references like "SEE TOK"
        "^(N/A|NA|NONE|NULL)$"  # Standard null strings
      )

      is_junk <- str_detect(vals, paste(junk_patterns, collapse = "|"))
      vals[is_junk | vals == ""] <- NA_character_

      df[[col]] <- vals
    }
  }

  # Map Categorical Overrides (Non-destructive)
  if ("category_mappings" %in% names(cfg)) {
    for (col in names(cfg$category_mappings)) {
      if (col %in% names(df)) {
        mapping_vec <- unlist(cfg$category_mappings[[col]])
        names(mapping_vec) <- str_to_upper(str_trim(names(mapping_vec)))
        
        matched_vals <- mapping_vec[df[[col]]]
        df[[col]] <- ifelse(!is.na(matched_vals), unname(matched_vals), df[[col]])
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

# Flag values outside YAML boundaries (Strict < min and > max)
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

    # Retrieve the raw evaluation column vector
    eval_vector <- if (col %in% names(df)) df[[col]] else NULL

    for (t_col in target_cols) {
      if (!t_col %in% names(df)) {
        cli_alert_warning(
          "Target column {.var {t_col}} (configured under {.var {col}}) not found in dataset. Skipping."
        )
        next
      }

      val_vector <- df[[t_col]]
      low_mask   <- rep(FALSE, length(val_vector))
      high_mask  <- rep(FALSE, length(val_vector))

      # Strictly flag values less than min
      if ("min" %in% names(limits) && !is.null(limits$min) && !is.na(limits$min)) {
        min_val  <- as.numeric(limits$min)
        low_mask <- !is.na(val_vector) & val_vector < min_val
      }

      # Strictly flag values greater than max
      if ("max" %in% names(limits) && !is.null(limits$max) && !is.na(limits$max)) {
        max_val   <- as.numeric(limits$max)
        high_mask <- !is.na(val_vector) & val_vector > max_val
      }

      # Record low violations
      if (any(low_mask, na.rm = TRUE)) {
        bad_rows <- which(low_mask)
        bad_ids  <- if (has_id) as.character(df$identifier[bad_rows]) else NA_character_
        
        # Pull actual observed value from eval_column if present, else fallback
        eval_obs <- if (!is.null(eval_vector)) as.character(eval_vector[bad_rows]) else as.character(val_vector[bad_rows])

        cli_alert_warning(
          "Column {.var {t_col}} (rule from {.var {col}}): Flagged {length(bad_rows)} value(s) < {limits$min}."
        )

        violation_list[[length(violation_list) + 1]] <- tibble::tibble(
          identifier          = bad_ids,
          row_index           = bad_rows,
          target_column       = t_col,
          observed_value      = as.character(val_vector[bad_rows]),
          eval_column         = col,
          eval_value_observed = eval_obs,
          rule_broken         = paste0("< ", limits$min),
          violation_level     = "L1_BOUNDS"
        )
      }

      # Record high violations
      if (any(high_mask, na.rm = TRUE)) {
        bad_rows <- which(high_mask)
        bad_ids  <- if (has_id) as.character(df$identifier[bad_rows]) else NA_character_
        
        eval_obs <- if (!is.null(eval_vector)) as.character(eval_vector[bad_rows]) else as.character(val_vector[bad_rows])

        cli_alert_warning(
          "Column {.var {t_col}} (rule from {.var {col}}): Flagged {length(bad_rows)} value(s) > {limits$max}."
        )

        violation_list[[length(violation_list) + 1]] <- tibble::tibble(
          identifier          = bad_ids,
          row_index           = bad_rows,
          target_column       = t_col,
          observed_value      = as.character(val_vector[bad_rows]),
          eval_column         = col,
          eval_value_observed = eval_obs,
          rule_broken         = paste0("> ", limits$max),
          violation_level     = "L1_BOUNDS"
        )
      }
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
  ) %>%
    filter(line_no %in% c(0, 10000))

  df <- recast_l1_data(df, cfg)
  
  bounds_result <- enforce_l1_bounds(df, cfg)
  df <- bounds_result$data
  violations_df <- bounds_result$violations

  dir_create(dirname(path_log_out))
  write_csv(violations_df, file = path_log_out)
  cli_alert_info("Quality log saved to {.file {path_file(path_log_out)}} ({nrow(violations_df)} entries).")

  dir_create(dirname(path_out))
  write_csv(df, file = path_out)
  cli_alert_success("Success! Prepared {file_name} for L2 evaluation at {.file {path_file(path_out)}}.")
}