options(cli.num_colors = 256)

library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)

# -------------------------------------------------------------------------
# DATA PREP & CLEANING ENGINE
# -------------------------------------------------------------------------

# Transform text columns, map string values, and scrub missingness patterns
recast_l1_data <- function(df, cfg) {

  # Recast & Clean Character Columns
  for (col in cfg$type_checks$character) {
    if (col %in% names(df)) {
      # Normalize text: UPPERCASE and strip internal/external whitespace padding
      vals <- str_to_upper(str_squish(as.character(df[[col]])))

      # Define global character junk/placeholder patterns
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

  # Map Categorical Overrides (Strict mapping for YAML category lists)
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


  # Calculate effective residential rate if rate columns exist
  if (all(c("actual_rate", "residential_rate") %in% names(df))) {
    df <- df %>%
      mutate(
        effective_residential_rate = residential_rate - actual_rate
      )
  }

  # Calculate diesel efficiency if fuel and kwh columns exist
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

# Null values that fall outside of YAML-specified boundaries
enforce_l1_bounds <- function(df, cfg) {
  if (!"bounds" %in% names(cfg)) return(df)

  cli_h2("Enforcing Range Boundaries")

  violation_list <- list()
  has_id <- "identifier" %in% names(df)

  for (col in names(cfg$bounds)) {
    limits <- cfg$bounds[[col]]

    # Normalize target_cols into a character vector (handles single strings, lists, or NULL)
    target_cols <- if ("target_columns" %in% names(limits)) {
      as.character(unlist(limits$target_columns))
    } else if ("target_column" %in% names(limits)) {
      as.character(unlist(limits$target_column))
    } else {
      col
    }

    # Ensure evaluated column exists
    if (!col %in% names(df)) {
      cli_alert_warning(
        "Configured bounds column {.var {col}} was not found in dataset. Skipping rule."
      )
      next
    }

    val_vector <- df[[col]]
    col_has_violations <- FALSE

    # Build logical masks for violations
    low_mask  <- rep(FALSE, length(val_vector))
    high_mask <- rep(FALSE, length(val_vector))

    # Check lower bound
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

    # Check upper bound
    if ("max" %in% names(limits) && !is.null(limits$max) && !is.na(limits$max)) {
      max_val <- as.numeric(limits$max)
      high_mask <- !is.na(val_vector) & val_vector > max_val
    }

    # Iterate through all target columns and apply the masks
    for (t_col in target_cols) {
      if (!t_col %in% names(df)) {
        cli_alert_warning(
          "Target scrub column {.var {t_col}} (configured for {.var {col}}) was not found in dataset. Skipping."
        )
        next
      }

      target_vector <- df[[t_col]]

      # Process low mask violations for t_col
      if (any(low_mask, na.rm = TRUE)) {
        col_has_violations <- TRUE
        bad_rows <- which(low_mask)
        bad_ids  <- if (has_id) as.character(df$identifier[low_mask]) else NA_character_

        cli_alert_warning(
          "Column {.var {t_col}} (via {.var {col}}): Found {sum(low_mask)} value(s) below min of {limits$min}."
        )

        violation_list[[length(violation_list) + 1]] <- tibble::tibble(
          identifier            = bad_ids,
          row_index             = bad_rows,
          target_column         = t_col,
          target_value_scrubbed = as.character(target_vector[low_mask]),
          eval_column           = col,
          eval_value_observed   = as.character(val_vector[low_mask]),
          rule_broken           = rule_desc
        )

        target_vector[low_mask] <- NA
      }

      # Process high mask violations for t_col
      if (any(high_mask, na.rm = TRUE)) {
        col_has_violations <- TRUE
        bad_rows <- which(high_mask)
        bad_ids  <- if (has_id) as.character(df$identifier[high_mask]) else NA_character_

        cli_alert_warning(
          "Column {.var {t_col}} (via {.var {col}}): Found {sum(high_mask)} value(s) exceeding max of {limits$max}."
        )

        violation_list[[length(violation_list) + 1]] <- tibble::tibble(
          identifier            = bad_ids,
          row_index             = bad_rows,
          target_column         = t_col,
          target_value_scrubbed = as.character(target_vector[high_mask]),
          eval_column           = col,
          eval_value_observed   = as.character(val_vector[high_mask]),
          rule_broken           = paste0("> ", limits$max)
        )

        target_vector[high_mask] <- NA
      }

      # Assign cleaned vector back to df
      df[[t_col]] <- target_vector
    }

    if (!col_has_violations && ("min" %in% names(limits) || "max" %in% names(limits))) {
      cli_alert_success("Column(s) {.var {target_cols}}: All values within bounds.")
    }
  }

  if (length(violation_list) > 0) {
    .pce_violations <<- purrr::list_rbind(violation_list)
  } else {
    .pce_violations <<- NULL
  }

  return(df)
}

# -------------------------------------------------------------------------
# VALIDATION ENGINE
# -------------------------------------------------------------------------

# Run assertions against the cleaned data
validate_l1_data <- function(df, cfg) {
  all_passed <- TRUE

  check_col <- function(col, condition, pass_msg, fail_msg) {
    if (!col %in% names(df)) {
      cli_alert_danger("Column '{col}' is missing.")
      return(FALSE)
    }
    if (all(condition, na.rm = TRUE)) {
      cli_alert_success(format_inline(pass_msg))
      return(TRUE)
    } else {
      cli_alert_danger(format_inline(fail_msg))
      return(FALSE)
    }
  }

  # --- Type Assertions ---
  cli_h2("Verifying Column Type Integrity")
  types <- list(character = is.character, numeric = is.numeric, logical = is.logical)
  for (type_name in names(types)) {
    if (is.null(cfg$type_checks[[type_name]])) next
    for (col in cfg$type_checks[[type_name]]) {
      passed <- check_col(col, types[[type_name]](df[[col]]),
                          "{.var {col}} is a {type_name} column",
                          "{.var {col}} should be {type_name}, but is not")
      if (!passed) all_passed <- FALSE
    }
  }

  # --- Constraint Assertions ---
  if (!is.null(cfg$constraints$not_null) || !is.null(cfg$constraints$positive_only)) {
    cli_h2("Running Constraint Checks")
  }

  # Constraints: Not Null
  if (!is.null(cfg$constraints$not_null)) {
    for (col in cfg$constraints$not_null) {
      passed <- check_col(col, !is.na(df[[col]]),
                          "{.var {col}} has no missing values",
                          "{.var {col}} contains missing (NULL) values")
      if (!passed) all_passed <- FALSE
    }
  }

  # Constraints: Positive Only
  if (!is.null(cfg$constraints$positive_only)) {
    for (col in cfg$constraints$positive_only) {
      passed <- check_col(col, df[[col]] >= 0,
                          "{.var {col}} has only positive values",
                          "{.var {col}} contains negative values")
      if (!passed) all_passed <- FALSE
    }
  }

  return(all_passed)
}

# -------------------------------------------------------------------------
# COORDINATOR & RUNNER
# -------------------------------------------------------------------------

l1_check_quality <- function(path_in, config) {
  file_name <- path_file(path_in)
  cli_h1("Running Data Quality Checks: {.file {file_name}}")

  # Ingest config & data
  cfg_whole <- read_yaml(config)
  config_key <- path_ext_remove(file_name) %>% str_remove("_\\d{4}-\\d{2}(-\\d{2})?$")

  if (!config_key %in% names(cfg_whole)) {
    stop(paste("Target config key", config_key, "not found in", config, "YAML."))
  }
  cfg <- cfg_whole[[config_key]]

  # Ingest every column strictly as character text, silencing all internal guessing logs
  df <- read_csv(
    path_in,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  # Process data (Recast -> Enforce Bounds)
  df <- recast_l1_data(df, cfg)
  df <- enforce_l1_bounds(df, cfg)

  # Validate data types and structural database constraints
  all_passed <- validate_l1_data(df, cfg)

  if (!all_passed) {
    cli_alert_danger("FATAL: Data quality checks failed for {file_name}")
    stop("Possible data quality issues, see above checklist for specifics.", call. = FALSE)
  }

  # Egress Setup
  path_out <- path_in %>%
    str_replace_all("l0", "l1")

  dir_create(dirname(path_out))
  write_csv(df, file = path_out)

  # Write out log file if violations were flagged
  if (exists(".pce_violations", envir = .GlobalEnv) && !is.null(get(".pce_violations", envir = .GlobalEnv))) {
    violations_df <- get(".pce_violations", envir = .GlobalEnv) %>%
      mutate(file = file_name, .before = 1)


    log_file_name <- path_ext_set(str_c(path_ext_remove(file_name), "_quality_log"), "csv")
    path_log_out <- path(path_dir(path_dir(path_out)), "logs", log_file_name)

    dir_create(dirname(path_log_out))
    write_csv(violations_df, file = path_log_out)

    cli_alert_info("Quality log saved to {.file {path_file(path_log_out)}} ({nrow(violations_df)} entries).")

    # Securely remove temporary state variable from workspace environment
    rm(.pce_violations, envir = .GlobalEnv)
  }

  cli_alert_success("Success! {file_name} passed all data quality checks, writing to {path_file(path_out)}.")
}

# Loop through directory
l1_check_quality_pce_dir <- function(dir_in, pattern, config) {
  files <- dir_ls(path = dir_in, regexp = pattern)

  for (file in files) {
    l1_check_quality(
      path_in = file,
      config = config
    )
  }
}
