library(cli)
library(fs)
library(readr)
library(yaml)
library(stringr)
library(purrr)

# -------------------------------------------------------------------------
# DATA PREP
# -------------------------------------------------------------------------

# Transform text columns, map string values
recast_l1_data <- function(df, cfg) {
  # Map values for categorical variables
  if ("category_mappings" %in% names(cfg)) {
    for (col in names(cfg$category_mappings)) {
      if (col %in% names(df)) {
        df[[col]] <- str_to_upper(str_trim(df[[col]]))
        mapping_vec <- unlist(cfg$category_mappings[[col]])
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

  # Recast Character
  for (col in cfg$type_checks$character) {
    if (col %in% names(df)) df[[col]] <- str_trim(as.character(df[[col]]))
  }

  return(df)
}

# Null values that fall outside of YAML-specified boundaries
enforce_l1_bounds <- function(df, cfg) {
  if (!"bounds" %in% names(cfg)) return(df)

  for (col in names(cfg$bounds)) {
    if (!col %in% names(df)) next

    limits <- cfg$bounds[[col]]
    val_vector <- df[[col]]

    # Check lower bound securely (handles both NULL and NA safely)
    if ("min" %in% names(limits) && !is.null(limits$min) && !is.na(limits$min)) {
      min_val <- as.numeric(limits$min)
      low_mask <- !is.na(val_vector) & val_vector < min_val
      if (any(low_mask, na.rm = TRUE)) {
        cli_alert_warning("Column {.var {col}}: Setting {sum(low_mask)} value(s) to NA (fell below min of {min_val})")
        val_vector[low_mask] <- NA
      }
    }

    # Check upper bound securely (handles both NULL and NA safely)
    if ("max" %in% names(limits) && !is.null(limits$max) && !is.na(limits$max)) {
      max_val <- as.numeric(limits$max)
      high_mask <- !is.na(val_vector) & val_vector > max_val
      if (any(high_mask, na.rm = TRUE)) {
        cli_alert_warning("Column {.var {col}}: Setting {sum(high_mask)} value(s) to NA (exceeded max of {max_val})")
        val_vector[high_mask] <- NA
      }
    }

    df[[col]] <- val_vector
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

  # Type Checks
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

l1_data_quality_checks <- function(path_in, config) {
  options(cli.num_colors = 256)
  cli_h1("Running Data Quality Checks: {.file {path_file(path_in)}}")

  # Ingest config & data
  cfg_whole <- read_yaml(config)
  config_key <- path_ext_remove(path_file(path_in)) %>% str_remove("_\\d{4}-\\d{2}(-\\d{2})?$")

  if (!config_key %in% names(cfg_whole)) {
    stop(paste("Target config key", config_key, "not found in", config ,"YAML."))
  }
  cfg <- cfg_whole[[config_key]]

  df <- read_csv(path_in, col_types = cols(.default = "c"), show_col_types = FALSE)

  # Process data (Recast -> Enforce Bounds)
  df <- recast_l1_data(df, cfg)
  df <- enforce_l1_bounds(df, cfg)

  # Validate data
  all_passed <- validate_l1_data(df, cfg)

  if (!all_passed) {
    cli_alert_danger("FATAL: Data quality checks failed for {path_file(path_in)}")
    stop("Possible data quality issues, see above checklist for specifics.", call. = FALSE)
  }

  # Egress
  file_name <- path_file(path_in)
  new_file_name <- str_replace(file_name, "^l0", "l1")
  path_out <- path("data", "l1_quality_checked", "monthly", new_file_name)

  dir_create(dirname(path_out))
  write_csv(df, file = path_out)

  cli_alert_success("Success! {path_file(path_in)} passed all data quality checks, writing to {path_file(path_out)}.")
}

# Loop through directory
l1_check_quality_pce_dir <- function(dir_in, pattern, config) {
  files <- dir_ls(path = dir_in, regexp = pattern)

  for (file in files) {
    l1_data_quality_checks(
      path_in = file,
      config = config
    )
  }
}
