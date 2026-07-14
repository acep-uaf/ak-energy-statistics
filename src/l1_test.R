library(cli)
library(fs)
library(readr)
library(yaml)
library(stringr)

l1_data_tests <- function(path_in, config) {
  # Setup Terminal Logging
  options(cli.num_colors = 256)

  # Start Run
  cli_h1("Running Data Quality Tests: {.file {path_file(path_in)}}")
  cli_alert_info("Starting test run for {path_file(path_in)}")

  df <- read_csv(path_in, show_col_types = FALSE)

  cfg_whole <- read_yaml(config)
  config_key <- path_ext_remove(path_file(path_in)) %>%
    str_remove("_\\d{4}-\\d{2}(-\\d{2})?$")

  if (!config_key %in% names(cfg_whole)) {
    stop(paste("Target config key", config_key, "not found in", config ,"YAML."))
  }
  cfg <- cfg_whole[[config_key]]

  all_passed <- TRUE

  # Core evaluator engine
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
    for (col in cfg$type_checks[[type_name]]) {
      passed <- check_col(col, types[[type_name]](df[[col]]),
                          "{.var {col}} is a {type_name} column",
                          "{.var {col}} should be {type_name}, but is not")
      if (!passed) all_passed <- FALSE
    }
  }

  # Value and Statistical Checks
  # Not Null
  for (col in cfg$constraints$not_null) {
    passed <- check_col(col, !is.na(df[[col]]),
                        "{.var {col}} has no missing values",
                        "{.var {col}} contains missing (NULL) values")
    if (!passed) all_passed=FALSE
  }

  # Positive Only
  for (col in cfg$constraints$positive_only) {
    passed <- check_col(col, df[[col]] >= 0,
                        "{.var {col}} has only positive values",
                        "{.var {col}} contains negative values")
    if (!passed) all_passed <- FALSE
  }

  # Outliers
  for (col in cfg$constraints$detect_outliers) {
    if (col %in% names(df)) {
      mad_val <- max(mad(df[[col]], na.rm = TRUE), 0.001)
      is_clean <- abs(df[[col]] - median(df[[col]], na.rm = TRUE)) <= (3 * mad_val)

      passed <- check_col(col, is_clean,
                          "{.var {col}} has no severe statistical outliers (> 3 MAD)",
                          "{.var {col}} contains values outside 3 Median Absolute Deviations!")
      if (!passed) all_passed <- FALSE
    }
  }

  # Gatekeeper
  if (!all_passed) {
    cli_alert_danger(col_red("FATAL: Data testing failed for {path_file(path_in)}"))
    stop("Possible data issues, see above checklist for specifics.", call. = FALSE)
  }

  file_name <- path_file(path_in)
  new_file_name <- str_replace(file_name, "^l0", "l1")
  path_out <- path("data", "l1", new_file_name)

  dir_create(dirname(path_out))
  write_csv(df, file = path_out)

  cli_alert_success(col_green("Success! {path_file(path_in)} passed all data tests, writing to {path_file(path_out)}."))
}

# Loop through directory
l1_test_pce_dir <- function(dir_in = 'data/l0', pattern = 'l0_pce') {
  files <- dir_ls(path = dir_in, regexp = pattern)

  for (file in files) {
    l1_data_tests(
      path_in = file,
      config = 'config/data_tests/l1_pce_tests.yml'
    )
  }
}
