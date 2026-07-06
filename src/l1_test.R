library(cli)
library(logger)
library(fs)
library(readr)
library(yaml)


l1_data_tests <- function(path_in, config, path_out) {

  # Setup and helper functions
  options(cli.num_colors = 256)
  log_layout(layout_glue_colors)

  df <- read_csv(path_in, show_col_types = FALSE)
  config <- read_yaml(config)

  cli_h1("Running Data Quality Tests: {.file {basename(path_in)}}")
  all_passed <- TRUE

  run_check <- function(condition, success_msg, fail_msg) {
    if (all(condition, na.rm = TRUE)) {
      cli_alert_success(success_msg)
      return(TRUE)
    } else {
      cli_alert_danger(fail_msg)
      return(FALSE)
    }
  }

  # ─── TYPE & EXISTENCE CHECKS ───────────────────────────────────────────
  # Character check
  for (col in config$type_checks$character) {
    if (!col %in% names(df)) {
      cli_alert_danger("Column {.var {col}} is missing.")
      all_passed <- FALSE
      next
    }
    passed <- run_check(is.character(df[[col]]),
                        "{.var {col}} is a text column",
                        "{.var {col}} should be text, but is not")
    if (!passed) all_passed <- FALSE
  }

  # Numeric check
  for (col in config$type_checks$numeric) {
    if (!col %in% names(df)) {
      cli_alert_danger("Column {.var {col}} is missing.")
      all_passed <- FALSE
      next
    }
    passed <- run_check(is.numeric(df[[col]]),
                        "{.var {col}} is a numeric column",
                        "{.var {col}} should be numeric, but is not")
    if (!passed) all_passed <- FALSE
  }

  # Logical check
  for (col in config$type_checks$logical) {
    if (!col %in% names(df)) {
      cli_alert_danger("Column {.var {col}} is missing.")
      all_passed <- FALSE
      next
    }
    passed <- run_check(is.logical(df[[col]]),
                        "{.var {col}} is a logical column",
                        "{.var {col}} should be logical, but is not")
    if (!passed) all_passed <- FALSE
  }

  # ─── CONSTRAINT CHECKS ─────────────────────────────────────────────────
  # Not Null check
  for (col in config$constraints$not_null) {
    if (col %in% names(df)) {
      passed <- run_check(!is.na(df[[col]]),
                          "{.var {col}} has no missing values",
                          "{.var {col}} contains missing (NULL) values")
      if (!passed) all_passed <- FALSE
    }
  }

  # Only positive values
  for (col in config$constraints$positive_only) {
    if (col %in% names(df)) {
      passed <- run_check(df[[col]] >= 0,
                          "{.var {col}} has only positive values",
                          "{.var {col}} contains negative values")
      if (!passed) all_passed <- FALSE
    }
  }

  # Outlier detection
  for (col in config$constraints$detect_outliers) {
    if (col %in% names(df)) {

      col_median <- median(df[[col]], na.rm = TRUE)
      col_mad    <- mad(df[[col]], na.rm = TRUE)

      # Protect against columns where MAD is 0 (e.g., a column of mostly identical numbers)
      if (col_mad == 0) col_mad <- 0.001

      # Calculate how many MADs each row is away from the median
      # A threshold of 3 is standard, but you can change it to 5 for "extreme" anomalies
      is_within_mad <- abs(df[[col]] - col_median) <= (3 * col_mad)

      passed <- run_check(is_within_mad,
                          "{.var {col}} has no severe statistical outliers (> 3 MAD)",
                          "{.var {col}} contains values outside 3 Median Absolute Deviations!")

      if (!passed) all_passed = FALSE
    }
  }

  # ─── PIPELINE GATEKEEPER ───────────────────────────────────────────────
  if (!all_passed) {
    log_fatal("Data testing failed.")
    stop("Possible data issues, see above output for specifics.", call. = FALSE)
  }

  dir_create(dirname(path_out))
  write.csv(df, path_out, row.names = FALSE)

  cli_alert_success(
    col_green("Success! {basename(path_in)} passed all data tests, writing to file as {basename(path_out)}.")
  )
}


l1_data_tests(
  path_in = 'data/l0/l0_rates_2026-06-12.csv',
  config = 'config/data_tests/l0_rates_tests.yaml',
  path_out = 'data/l1/l1_rates_2026-06-12.csv'
)


l1_data_tests(
  path_in = 'data/l0/l0_header_2026-06-12.csv',
  config = 'config/data_tests/l0_header_tests.yaml',
  path_out = 'data/l1/l1_header_2026_06-12.csv'
)
