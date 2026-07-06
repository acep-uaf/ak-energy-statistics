library(cli)
library(logger)
library(readr)
library(yaml)


test_l0_data <- function(path_in, config) {

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

  # ─── PIPELINE GATEKEEPER ───────────────────────────────────────────────
  if (!all_passed) {
    log_fatal("Data testing failed.")
    stop("Possible data issues, see above output for specifics.", call. = FALSE)
  }

  cli_alert_success(
    col_green("Success! {basename(path_in)} passed all data tests.")
  )
}


# test_l0_data(
#   path_in = 'data/l0/l0_rates_2026-06-12.csv',
#   config = 'config/data_tests/l0_rates_tests.yaml'
# )
