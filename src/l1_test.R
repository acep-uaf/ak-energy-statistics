library(cli)
library(fs)
library(readr)
library(yaml)

l1_data_tests <- function(path_in, config) {
  # ─── 1. SETUP LOG FILE ──────────────────────────────────────────────────
  options(cli.num_colors = 256)

  log_dir <- "logs"
  dir_create(log_dir)
  log_file_path <- file.path(log_dir, paste0("pipeline_", Sys.Date(), ".log"))

  # Helper function to append timestamped text to our permanent log archive
  write_log <- function(text, status = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    clean_text <- cli::ansi_strip(text) # Ensure zero raw ANSI color code leaks
    log_line <- paste0("[", timestamp, "] [", status, "] ", clean_text)
    write_lines(log_line, log_file_path, append = TRUE)
  }

  # ─── 2. START RUN ───────────────────────────────────────────────────────
  cli_h1("Running Data Quality Tests: {.file {basename(path_in)}}")
  write_log(paste0("--- Starting test run for ", basename(path_in), " ---"))

  df <- read_csv(path_in, show_col_types = FALSE)

  cfg_whole <- read_yaml(config)
  config_key <- path_ext_remove(basename(path_in)) %>%
    str_remove("_\\d{4}-\\d{2}(-\\d{2})?$")
  if (!config_key %in% names(cfg_whole)) {
    stop(paste("Target config key", config_key, "not found in", config ,"YAML."))
  }
  cfg <- cfg_whole[[config_key]]


  all_passed <- TRUE

  # Core evaluator engine
  check_col <- function(col, condition, pass_msg, fail_msg) {
    if (!col %in% names(df)) {
      err <- paste0("Column '", col, "' is missing.")
      cli_alert_danger(err)
      write_log(err, "ERROR")
      return(FALSE)
    }

    if (all(condition, na.rm = TRUE)) {
      # Use cli to safely evaluate the text variables first
      formatted_msg <- cli::format_inline(pass_msg)

      cli_alert_success(formatted_msg)   # Prints beautiful color on screen
      write_log(formatted_msg, "SUCCESS") # Appends pure plain text to file
      return(TRUE)
    } else {
      formatted_msg <- cli::format_inline(fail_msg)

      cli_alert_danger(formatted_msg)
      write_log(formatted_msg, "WARN")
      return(FALSE)
    }
  }

  # ─── TYPE CHECKS ────────────────────────────────────────────────────────
  types <- list(character = is.character, numeric = is.numeric, logical = is.logical)

  for (type_name in names(types)) {
    for (col in cfg$type_checks[[type_name]]) {
      passed <- check_col(col, types[[type_name]](df[[col]]),
                          "{.var {col}} is a {type_name} column",
                          "{.var {col}} should be {type_name}, but is not")
      if (!passed) all_passed <- FALSE
    }
  }

  # ─── VALUE & STATISTICAL CHECKS ────────────────────────────────────────
  # Not Null
  for (col in cfg$constraints$not_null) {
    passed <- check_col(col, !is.na(df[[col]]),
                        "{.var {col}} has no missing values",
                        "{.var {col}} contains missing (NULL) values")
    if (!passed) all_passed <- FALSE
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

  # ─── GATEKEEPER ────────────────────────────────────────────────────────
  if (!all_passed) {
    fatal_msg <- paste0("FATAL: Data testing failed for ", basename(path_in))
    cli_alert_danger(col_red(fatal_msg))
    write_log(fatal_msg, "FATAL")
    stop("Possible data issues, see above checklist for specifics.", call. = FALSE)
  }

  path_out = str_replace_all(path_in, 'l0', 'l1')
  dir_create(dirname(path_out))
  write.csv(df, path_out, row.names = FALSE)

  success_msg <- paste0("Success! ", basename(path_in), " passed all data tests, writing to file as ", basename(path_out), ".")
  cli_alert_success(col_green(success_msg))
  write_log(success_msg, "INFO")
}




l1_test_pce_dir <- function(
  dir_in = 'data/l0',
  pattern = 'l0_pce'
) {

  files <- list.files(path = dir_in, pattern = pattern, full.names=T)

  for (file in files) {

    l1_data_tests(
      path_in = file,
      config = 'config/data_tests/l1_pce_tests.yml'
    )
  }

}
