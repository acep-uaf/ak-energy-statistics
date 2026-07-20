library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)


l1_consolidate_pce_data <- function(path_with_pattern, join_by_columns) {

  files <- dir_ls(
    path = path_dir(path_with_pattern),
    regexp = path_file(path_with_pattern)
  )

  if (length(files) == 0) {
    stop("No L1 files found to consolidate.")
  }

  cli_alert_info("Consolidating {length(files)} monthly data files...")

  df_main <- files %>%
    set_names() %>%
    map(read_csv, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    list_rbind(names_to = "file_path") %>%
    mutate(
      file_date = str_extract(path_file(file_path), "\\d{4}-\\d{2}")
    ) %>%
    suppressMessages(type_convert(guess_integer = TRUE))


  df_deduped <- df_main %>%
    arrange(file_date) %>%
    group_by(across(all_of(join_by_columns))) %>%
    slice_tail(n = 1) %>%   # Keep latest record, overwrite older
    ungroup() %>%
    select(-file_path, -file_date)


  path_out <- path_ext_set(str_c(str_replace_all(path_with_pattern, "monthly", "consolidated"), "_consolidated"),"csv")
  dir_create(dirname(path_out))
  write_csv(df_deduped, path_out)

  cli_alert_success("Consolidated file updated at {.file {path_out}}. Total unique records: {nrow(df_deduped)}.")
}
