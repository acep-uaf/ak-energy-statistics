library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(fs)
library(purrr)
library(cli)


l2_combine_imputation_decisions <- function(
  path_in_dir, 
  primary_key = 'id',
  path_out) {

  files <- dir_ls(
    path = path_in_dir,
    glob = "*.csv"
  )

  if (length(files) == 0) {
    stop("No imputation decisions found to combine")
  }

  cli_alert_info("Combining {length(files)} imputation decision files...")

  df_main <- files %>%
    map(\(f) read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE)) %>% 
    list_rbind()

  df_deduped <- df_main %>%
    group_by(across(all_of(primary_key))) %>%
    slice_tail(n = 1) %>%   # Keep latest record, overwrite older
    ungroup()


  dir_create(dirname(path_out))
  write_csv(df_deduped, path_out)

  cli_alert_success("Combined file updated at {.file {path_out}}. Total unique records: {nrow(df_deduped)}.")
}


