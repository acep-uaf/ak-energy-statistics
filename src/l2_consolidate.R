library(dplyr)
library(lubridate)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)

# header_raw <- read_csv('data/l1/l1_pce_header_2026-06.csv')

# rate_line_raw <- read_csv('data/l1/l1_pce_rate_line_2026-06.csv')


# header <- header_raw %>%
#   # split identifier
#   mutate(
#     # id1 = str_sub(identifier, 1, 3), .before = identifier, # All records are 'A33', not sure significance, drop?
#     community_id = str_sub(identifier, 4, 7), # Foreign key, community_id
#     # fiscal_month_year = str_sub(identifier, 8, 11), # Fiscal month and year combo identifier, captured elsewhere, could drop
#     # id4 = str_sub(identifier, 12, 17) # All records are '225003', not sure significance, drop?
#   .before = community) %>%

#   mutate(
#     month = as.integer(month(parse_date_time(umr_month, 'B'))),
#     fiscal_year = as.integer(fiscal_year),
#     calendar_year = as.integer(calendar_year),
#     community_id = as.integer(community_id),
#     other_customers_description = as.character(other_customers_description)
#   ) %>%

#   arrange(calendar_year, month)



# tmp2 <- header %>%
#   filter(community_id == 1720) %>%
#   select(
#     community_id,
#     umr_month,
#     fiscal_year,
#     calendar_year,
#     month
#   )





l2_consolidate_pce_data <- function(path_with_pattern, id_col) {

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
    map_dfr(read_csv, show_col_types = FALSE, .id = "file_path") %>%
    mutate(
      # Extract the YYYY-MM string from filename
      file_date = str_extract(basename(file_path), "\\d{4}-\\d{2}")
    )

  df_deduped <- df_main %>%
    arrange(file_date) %>%
    group_by(.data[[id_col]]) %>%
    slice_tail(n = 1) %>%   # Keep latest record, overwrite older
    ungroup() %>%
    select(-file_path, -file_date)


  path_out <- path_ext_set(str_replace(path_with_pattern, "l1", "l2"),"csv")
  dir_create(dirname(path_out))
  write_csv(df_deduped, path_out)

  cli_alert_success("Main file updated at {.file {path_out}}. Total unique records: {nrow(df_deduped)}.")
}
