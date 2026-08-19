library(dplyr)
library(readr)
library(fs)
library(yaml)
library(lubridate)
library(tidyr)

l2_generate_imputation_options <- function(
  pce_path,
  pce_outliers_log_path,
  outlier_config_path = 'config/check_data/l2_pce_outlier_check.yml',
  path_out) {

  pce <- read_csv(pce_path, show_col_types = FALSE)

  pce_outliers_log <- read_csv(pce_outliers_log_path, show_col_types = FALSE) %>%
    select(-any_of(c("median_val", "mad_score", "anomaly_severity")))

  config <- read_yaml(outlier_config_path)
  target_cols <- config$columns_to_check

  valid_cols <- intersect(target_cols, names(pce))

  # pivot long for speed
  pce_long <- pce %>%
    pivot_longer(
      cols = any_of(valid_cols),
      names_to = "column",
      values_to = "val",
      values_transform = list(val = as.numeric)
    )

  # avg same calendar year
  same_year_avg_df <- pce_long %>%
    mutate(year_num = year(date)) %>%
    group_by(project_code, column, year_num) %>%
    summarise(
      annual_average = round(mean(val, na.rm = TRUE), 0),
      .groups = "drop"
    )

  # same month in other years (select ONLY key columns)
  same_month_avg_df <- pce_long %>%
    mutate(
      month_num = month(date),
      year_num  = year(date)
    ) %>%
    select(project_code, column, month_num, year_num, val)

  # build decision dataframe
  df_out <- pce_outliers_log %>%
    mutate(
      prev_month = date %m-% months(1),
      next_month = date %m+% months(1),
      month_num  = month(date),
      year_num   = year(date)
    ) %>%

    # last observation carry forward
    left_join(
      pce_long %>% select(project_code, date, column, val),
      by = c("project_code", "prev_month" = "date", "column")
    ) %>%
    rename(carry_forward = val) %>%

    # next month helper
    left_join(
      pce_long %>% select(project_code, date, column, val),
      by = c("project_code", "next_month" = "date", "column")
    ) %>%
    rename(next_month_val = val) %>%

    # annual average
    left_join(
      same_year_avg_df,
      by = c("project_code", "column", "year_num")
    ) %>%

    # average of same month from other years
    left_join(
      same_month_avg_df,
      by = c("project_code", "column", "month_num"),
      relationship = "many-to-many"
    ) %>%
    filter(year_num.x != year_num.y | is.na(year_num.y)) %>% # exclude outlier's own year
    group_by(project_code, date, column) %>%
    mutate(
      avg_same_month_other_years = round(mean(val, na.rm = TRUE), 0)
    ) %>%
    ungroup() %>%
    distinct(project_code, date, column, .keep_all = TRUE) %>%

    # average of preceding and proceeding months
    mutate(
      avg_preceding_proceeding = round(rowMeans(across(c(carry_forward, next_month_val)), na.rm = TRUE), 0),
      avg_preceding_proceeding = ifelse(is.nan(avg_preceding_proceeding), NA_real_, avg_preceding_proceeding)
    ) %>%
    
    # create empty columns for manual overrides
    # create composite_id
    mutate(
      id = paste0(identifier, "_", column),
      manual_override = NA,
      decision = NA,
      comment = NA
    ) %>%

    # organize
    select(
      id,
      identifier,
      any_of(names(pce_outliers_log)),
      carry_forward,
      annual_average,
      avg_preceding_proceeding,
      avg_same_month_other_years,
      manual_override,
      decision,
      comment
    )

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  message(paste("Outlier imputation options saved to:", path_out))
}




# Options:
          # "Value out of bounds, but original value kept",
          # "Carry forward",
          # "Annual average",
          # "Average of preceding and proceeding months",
          # "Average of months from other years",
          # "Manual override: expert opinion"
