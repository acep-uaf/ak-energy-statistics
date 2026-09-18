library(readr)
library(dplyr)
library(fs)


l2_combine_outliers_and_quality_violations <- function(
  l1_pce_header_quality_log_path,
  l1_pce_rate_line_quality_log_path,
  l2_pce_outliers_log_path,
  l1_pce_path,
  path_out
) {
  header_quality_log <- read_csv(l1_pce_header_quality_log_path, show_col_types = F)
  rate_line_quality_log <- read_csv(l1_pce_rate_line_quality_log_path, show_col_types = F)

  l2_pce_outliers_log <- read_csv(l2_pce_outliers_log_path, show_col_types = F) %>%
    select(-c(median_val, mad_score, anomaly_severity))

  l1_pce <- read_csv(l1_pce_path, show_col_types = F)

  combined_quality_logs <- rbind(header_quality_log, rate_line_quality_log) %>%
    left_join(l1_pce, by = join_by(identifier) ) %>%
    select(
      identifier,
      project_code,
      sales_reporting_name,
      date,
      calendar_year,
      calendar_month,
      column = target_column,
      raw_value = target_value_scrubbed
    ) %>%
    filter(
      raw_value != 0
    ) %>%
    arrange(sales_reporting_name)
  
  df_out <- rbind(l2_pce_outliers_log, combined_quality_logs)

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  message(paste("Combined outlier and quality violations saved to", path_out))

}





