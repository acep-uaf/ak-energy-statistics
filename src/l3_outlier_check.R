library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(readr)
library(yaml)
library(fs)

l3_check_outliers <- function(path_in, path_config, output_log_path, path_out) {

  config_data <- read_yaml(path_config)
  l2 <- read_csv(path_in, show_col_types = FALSE)

  # Extract variables from config
  columns_to_check <- unlist(config_data$columns_to_check)
  mad_threshold   <- as.numeric(config_data$settings$mad_threshold)

  # Filter down to columns that actually exist in the data
  columns_to_check <- intersect(columns_to_check, names(l2))

  message(paste("Loaded config. Checking", length(columns_to_check), "columns with a MAD threshold of", mad_threshold))

  # Add temporary row index to preserve strict row ordering and uniqueness
  l2_indexed <- l2 %>%
    mutate(.row_id = row_number(), .before = 1)

  # Pivot based on YAML list of columns & calculate outliers
  outliers <- l2_indexed %>%
    select(.row_id, project_code, calendar_year, stage_code, all_of(columns_to_check)) %>%
    pivot_longer(
      cols = all_of(columns_to_check),
      names_to = "column_tested",
      values_to = "raw_value"
    ) %>%
    filter(!is.na(raw_value)) %>%
    group_by(project_code, stage_code, column_tested) %>%
    mutate(
      points_in_group = n(),
      median_val = median(raw_value, na.rm = TRUE),
      mad_val    = mad(raw_value, na.rm = TRUE),

      adjusted_mad = case_when(
        mad_val > 0 ~ mad_val,
        median_val > 0 ~ max(median_val * 0.10, 10),
        TRUE ~ NA_real_
      ),

      mad_score = abs(raw_value - median_val) / adjusted_mad,
      absolute_diff = abs(raw_value - median_val),

      anomaly_severity = case_when(
        points_in_group < 3 ~ "Insufficient Data",

        median_val == 0 ~ case_when(
          raw_value == 0 ~ "Normal",
          raw_value < 1000 ~ "Normal",
          raw_value >= 100000 ~ "Extreme",
          raw_value >= 10000  ~ "Strong",
          .default = "Mild"
        ),

        absolute_diff < 50 ~ "Normal",
        mad_score < mad_threshold ~ "Normal",
        mad_score < (mad_threshold * 1.5) ~ "Mild",
        mad_score < (mad_threshold * 3.0) ~ "Strong",
        TRUE ~ "Extreme"
      )
    ) %>%
    ungroup() %>%
    mutate(mad_score = round(mad_score, 0)) %>%
    filter(anomaly_severity %in% c("Strong", "Extreme")) %>%
    select(.row_id, project_code, calendar_year, stage_code, column_tested, raw_value, median_val, mad_score, anomaly_severity) %>%
    arrange(desc(mad_score))

  # Write Outlier Log to File
  dir_create(dirname(output_log_path))

  log_export <- outliers %>% select(-.row_id)
  write_csv(log_export, output_log_path)
  message(paste("Successfully recorded", nrow(log_export), "outliers to log."))

  # --- Scrub Outliers directly in Wide Format ---
  if (nrow(outliers) > 0) {
    # Pivot outliers to wide format to easily mask matching cells
    outliers_to_null <- outliers %>%
      select(.row_id, column_tested) %>%
      mutate(flag_null = TRUE) %>%
      pivot_wider(names_from = column_tested, values_from = flag_null)

    # Left join and NULL out flagged cells
    l3_clean <- l2_indexed %>%
      left_join(outliers_to_null, by = ".row_id", suffix = c("", "_is_outlier"))

    # Set matching cells to NA
    for (col in columns_to_check) {
      flag_col <- paste0(col, "_is_outlier")
      if (flag_col %in% names(l3_clean)) {
        l3_clean <- l3_clean %>%
          mutate(!!col := if_else(!is.na(.data[[flag_col]]), NA_real_, .data[[col]]))
      }
    }

    # Clean up index and temporary join columns
    l3_clean <- l3_clean %>%
      select(-.row_id, -ends_with("_is_outlier"))
  } else {
    l3_clean <- l2
  }

  # --- Recalculate Derived Metrics Post-Scrubbing ---
  # Ensures scrubbed NAs in fuel or kWh automatically wipe out the derived efficiency
  if (all(c("fuel_used_gallons", "diesel_kwh_generated") %in% names(l3_clean))) {
    l3_clean <- l3_clean %>%
      mutate(
        fuel_num = as.numeric(fuel_used_gallons),
        kwh_num  = as.numeric(diesel_kwh_generated),

        diesel_efficiency = if_else(
          is.na(fuel_num) | is.na(kwh_num) | kwh_num == 0,
          NA_real_,
          fuel_num / kwh_num
        )
      ) %>%
      select(-fuel_num, -kwh_num)
  }

  # Write Cleaned Dataset to File
  dir_create(dirname(path_out))
  write_csv(l3_clean, path_out)
  message(paste("Cleaned dataset saved with", nrow(outliers), "outliers scrubbed to NA at:", path_out))
}
