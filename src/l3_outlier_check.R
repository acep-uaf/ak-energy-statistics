library(dplyr)
library(tidyr)
library(readr)
library(yaml)
library(fs)

l3_generate_outlier_log <- function(path_in, path_config, output_log_path) {

  config_data <- read_yaml(path_config)
  l2 <- read_csv(path_in)

  # Extract variables from config
  columns_to_check <- unlist(config_data$columns_to_check)
  mad_threshold   <- as.numeric(config_data$settings$mad_threshold)

  message(paste("Loaded config. Checking", length(columns_to_check), "columns with a MAD threshold of", mad_threshold))

  # Pivot based on YAML list of columns
  outliers <- l2 %>%
    select(pce_id, calendar_year, umr_month_numeric, all_of(columns_to_check)) %>%
    pivot_longer(
      cols = all_of(columns_to_check),
      names_to = "column_tested",
      values_to = "raw_value"
    ) %>%

    # Drop NAs
    filter(!is.na(raw_value)) %>%

    # Run statistical checks
    group_by(pce_id, umr_month_numeric, column_tested) %>%
    mutate(
      points_in_group = n(),
      median_val = median(raw_value, na.rm = TRUE),
      mad_val    = mad(raw_value, na.rm = TRUE),

      # Calculate MAD-based score ONLY if the median is positive
      adjusted_mad = case_when(
        mad_val > 0 ~ mad_val,
        median_val > 0 ~ max(median_val * 0.10, 10),
        TRUE ~ NA_real_ # Handle zeros separately
      ),

      mad_score = abs(raw_value - median_val) / adjusted_mad,
      absolute_diff = abs(raw_value - median_val),

      # Set the classification using conditional rules
      anomaly_severity = case_when(
        points_in_group < 3 ~ "Insufficient Data",

        # LANE B: If the median is 0 (Zero-Inflated Data)
        median_val == 0 ~ case_when(
          raw_value == 0 ~ "Normal",
          raw_value < 1000 ~ "Normal", # Skip small starting values
          raw_value >= 100000 ~ "Extreme",
          raw_value >= 10000  ~ "Strong",
          .default = "Mild" # Clean fallback inside Lane B
        ),

        # LANE A: Normal active data (Median > 0)
        absolute_diff < 50 ~ "Normal",
        mad_score < mad_threshold ~ "Normal",
        mad_score < (mad_threshold * 1.5) ~ "Mild",
        mad_score < (mad_threshold * 3.0) ~ "Strong",
        TRUE ~ "Extreme"
      )
    ) %>%
    ungroup() %>%
    mutate(mad_score = round(mad_score, 0)) %>%

    # Log only dramatic outliers
    filter(anomaly_severity %in% c("Strong", "Extreme")) %>%

    # Organize
    select(pce_id, calendar_year, umr_month_numeric, column_tested, raw_value, median_val, mad_score, anomaly_severity) %>%
    arrange(desc(mad_score))

  # Write list to file
  dir_create(dirname(output_log_path))
  write_csv(outliers, output_log_path)
  message(paste("Successfully recorded", nrow(outliers), "outliers"))
}
