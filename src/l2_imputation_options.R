library(dplyr)
library(readr)
library(fs)
library(yaml)
library(purrr)
library(lubridate)
library(tidyr)

l2_generate_imputation_options <- function(
  pce_path,
  overrides_path,
  combined_outliers_and_quality_violations_log_path,
  path_out) {
  
  overrides <- read_yaml(overrides_path)
  carry_forward_columns <- unlist(overrides$carry_forward)

  pce <- read_csv(pce_path, show_col_types = FALSE)

  combined_outliers_and_quality_violations_log <- read_csv(
    combined_outliers_and_quality_violations_log_path, 
    show_col_types = FALSE
  ) %>%
    select(-any_of(c("median_val", "mad_score", "anomaly_severity")))

  col_name_in_log <- if ("target_column" %in% names(combined_outliers_and_quality_violations_log)) {
    "target_column"
  } else {
    "column"
  }

  target_cols <- combined_outliers_and_quality_violations_log[[col_name_in_log]] %>%
    unique() %>%
    na.omit()

  valid_cols <- intersect(target_cols, names(pce))

  # Pivot full dataset long
  pce_long <- pce %>%
    pivot_longer(
      cols = any_of(valid_cols),
      names_to = "column",
      values_to = "val",
      values_transform = list(val = as.numeric)
    )

  # -------------------------------------------------------------------------
  # 1. CREATE CLEAN DATASET FOR IMPUTATION CALCULATIONS
  # -------------------------------------------------------------------------
  
  # Build flagged lookup keys using project_code + date + column
  flagged_cells <- combined_outliers_and_quality_violations_log %>%
    select(project_code, date, column = all_of(col_name_in_log)) %>%
    distinct() %>%
    filter(!is.na(project_code) & !is.na(date) & !is.na(column))

  # Exclude flagged cells from clean calculation pool
  pce_long_clean <- pce_long %>%
    anti_join(flagged_cells, by = c("project_code", "date", "column"))

  # -------------------------------------------------------------------------
  # 2. CALCULATE CLEAN SUMMARY METRICS
  # -------------------------------------------------------------------------
  
  # Annual Averages (using only un-flagged clean data)
  same_year_avg_df <- pce_long_clean %>%
    mutate(year_num = year(date)) %>%
    group_by(project_code, column, year_num) %>%
    summarise(
      annual_average = round(mean(val, na.rm = TRUE), 0),
      .groups = "drop"
    )

  # Same Month Across Other Years
  same_month_avg_df <- pce_long_clean %>%
    mutate(
      month_num = month(date),
      year_num  = year(date)
    ) %>%
    group_by(project_code, column, month_num, year_num) %>%
    summarise(month_year_val = mean(val, na.rm = TRUE), .groups = "drop")

  # Clean lookup table for next_month_val
  clean_lookup <- pce_long_clean %>% 
    select(project_code, date, column, val)

  # -------------------------------------------------------------------------
  # 3. BUILD IMPUTATION OPTIONS FOR REVIEW LOG
  # -------------------------------------------------------------------------
  
  # Compute Last Observation Carried Forward (LOCF) across consecutive outlier gaps
  pce_long_filled <- pce_long %>%
    # Tag flagged outlier rows
    mutate(
      is_outlier = paste(project_code, date, column) %in% 
                   paste(flagged_cells$project_code, flagged_cells$date, flagged_cells$column),
      clean_val = if_else(is_outlier, NA_real_, val)
    ) %>%
    arrange(project_code, column, date) %>%
    group_by(project_code, column) %>%
    # Carry forward the last non-outlier value across gaps
    fill(clean_val, .direction = "down") %>%
    ungroup() %>%
    select(project_code, date, column, carry_forward = clean_val) %>%
    distinct(project_code, date, column, .keep_all = TRUE)

  df_out <- combined_outliers_and_quality_violations_log %>%
    mutate(
      prev_month = date %m-% months(1),
      next_month = date %m+% months(1),
      month_num  = month(date),
      year_num   = year(date)
    ) %>%

    # Join the robust carry_forward values directly by date
    left_join(
      pce_long_filled,
      by = c("project_code", "date", "column")
    ) %>%

    # Next Month Value
    left_join(
      clean_lookup,
      by = c("project_code", "next_month" = "date", "column")
    ) %>%
    rename(next_month_val = val) %>%

    # Annual Average
    left_join(
      same_year_avg_df,
      by = c("project_code", "column", "year_num")
    ) %>%

    # Average of Same Month from Other Years
    left_join(
      same_month_avg_df,
      by = c("project_code", "column", "month_num"),
      relationship = "many-to-many"
    ) %>%
    filter(year_num.x != year_num.y | is.na(year_num.y)) %>%
    
    # GROUP BY COMPOSITE PRIMARY KEY 'id'
    group_by(id, identifier, column, date) %>%
    mutate(
      avg_same_month_other_years = round(mean(month_year_val, na.rm = TRUE), 0)
    ) %>%
    ungroup() %>%
    distinct(id, .keep_all = TRUE) %>%

    # Average of Preceding & Succeeding Months
    mutate(
      avg_preceding_proceeding = round(rowMeans(across(c(carry_forward, next_month_val)), na.rm = TRUE), 0),
      avg_preceding_proceeding = ifelse(is.nan(avg_preceding_proceeding), NA_real_, avg_preceding_proceeding)
    ) %>%

    # Placeholders for expert review
    mutate(
      manual_override = NA_real_,
      decision = NA_character_,
      comment = NA_character_
    ) %>%

    # Final Formatting
    select(
      id,
      identifier,
      any_of(names(combined_outliers_and_quality_violations_log)),
      carry_forward,
      annual_average,
      avg_preceding_proceeding,
      avg_same_month_other_years,
      manual_override,
      decision,
      comment
    ) %>%
    
    mutate(
      decision = if_else(
        column %in% carry_forward_columns, 
        "carry_forward", 
        decision
      )
    )

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  message(paste("Outlier imputation options saved to:", path_out))
}