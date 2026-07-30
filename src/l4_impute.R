library(dplyr, warn.conflicts = FALSE)
library(readr)
library(purrr, warn.conflicts = FALSE)
library(imputeTS)
library(yaml)
library(fs)


config  <- read_yaml("config/imputations/l4_impute.yml")
l3_data <- read_csv("data/l3_outliers_checked/consolidated/l3_pce.csv", show_col_types = FALSE)






safe_impute_vector <- function(x) {
  n_valid <- sum(!is.na(x))

  # Case 0: All NAs - cannot impute
  if (n_valid == 0) {
    return(x)
  }

  # Case 1: Exactly 1 non-NA value - fill missing with LOCF / NOCB
  if (n_valid == 1) {
    return(imputeTS::na_locf(x, option = "locf", na_remaining = "rev"))
  }

  # Case 2: 2 non-NA values - linear interpolation is safe
  if (n_valid == 2) {
    return(imputeTS::na_interpolation(x, option = "linear"))
  }

  # Case 3: 3+ non-NA values - Kalman smoothing + interpolation fallback
  tryCatch(
    {
      imputed <- imputeTS::na_kalman(x, model = "auto.arima")
      imputeTS::na_interpolation(imputed, option = "linear")
    },
    error = function(e) {
      # Fallback if ARIMA fails to converge on edge cases
      imputeTS::na_interpolation(x, option = "linear")
    }
  )
}

impute_column <- function(df, col_name, group_var, year_var, month_var) {
  flag_name <- paste0(col_name, "_is_imputed")

  df %>%
    group_by(across(all_of(group_var))) %>%
    arrange(across(all_of(c(year_var, month_var))), .by_group = TRUE) %>%
    mutate(
      across(all_of(col_name), ~ is.na(.x), .names = flag_name),

      temp_imputed = safe_impute_vector(.data[[col_name]]),

      across(all_of(col_name), ~ if_else(is.na(.x), temp_imputed, .x))
    ) %>%
    select(-temp_imputed) %>%
    ungroup()
}


target_cols <- config$imputation$target_columns
group_var   <- config$imputation$group_by
year_var    <- config$imputation$year
month_var   <- config$imputation$month

flag_cols <- paste0(target_cols, "_is_imputed")

l4_imputed <- target_cols %>%
  reduce(
    .init = l3_data,
    .f = function(df, col_name) {
      impute_column(
        df        = df,
        col_name  = col_name,
        group_var = group_var,
        year_var  = year_var,
        month_var = month_var
      )
    }
  ) %>%
  mutate(
    any_imputed = if_any(all_of(flag_cols), ~ .x %in% TRUE)
  )


output_path <- "data/l4_imputed_panel.csv"
dir_create(dirname(output_path))
write_csv(l4_imputed, output_path)

message("Successfully imputed missing values across target columns: ", paste(target_cols, collapse = ", "))
message("Total rows with at least one imputed value: ", sum(l4_imputed$any_imputed))



tmp <- l4_imputed %>%
  filter(any_imputed == TRUE)

outliers <- read_csv('data/l3_outliers_checked/logs/l3_pce_outliers_log.csv')
