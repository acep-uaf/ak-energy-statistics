library(dplyr, warn.conflicts = FALSE)
library(readr)
library(purrr, warn.conflicts = FALSE)
library(imputeTS)
library(yaml)
library(fs)


l3_impute_columns <- function(path_in, path_config, path_out) {

  config <- read_yaml(path_config)
  df_in <- read_csv(path_in, show_col_types = FALSE)


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
    flag_name <- paste0("imputed_", col_name)

    df %>%
      group_by(across(all_of(group_var))) %>%
      arrange(across(all_of(c(year_var, month_var))), .by_group = TRUE) %>%
      mutate(
        # 1. Compute safe vector imputation across the group
        temp_imputed = safe_impute_vector(.data[[col_name]]),

        # 2. Flag as TRUE ONLY if it started as NA AND became non-NA
        across(
          all_of(col_name),
          ~ is.na(.x) & !is.na(temp_imputed),
          .names = flag_name
        ),

        # 3. Fill missing values in target column
        across(all_of(col_name), ~ if_else(is.na(.x), temp_imputed, .x))
      ) %>%
      select(-temp_imputed) %>%
      ungroup()
  }



  target_cols <- config$imputation$target_columns
  group_var   <- config$imputation$group_by
  year_var    <- config$imputation$year
  month_var   <- config$imputation$month

  flag_cols <- paste0("imputed_", target_cols)

  df_out <- target_cols %>%
    reduce(
      .init = df_in,
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
      cleaned_during_energy_statistics = if_any(all_of(flag_cols), ~ .x %in% TRUE)
    )


  dir_create(dirname(path_out))
  write_csv(df_out, path_out)

  message("Successfully imputed missing values across target columns: ", paste(target_cols, collapse = ", "))
  message("Total rows with at least one imputed value: ", sum(df_out$cleaned_during_energy_statistics))

}




# Notes:
# don't just target all NULL values
# use outlier log (and bounds log?) as input for imputation targets (7000 records)


# going to need multiple strategies depending on column (figure out via SME)
