library(readr)
library(dplyr)
library(purrr)
library(fs)

l3_impute_columns <- function(path_in, path_overrides, path_out) {

  overrides_raw <- read_csv(path_overrides, show_col_types = FALSE)

  overrides <- overrides_raw %>%
    mutate(
      decided_value = map2_chr(decision, seq_len(n()), function(col_name, row_idx) {
        if (is.na(col_name) || !col_name %in% names(overrides_raw)) {
          return(NA_character_)
        }
        as.character(overrides_raw[[col_name]][row_idx])
      })
    ) %>%
    select(
      identifier,
      column,
      decision,
      decided_value,
      cleaned_during_energy_statistics,
      comment
    )

  df <- read_csv(path_in, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    as.data.frame()

  flagged_ids <- overrides %>% 
    filter(cleaned_during_energy_statistics == TRUE) %>% 
    pull(identifier)

  df$cleaned_during_energy_statistics <- df$identifier %in% flagged_ids

  row_coords <- match(overrides$identifier, df$identifier)
  col_coords <- match(overrides$column, names(df))

  valid_id <- which(!is.na(row_coords) & !is.na(col_coords) & !is.na(overrides$decided_value))

  imputed_matrix <- matrix(FALSE, nrow = nrow(df), ncol = ncol(df), dimnames = list(NULL, names(df)))

  for (i in valid_id) {
    df[row_coords[i], col_coords[i]] <- overrides$decided_value[i]
    imputed_matrix[row_coords[i], col_coords[i]] <- TRUE
  }

  imputed_cols <- names(which(colSums(imputed_matrix) > 0))


  flags_df <- as_tibble(imputed_matrix[, imputed_cols, drop = FALSE]) %>%
    rename_with(~ paste0("imputed_", .x))

  df_out <- bind_cols(as_tibble(df), flags_df)

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  
  message(paste("Cleaned dataset with imputed values written to:", path_out))
}