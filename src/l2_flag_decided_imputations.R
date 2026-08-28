library(dplyr)
library(readr)
library(fs)


l2_flag_decided_imputations <- function(
  pce_inputation_options_path,
  pce_inputation_decisions_path,
  path_out) {
  
  pce_inputation_options <- read_csv(pce_inputation_options_path, show_col_types = FALSE)
  pce_inputation_decisions <- read_csv(pce_inputation_decisions_path, show_col_types = FALSE)

  decisions_subset <- pce_inputation_decisions %>%
    select(id, 
           new_manual_override = manual_override, 
           new_decision = decision, 
           new_comment = comment)

  df_out <- pce_inputation_options %>%
    left_join(decisions_subset, by = "id") %>%
    mutate(
      manual_override = coalesce(new_manual_override, manual_override),
      decision        = coalesce(new_decision, decision),
      comment         = coalesce(new_comment, comment),
      
      cleaned_during_energy_stats = !is.na(new_decision)
    ) %>%
    select(-starts_with("new_"))

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  message(paste("Flagged imputations saved to:", path_out))
}


