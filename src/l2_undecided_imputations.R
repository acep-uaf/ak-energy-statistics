library(dplyr)
library(readr)
library(fs)

generate_undecided_imputations <- function(
  pce_inputation_options_path,
  pce_inputation_decisions_path,
  path_out) {
  
  pce_inputation_options <- read_csv(pce_inputation_options_path)
  pce_inputation_decisions <- read_csv(pce_inputation_decisions_path)

  df_out <- anti_join(pce_inputation_options, pce_inputation_decisions, by = "id")

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  message(paste("Undecided imputations saved to:", path_out))
}


