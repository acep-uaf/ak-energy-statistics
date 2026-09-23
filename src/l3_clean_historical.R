library(readxl)
library(dplyr)
library(fs)


l3_clean_historical <- function(path_in, sheet, path_out) {


  xlsx_in <- read_xlsx(
    path = path_in, 
    sheet = sheet
    )

  df_out <- xlsx_in %>%
    rename(
      reporter_id = 'Sales Reporter ID',
      pce_id = 'PCE ID',
      pce_operator_acronym = 'PCE_operator_acronym',
      aea_operator_id = 'AEA Operator ID',
      total_sales = 'Total Sales'
    )

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  
  message(paste("Cleaned historical workbook PCE data written as CSV to:", path_out))
  
}






