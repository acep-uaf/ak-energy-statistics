library(readxl)
library(dplyr)
library(fs)


clean_historical <- function(path_in, sheet, path_out) {


  xlsx_in <- read_xlsx(
    path = path_in, 
    sheet = sheet
    )

  df_out <- xlsx_in %>%
    rename(
      count = 'Count',
      reporter_id = 'Reporter ID',
      pce_id = 'PCE ID',
      pce_pperator_acronym = 'PCE_operator_acronym',
      aea_operator_id = 'AEA Operator ID'
    )

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  
  message(paste("Cleaned historical workbook PCE data written as CSV to:", path_out))
  
}





clean_historical(
  path_in = 'data/raw/historical_workbooks/sales_monthly_pce_eia_consolidated.xlsx',
  sheet = 'PCE 2001-20 ALL DATA',
  path_out = 'data/l3/consolidated/l3_pce_historical_2001-2020.csv'
)
