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
      microgrid_id = intertie_id,
      microgrid_name = intertie_name,
      pce_operator_acronym = 'PCE_operator_acronym',
      ak_operator_id = 'AEA Operator ID',
      total_sales = 'Total Sales',
      checked_for_outliers = cleaned_during_energy_statistics
    ) %>%
    select(-c(
      'purchased_power_type', 
      'purchased_power_price_per_kwh', 
      'sales_for_resale_kwh_sold',
      'imputed_sales_for_resale_kwh_sold',
      'pce_community_intertied_to_another_pce_community',
      'year_of_intertie'
    ))

  dir_create(dirname(path_out))
  write_csv(df_out, path_out)
  
  message(paste("Cleaned historical workbook PCE data written as CSV to:", path_out))
  
}






