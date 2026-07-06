library(readr)
library(readxl)
library(janitor)
library(stringr)
library(fs)


l0_extract_xlsx_write_csv <- function(path) {

  date_stamp <- str_split_i(basename(path), '_raw_data', 1)

  raw_header <- read_xlsx(path = path, sheet = 'HeaderData') %>% clean_names()
  raw_rates <- read_xlsx(path = path, sheet = 'RateLineData') %>% clean_names()

  raw_header$file_upload_date <- as.Date(date_stamp)
  raw_rates$file_upload_date  <- as.Date(date_stamp)

  dir_create('data/l0')

  write_csv(raw_header, paste0("data/l0/l0_header_", date_stamp, ".csv"))
  write_csv(raw_rates,  paste0("data/l0/l0_rates_", date_stamp, ".csv"))

}


l0_extract_xlsx_write_csv(path = 'data/raw/2026-06-12_raw_data.xlsx')
