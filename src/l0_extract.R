library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(fs)
library(yaml)


# general function to read XLSX, rename columns according to YAML config file, return df
read_xlsx_rename_cols <- function(path_in, sheet_name, schema_config) {

  raw_data <- read_xlsx(path = path_in, sheet = sheet_name)

  config   <- read_yaml(schema_config)
  sheet_mapping <- config[[sheet_name]]
  if (is.null(sheet_mapping)) {
    stop(paste("Sheet '", sheet_name, "' not found in YAML config."))
  }
  mapping <- unlist(sheet_mapping)

  missing_cols <- setdiff(names(mapping), names(raw_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing expected headers:", paste(missing_cols, collapse = ", ")))
  }

  target_contract <- setNames(names(mapping), mapping)

  clean_df <- raw_data %>%
    select(all_of(target_contract))

  return(clean_df)
}


# l0 PCE specific function to extract, rename cols, write to file as CSV
l0_pce_extract_rename_write <- function(
  path_in,
  sheets = c('HeaderData', 'RateLineData'),
  schema_config = 'config/extract/l0_pce_schema.yml',
  dir_out = 'data/l0_extracted/monthly'
) {

  for (sheet in sheets) {
    df <- read_xlsx_rename_cols(
    path_in = path_in,
    sheet_name = sheet,
    schema_config = schema_config
  )

  xlsx_date <- str_extract(path_in, "\\d{4}-\\d{2}")
  file_out <- str_glue('l0_pce', str_split_i(str_to_snake(sheet), '_data', 1), xlsx_date, .sep="_")


  dir_create(dir_out)
  write_csv(df, path(dir_out, file_out, ext = "csv"))
  }

}


# loop function over directory, matching PCE XLSX files
l0_extract_pce_dir <- function(dir_in = 'data/raw', pattern = 'raw_pce') {

  files <- list.files(path = dir_in, pattern = pattern, full.names=T)

  for (file in files) {

    l0_pce_extract_rename_write(path_in = file)
  }

}
