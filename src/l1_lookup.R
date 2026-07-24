library(dplyr)


raw_extract_lookup_sales_report_from_xlsx_download <- function(
  path_in = '~/Downloads/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP SalesReport 2026-06-23",
  path_out = 'data/raw/lookup/raw_lookup_sales_report_2026-06-23.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_plants_from_xlsx_download <- function(
  path_in = '~/Downloads/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP PLANTS 2025-03-10",
  path_out = 'data/raw/lookup/raw_lookup_plants_2025-03-12.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_operators_from_xlsx_download <- function(
  path_in = '~/Downloads/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP PCEUtilityOperator2026 ",
  path_out = 'data/raw/lookup/raw_lookup_operators_2026-01-01.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

# run these ONCE to build raw lookup tables
# uncomment and run manually
# *** not part of the pipeline ***
# ***     not run by main.R    ***
# raw_extract_lookup_sales_report_from_xlsx_download()
# raw_extract_lookup_plants_from_xlsx_download()
# raw_extract_lookup_operators_from_xlsx_download()




l1_clean_lookup_sales_report <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_out = "data/l1_quality_checked/lookup/l1_lookup_sales_report.csv"
) {

  if (is.null(path_in)) {
    matching_files <- dir_ls(dir_raw, regexp = "raw_lookup_sales_report_.*\\.(csv|xlsx)$")

    if (length(matching_files) == 0) {
      stop(paste("No lookup files matching 'raw_lookup_sales_report_.*' found in", dir_raw))
    }

    path_in <- matching_files %>% sort() %>% last()
  }

  message(paste("Processing raw sales lookup file:", path_in))

  df <- read_csv(path_in, show_col_types = FALSE) %>%
    rename(
      pce_reporting_id = `PCE Reporting ID`,
      sales_reporter_id = `Sales Reporting ID`,
      sales_reporting_name = `Reporting Name`,
      intertie_id = `INTERTIE_Current Intertie ID`,
      intertie_name = `INTERTIE_Current Intertie name`
    ) %>%
    group_by(pce_reporting_id, sales_reporter_id) %>%
    arrange(sales_reporting_name) %>%
    slice(1) %>%
    select(
      pce_reporting_id,
      sales_reporter_id,
      sales_reporting_name,
      intertie_id,
      intertie_name
    ) %>%
    arrange(sales_reporting_name) %>%
    filter(
    pce_reporting_id != 332200 & sales_reporter_id != "SR-202" # drop wrong value for Manley Hot Springs
    )

  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean sales report lookup to:", path_out))
}




l1_clean_lookup_plants <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_out = "data/l1_quality_checked/lookup/l1_lookup_plants.csv"
) {

  if (is.null(path_in)) {
    matching_files <- dir_ls(dir_raw, regexp = "raw_lookup_plants_.*\\.(csv|xlsx)$")

    if (length(matching_files) == 0) {
      stop(paste("No lookup files matching 'raw_lookup_plantssales_report_.*' found in", dir_raw))
    }

    path_in <- matching_files %>% sort() %>% last()
  }

  message(paste("Processing plants lookup file:", path_in))

  df <- read_csv(path_in, show_col_types = FALSE) %>%
    rename(
      pce_reporting_id = `PCE reporting ID`,
      plant_id = `AK Plant ID`,
      plant_name = `plant_name`
    ) %>%
    group_by(pce_reporting_id, plant_id) %>% # some plants have multiple EIA reporting IDs, causing duplicate records, fixed by this
    arrange(plant_name) %>%
    slice(1) %>% # pull single plant name to prevent one-to-many in future joins
    select(
      pce_reporting_id,
      plant_id,
      plant_name
    ) %>%
    arrange(plant_name)


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean plant lookup to:", path_out))
}



l1_clean_lookup_operators <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_out = "data/l1_quality_checked/lookup/l1_lookup_operators.csv"
) {

  if (is.null(path_in)) {
    matching_files <- dir_ls(dir_raw, regexp = "raw_lookup_operators_.*\\.(csv|xlsx)$")

    if (length(matching_files) == 0) {
      stop(paste("No lookup files matching 'raw_lookup_operators_.*' found in", dir_raw))
    }

    path_in <- matching_files %>% sort() %>% last()
  }

  message(paste("Processing raw operators lookup file:", path_in))

  df <- read_csv(path_in, show_col_types = FALSE) %>%
    rename(
      project_code = `Project Code`,
      ak_operator_id = `AK_operator Id`,
      operator_name = 4
    ) %>%
    group_by(project_code) %>%
    arrange(operator_name) %>%
    slice(1) %>%
    select(
      project_code,
      ak_operator_id,
      operator_name
    ) %>%
    arrange(operator_name)


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean operators lookup to:", path_out))
}
