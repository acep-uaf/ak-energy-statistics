library(dplyr)
library(fs)
library(readxl)
library(yaml)


raw_extract_lookup_sales_report_from_xlsx_download <- function(
  path_in = 'data/raw/historical_workbooks/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP SalesReport 2026-06-23",
  path_out = 'data/raw/lookup/raw_lookup_sales_report_2026-06-23.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_plants_from_xlsx_download <- function(
  path_in = 'data/raw/historical_workbooks/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP PLANTS 2025-03-10",
  path_out = 'data/raw/lookup/raw_lookup_plants_2025-03-12.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_operators_from_xlsx_download <- function(
  path_in = 'data/raw/historical_workbooks/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP OPERATOR 2025-03-07",
  path_out = 'data/raw/lookup/raw_lookup_operators_2025-03-07.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_pce_floor_from_xlsx_download <- function(
  path_in = 'data/raw/historical_workbooks/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP PCE floor 2026-06-18",
  path_out = 'data/raw/lookup/raw_lookup_pce_floor_2026-06-18.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_interties_from_xlsx_download <- function(
  path_in = 'data/raw/historical_workbooks/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP INTERTIES 2026-05-04",
  path_out = 'data/raw/lookup/raw_lookup_interties_2026-05-04.csv') {

  dir_create(dirname(path_out))
  read_xlsx(path_in, sheet = sheet) %>%
    write_csv(path_out)

}

raw_extract_lookup_pce_utility_operators_from_xlsx_download <- function(
  path_in = 'data/raw/historical_workbooks/PCE UAF Data Request_Cleaning_Process.xlsx',
  sheet = "LOOKUP PCEUtilityOperator2026 ",
  path_out = 'data/raw/lookup/raw_lookup_pce_utility_operators_2026.csv') {

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
# raw_extract_lookup_pce_floor_from_xlsx_download()
# raw_extract_lookup_interties_from_xlsx_download()
# raw_extract_lookup_pce_utility_operators_from_xlsx_download()




l1_clean_lookup_sales_report <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_config = "config/overrides/l1_lookup.yml",
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
      pce_id = `PCE Reporting ID`,
      sales_reporter_id = `Sales Reporting ID`,
      sales_reporting_name = `Reporting Name`,
      intertie_id = `INTERTIE_Current Intertie ID`,
      intertie_name = `INTERTIE_Current Intertie name`,
      cpcn_id = `OPERATOR_RCA CPCN`,
      eia_id = `OPERATOR_EIA operator Number`,
      communities_reported = `Communities reported`
    ) %>%
    group_by(pce_id, sales_reporter_id) %>%
    arrange(sales_reporting_name) %>%
    slice(1) %>%
    select(
      pce_id,
      sales_reporter_id,
      sales_reporting_name,
      intertie_id,
      intertie_name,
      cpcn_id,
      eia_id,
      communities_reported
    ) %>%
    arrange(sales_reporting_name)

  if (file_exists(path_config)) {
    cfg <- read_yaml(path_config)
    exclusions_list <- cfg$l1_lookup_sales_report$exclude_records

    if (!is.null(exclusions_list) && length(exclusions_list) > 0) {
      exclude_df <- bind_rows(exclusions_list)

      df <- anti_join(df, exclude_df, by = names(exclude_df))
    }
  }


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean sales report lookup to:", path_out))
}




l1_clean_lookup_plants <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_config = "config/overrides/l1_lookup.yml",
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


  if (file_exists(path_config)) {
    cfg <- read_yaml(path_config)
    exclusions_list <- cfg$l1_lookup_plants$exclude_records

    if (!is.null(exclusions_list) && length(exclusions_list) > 0) {
      exclude_df <- bind_rows(exclusions_list)

      df <- anti_join(df, exclude_df, by = names(exclude_df))
    }
  }


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean plant lookup to:", path_out))
}



l1_clean_lookup_pce_utility_operators <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_config = "config/overrides/l1_lookup.yml",
  path_out = "data/l1_quality_checked/lookup/l1_lookup_pce_utility_operators.csv"
) {

  if (is.null(path_in)) {
    matching_files <- dir_ls(dir_raw, regexp = "raw_lookup_pce_utility_operators_.*\\.(csv|xlsx)$")

    if (length(matching_files) == 0) {
      stop(paste("No lookup files matching 'raw_lookup_pce_utility_operators_.*' found in", dir_raw))
    }

    path_in <- matching_files %>% sort() %>% last()
  }

  message(paste("Processing raw pce utility operators lookup file:", path_in))

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

  if (file_exists(path_config)) {
    cfg <- read_yaml(path_config)
    exclusions_list <- cfg$l1_lookup_pce_utility_operators$exclude_records

    if (!is.null(exclusions_list) && length(exclusions_list) > 0) {
      exclude_df <- bind_rows(exclusions_list)

      df <- anti_join(df, exclude_df, by = names(exclude_df))
    }
  }


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean pce utility operators lookup to:", path_out))
}



l1_clean_lookup_pce_floor <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_config = "config/overrides/l1_lookup.yml",
  path_out = "data/l1_quality_checked/lookup/l1_lookup_pce_floor.csv"
) {

  if (is.null(path_in)) {
    matching_files <- dir_ls(dir_raw, regexp = "raw_lookup_pce_floor_.*\\.(csv|xlsx)$")

    if (length(matching_files) == 0) {
      stop(paste("No lookup files matching 'raw_lookup_pce_floor_.*' found in", dir_raw))
    }

    path_in <- matching_files %>% sort() %>% last()
  }

  message(paste("Processing raw PCE floor lookup file:", path_in))

  df <- read_csv(path_in, show_col_types = FALSE) %>%
    rename(
      fiscal_year = `Fiscal Year`,
      pce_base_rate = `PCE base rate`,
      pce_ceiling = `PCE ceiling`,
      percent_funding = `% of funding [pro rata stuff]`,
      residential_maximum_monthly = `Residential Maximum Monthly`
    ) %>%
    select(
      fiscal_year,
      pce_base_rate,
      pce_ceiling,
      percent_funding,
      residential_maximum_monthly
    ) %>%
    arrange(fiscal_year)

  if (file_exists(path_config)) {
    cfg <- read_yaml(path_config)
    exclusions_list <- cfg$l1_lookup_pce_floor$exclude_records

    if (!is.null(exclusions_list) && length(exclusions_list) > 0) {
      exclude_df <- bind_rows(exclusions_list)

      df <- anti_join(df, exclude_df, by = names(exclude_df))
    }
  }


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean PCE floor lookup to:", path_out))
}



l1_clean_lookup_interties <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_config = "config/overrides/l1_lookup.yml",
  path_out = "data/l1_quality_checked/lookup/l1_lookup_interties.csv"
) {

  if (is.null(path_in)) {
    matching_files <- dir_ls(dir_raw, regexp = "raw_lookup_interties_.*\\.(csv|xlsx)$")

    if (length(matching_files) == 0) {
      stop(paste("No lookup files matching 'raw_lookup_interties_.*' found in", dir_raw))
    }

    path_in <- matching_files %>% sort() %>% last()
  }

  message(paste("Processing raw interties lookup file:", path_in))

  df <- read_csv(path_in, show_col_types = FALSE) %>%
    rename(
      current_id = `Current ID`,
      communities_intertied = `Communities Intertied`,
      month_of_intertie = `Month of interite`,
      year_of_intertie = `Year of intertie`
    ) %>%
    select(
      intertie_id,
      current_id,
      communities_intertied,
      month_of_intertie,
      year_of_intertie
    ) %>%
    arrange(intertie_id)

  if (file_exists(path_config)) {
    cfg <- read_yaml(path_config)
  exclusions_list <- cfg$l1_lookup_interties$exclude_records

    if (!is.null(exclusions_list) && length(exclusions_list) > 0) {
      exclude_df <- bind_rows(exclusions_list)

      df <- anti_join(df, exclude_df, by = names(exclude_df))
    }
  }


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean interties lookup to:", path_out))
}







l1_clean_lookup_operators <- function(
  path_in = NULL,
  dir_raw = "data/raw/lookup",
  path_config = "config/overrides/l1_lookup.yml",
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
      ak_operator_id = `AK_operator Id`,
      pce_utility_code = `PCE_utility_code`,
      operator_utility_type_name = `operator__utility_type_name`, 
      rca_regulatory_status_name = `operator_rca_regulatory_status_name`,
      power_generation_end_use = `Power Generation End Use`
    ) %>%
    group_by(ak_operator_id) %>%
    arrange(pce_utility_code) %>%
    slice(1) %>%
    select(
      ak_operator_id,
      pce_utility_code,
      operator_utility_type_name,
      rca_regulatory_status_name,
      operator_cpcn_status,
      pce_eligible,
      power_generation_end_use
    ) %>%
    arrange(ak_operator_id)

  if (file_exists(path_config)) {
    cfg <- read_yaml(path_config)
    exclusions_list <- cfg$l1_lookup_operators$exclude_records

    if (!is.null(exclusions_list) && length(exclusions_list) > 0) {
      exclude_df <- bind_rows(exclusions_list)

      df <- anti_join(df, exclude_df, by = names(exclude_df))
    }
  }


  dir_create(dirname(path_out))
  write_csv(df, path_out)

  message(paste("Successfully written clean operators lookup to:", path_out))
}
