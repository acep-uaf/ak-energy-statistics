library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)


l1_transform_header <- function(l1_consolidated_dir) {

  raw <- read_csv(
    path(l1_consolidated_dir, 'l1_pce_header.csv'),
    show_col_types = FALSE
  )

  cleaned <- raw %>%
    mutate(
      # Parse foreign keys & codes from 11-char identifier (e.g. A3321500226)
      project_code = str_sub(identifier, 2, 7),
      stage_code   = str_sub(identifier, 8, 9)   # Fiscal month (1..12)
    ) %>%
    mutate(
      # Parse numeric types explicitly
      project_code = as.integer(project_code),
      fiscal_month = as.numeric(stage_code),
      calendar_month = ((fiscal_month + 5) %% 12) + 1,
      date = make_date(calendar_year, calendar_month, 1)
    )


  # Isolate other_*_kwh_* columns in order to pivot
  pivot_other <- cleaned %>%
    select(identifier, starts_with("other_1"), starts_with("other_2")) %>%
    pivot_longer(
      cols = c(
        other_1_kwh_type, other_2_kwh_type,
        other_1_kwh_generated, other_2_kwh_generated
      ),
      names_to = c("source", ".value"),
      names_pattern = "other_(1|2)_(kwh_type|kwh_generated)"
    ) %>%
    filter(!is.na(kwh_type)) %>%
    select(-source) %>%
    pivot_wider(
      names_from = kwh_type,
      values_from = kwh_generated,
      names_glue = "{kwh_type}_kwh_generated"
    )


  # Join side table back to main table
  df_out <- cleaned %>%
    rename(hydro_kwh_generated_main = hydro_kwh_generated) %>%
    left_join(pivot_other, by = "identifier") %>%
    mutate(
      purchased_from_2 = NA_character_,
      total_kwh_purchased_2 = NA_real_,
      # Coalesce hydro columns if extra hydro exists in 'other'
      hydro_kwh_combined = if ("hydro_kwh_generated" %in% names(.)) {
        coalesce(as.numeric(hydro_kwh_generated), as.numeric(hydro_kwh_generated_main))
      } else {
        as.numeric(hydro_kwh_generated_main)
      }
    ) %>%
    # Clean up intermediate hydro & other columns
    select(
      -hydro_kwh_generated_main,
      -starts_with("other_1"),
      -starts_with("other_2")
    )

  if ("hydro_kwh_generated" %in% names(df_out)) {
    df_out <- select(df_out, -hydro_kwh_generated)
  }
  df_out <- rename(df_out, hydro_kwh_generated = hydro_kwh_combined)

  return(df_out)
}


l1_transform_rate_line <- function(l1_consolidated_dir) {

  raw <- read_csv(path(l1_consolidated_dir, 'l1_pce_rate_line.csv'), show_col_types = FALSE)

  pce_eligible_kwhs <- raw %>%
    group_by(identifier) %>%
    summarize(
      pce_eligible_residential_kwh = sum(pce_eligible_residential_kwh, na.rm = TRUE),
      pce_eligible_com_facil_kwh = sum(pce_eligible_com_facil_kwh, na.rm = TRUE),
      pce_eligible_community_kwh = sum(pce_eligible_community_kwh, na.rm = TRUE),
      pce_eligible_total_kwh = sum(pce_eligible_kwh_total, na.rm = TRUE)
    )

  rates <- raw %>%
    filter(line_no == 10000) %>%
    select(
      identifier,
      actual_rate,
      pro_rata_rate,
      check,
      residential_rate,
      effective_residential_rate
    )


  df_out <- pce_eligible_kwhs %>%
    left_join(rates, by = "identifier")


  return(df_out)

}


l1_transform_pce <- function(
  l1_consolidated_dir,
  l1_lookup_sales_report_path,
  l1_lookup_plants_path,
  l1_lookup_operators_path,
  l1_lookup_pce_floor_path,
  config = "config/schema/l1_pce_schema.yml") {

  header <- l1_transform_header(l1_consolidated_dir)
  rate_line <- l1_transform_rate_line(l1_consolidated_dir)

  lookup_sales_report <- read_csv(l1_lookup_sales_report_path, show_col_types = FALSE)
  lookup_plants <- read_csv(l1_lookup_plants_path, show_col_types = FALSE)
  lookup_operators <- read_csv(l1_lookup_operators_path, show_col_types = FALSE)
  lookup_pce_floor <- read_csv(l1_lookup_pce_floor_path, show_col_types = FALSE)

  joined <- header %>%
    left_join(rate_line, by = "identifier") %>%
    left_join(lookup_sales_report, by = join_by(project_code == pce_reporting_id)) %>%
    left_join(lookup_plants, by = join_by(project_code == pce_reporting_id)) %>%
    left_join(lookup_operators, by = join_by(project_code == project_code)) %>%
    left_join(lookup_pce_floor, by = join_by(fiscal_year))

  calculated <- joined %>%
    mutate(
      residential_kwh_per_customer_per_month = residential_sold_to / residential_customers,
      pce_residential_kwh_per_customer_per_month = pce_eligible_residential_kwh / residential_customers
    )

  # Schema from YAML config
  if (file_exists(config)) {
    col_config <- yaml::read_yaml(config)
    target_columns <- col_config$pce_columns

    # option A: strict (throws an error if a requested column is missing in data)
    # df <- calculated %>% select(all_of(target_columns))

    # Option B: resilient (keeps missing config columns from breaking the ETL)
    df <- calculated %>% select(any_of(target_columns))

    # Optional check to warn if new/unconfigured columns are being dropped silently
    omitted <- setdiff(names(calculated), target_columns)
    if (length(omitted) > 0) {
      cli::cli_alert_info("Omitted {length(omitted)} unlisted column(s): {paste(omitted, collapse = ', ')}")
    }
  } else {
    cli::cli_alert_warning("Config path {.path {config}} not found. Defaulting to calculated column order.")
    df <- calculated
  }

  path_out <- path_ext_set(path('data/l1/consolidated/l1_pce'), "csv")
  dir_create(dirname(path_out))
  write_csv(df, path_out)

}
