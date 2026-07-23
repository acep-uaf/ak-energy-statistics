library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(lubridate, warn.conflicts = FALSE)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)



l2_transform_header <- function(l1_consolidated_dir) {

  raw <- read_csv(path(l1_consolidated_dir, 'l1_pce_header_consolidated.csv'), show_col_types = FALSE)


  cleaned <- raw %>%
  # split identifier
  mutate(
    # id1 = str_sub(identifier, 1, 3), .before = identifier, # All records are 'A33', not sure significance, drop?
    project_code = str_sub(identifier, 2, 7), # Foreign key, project_code
    stage_code = str_sub(identifier, 8, 9), # Fiscal month
    shortcut_dimension_4_code = str_sub(identifier, 10, 11), # Fiscal year
  .before = community) %>%

  mutate(
    fiscal_year = as.integer(fiscal_year),
    calendar_year = as.integer(calendar_year),
    project_code = as.integer(project_code),
    other_customers_description = as.character(other_customers_description)
  ) %>%
  arrange(calendar_year, stage_code)



  # Isolate other_*_kwh_* columns in order to pivot
  pivot_other <- cleaned %>%
    select(identifier, line_no, starts_with("other_1"), starts_with("other_2")) %>%
    # Collapse to long in order to combine other_1 and other_2
    pivot_longer(
      cols = c(
        other_1_kwh_type, other_2_kwh_type,
        other_1_kwh_generated, other_2_kwh_generated
      ),
      names_to = c("source", ".value"),
      names_pattern = "other_(1|2)_(kwh_type|kwh_generated)"
    ) %>%
    # Drop NA values (most data) in order to produce side table. Will rejoin later
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
    left_join(pivot_other, by = c("identifier", "line_no")) %>%
    # Coalesce multiple hydro columns
    # 10 years of data had one record with other_*_kwh_type = 'hydro', but combine for posterity
    mutate(
      purchased_from_2 = NA,
      total_kwh_purchased_2 = NA,
      hydro_kwh_combined = coalesce(hydro_kwh_generated, hydro_kwh_generated_main)
    ) %>%

    # Clean up and reorder
    select(
      -hydro_kwh_generated,
      -hydro_kwh_generated_main,
      -starts_with("other_1"),
      -starts_with("other_2")
    ) %>%
    rename(
      hydro_kwh_generated = hydro_kwh_combined
    )

  return(df_out)

}


l2_transform_rate_line <- function(l1_consolidated_dir) {

  raw <- read_csv(path(l1_consolidated_dir, 'l1_pce_rate_line_consolidated.csv'), show_col_types = FALSE)

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


l2_transform_pce <- function(l1_consolidated_dir, config = "config/schema/l2_pce_schema.yml") {
  header <- l2_transform_header(l1_consolidated_dir)
  rate_line <- l2_transform_rate_line(l1_consolidated_dir)

  joined <- header %>%
    left_join(rate_line, by = "identifier")

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

  path_out <- path_ext_set(path('data/l2_transformed/consolidated/l2_pce'), "csv")
  dir_create(dirname(path_out))
  write_csv(df, path_out)

}
