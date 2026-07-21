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
    pce_id = str_sub(identifier, 2, 7), # Foreign key, pce_id
    # fiscal_month_year = str_sub(identifier, 8, 11), # Fiscal month and year combo identifier, captured elsewhere, could drop
    # id4 = str_sub(identifier, 12, 17) # All records are '225003', not sure significance, drop?
  .before = community) %>%

  mutate(
    umr_month_numeric = as.integer(month(parse_date_time(umr_month, 'B'))),
    fiscal_year = as.integer(fiscal_year),
    calendar_year = as.integer(calendar_year),
    pce_id = as.integer(pce_id),
    other_customers_description = as.character(other_customers_description)
  ) %>%
  relocate(umr_month_numeric, .after = "umr_month") %>%
  arrange(calendar_year, umr_month_numeric)



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
    ) %>%
    relocate(
      any_of(c(
        "diesel_kwh_generated",
        "hydro_kwh_generated",
        "solar_kwh_generated",
        "wind_kwh_generated",
        "natural_gas_kwh_generated",
        "other_kwh_generated"
      )),
      .after = other_customers_description
    ) %>%
    relocate(
      purchased_from,
      total_kwh_purchased,
      .after = other_customers_description
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


l2_transform_pce <- function(l1_consolidated_dir) {
  header <- l2_transform_header(l1_consolidated_dir)
  rate_line <- l2_transform_rate_line(l1_consolidated_dir)

  joined <- header %>%
    left_join(rate_line, by = "identifier")

  calculated <- joined %>%
    mutate(
      residential_kwh_per_customer_per_month = residential_sold_to/residential_customers,
      pce_residential_kwh_per_customer_per_month = pce_eligible_residential_kwh/residential_customers
    )

  organized <- calculated %>%
    relocate(
      residential_kwh_per_customer_per_month, .after = "residential_sold_to"
    ) %>%
    relocate(
      pce_residential_kwh_per_customer_per_month, .after = "residential_kwh_per_customer_per_month"
    )

  df <- organized

  path_out <- path_ext_set(path('data/l2_transformed/consolidated/l2_pce'),"csv")
  dir_create(dirname(path_out))
  write_csv(df, path_out)
}
