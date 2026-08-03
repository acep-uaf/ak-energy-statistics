library(janitor)
library(readr)
library(tidyverse)

tmp <- read_csv('data/l1_quality_checked/lookup/l1_lookup_sales_report.csv') %>%
  filter(!is.na(pce_reporting_id))


tmp2 <- tmp %>%
  group_by(pce_reporting_id) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(pce_reporting_id)


l1_rate_line <- read_csv('data/l1_quality_checked/consolidated/l1_pce_rate_line_consolidated.csv')

tmp <- read_csv('data/l3_outliers_checked/consolidated/l3_pce.csv') %>%
  filter(sales_reporting_name == "Chilkat Valley")


outliers <- read_csv('data/l3_outliers_checked/logs/l3_pce_outliers_log.csv')


l3_pce <- read_csv('data/l3_outliers_checked/consolidated/l3_pce.csv')



line_loss <- l3_pce %>%
  mutate(
    total_gen = sum(diesel_kwh_generated, hydro_kwh_generated, natural_gas_kwh_generated, wind_kwh_generated, solar_kwh_generated, other_kwh_generated),
    total_sales = sum(residential_sold_to, commercial_sold_to, com_facil_sold_to, govt_facil_sold_to, unbilled_sold_to),
    total_purchased = sum(total_kwh_purchased, total_kwh_purchased_2)
  )

l4_imputed <- read_csv('data/l4_nulls_imputed/consolidated/l4_pce.csv')





l2_pce <- read_csv('data/l2_transformed/consolidated/l2_pce.csv')

l3_pce <- read_csv('data/l3_outliers_checked/consolidated/l3_pce.csv')


check_df <- read_csv('data/l3_outliers_checked/consolidated/l3_pce.csv') %>%
  select(identifier, diesel_efficiency, fuel_used_gallons, diesel_kwh_generated) %>%
  mutate(
    gal_per_kwh = fuel_used_gallons / diesel_kwh_generated,
    kwh_per_gal = diesel_kwh_generated / fuel_used_gallons,

    diff_gal_per_kwh = diesel_efficiency - gal_per_kwh
  ) %>%
  filter(is.na(fuel_used_gallons))







tmp <- check_df %>%
  filter(!is.na(diesel_efficiency) & !is.na(gal_per_kwh)) %>%
  filter(abs(diff_gal_per_kwh) > 1e-6)




header <- l2_transform_header("data/l1_quality_checked/consolidated")
rate_line <- l2_transform_rate_line("data/l1_quality_checked/consolidated")

# 1. Check original row count before joining lookups
nrow(header)

l1_lookup_sales_report_path = 'data/l1_quality_checked/lookup/l1_lookup_sales_report.csv'
l1_lookup_plants_path = 'data/l1_quality_checked/lookup/l1_lookup_plants.csv'
l1_lookup_operators_path = 'data/l1_quality_checked/lookup/l1_lookup_operators.csv'

# 2. Check row count after joining
joined <- header %>%
  left_join(rate_line, by = "identifier") %>%
  left_join(read_csv(l1_lookup_sales_report_path), by = join_by(project_code == pce_reporting_id)) %>%
  left_join(read_csv(l1_lookup_plants_path), by = join_by(project_code == pce_reporting_id)) %>%
  left_join(read_csv(l1_lookup_operators_path), by = join_by(project_code == project_code))

nrow(joined)
