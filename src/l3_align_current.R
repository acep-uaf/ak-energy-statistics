library(readr)
library(dplyr)
library(fs)


# helper function to add columns if they don't exist
add_missing_cols <- function(data, ...) {
  cols <- list(...)
  missing <- setdiff(names(cols), names(data))
  
  for (col in missing) {
    data[[col]] <- cols[[col]]
  }
  
  data
}



l3_align_current_pce <- function(
  path_in, 
  path_to_lookup_sales_report, 
  path_to_lookup_pce_floor, 
  path_to_lookup_interties, 
  path_to_lookup_pce_utility_operators,
  path_to_lookup_operators,
  path_out) {

  tmp <- read_csv(path_in, show_col_types=F)

  lookup_sales_report <- read_csv(path_to_lookup_sales_report, show_col_types=F) %>%
    select(pce_id, cpcn_id, eia_id, communities_reported)

  lookup_pce_floor <- read_csv(path_to_lookup_pce_floor, show_col_types=F)

  lookup_interties <- read_csv(path_to_lookup_interties, show_col_types=F)

  lookup_pce_utility_operators <- read_csv(path_to_lookup_pce_utility_operators, show_col_types=F)

  lookup_operators <- read_csv(path_to_lookup_operators, show_col_types=F)



  joined_lookups <- tmp %>% 
    left_join(lookup_pce_floor, by = join_by(fiscal_year)) %>%
    left_join(lookup_sales_report, by = join_by(project_code == pce_id)) %>%
    left_join(lookup_interties, by = join_by(intertie_id)) %>%
    left_join(lookup_operators, by = join_by(ak_operator_id))

  df_out <- joined_lookups %>%
    add_missing_cols(
      notes = NA_character_
    ) %>%
    mutate(
      season = if_else(calendar_month >= 4 & calendar_month <= 9, 'summer', 'winter')
      ) %>%
    select(
      reporter_id = sales_reporter_id,
      pce_id = project_code,
      community_names = sales_reporting_name,
      pce_operator_acronym = pce_utility_code,
      operator_utility_name = operator_name,
      aea_operator_id = ak_operator_id,
      utility_utility_regulatory_status_name = rca_regulatory_status_name,
      utility_utility_certificate = operator_utility_certificate, 
      utility_utility_utility_type_name = operator_utility_type_name, 
      utility_utility_eia_operator_id = eia_id, 
      utility_utility_cpcn_number = cpcn_id,
      utility_utility_cpcn_status =  operator_cpcn_status, 
      pce_base_rate,
      pce_ceiling,
      year = calendar_year,
      month = calendar_month,
      season,
      intertie_id,
      intertie_name,
      # pce_community_intertied_to_another_pce_community, # boolean, not in current lookups, need to add from somewhere
      year_of_intertie,
      communities_intertied,
      residential_rate,
      pce_rate = actual_rate,
      pro_rata_rate,
      effective_rate = effective_residential_rate,
      pce_eligible_residential_kwh,
      # pce_eligible_commercial_kwh,
      # pce_eligible_community_kwh,    # problem in l3_pce.csv concerning this data, investigate
      pce_eligible_total_kwh,
      disbursement = amount,   
      fuel_price = most_recent_fuel_purch_price,
      fuel_used_gal = fuel_used_gallons,
      fuel_cost,
      nonfuel_expenses = non_fuel_expenses,
      diesel_efficiency,
      diesel_kwh_generated,
      hydro_kwh_generated,
      natural_gas_kwh_generated,
      wind_kwh_generated,
      solar_kwh_generated,
      purchased_from,
      kwh_purchased = total_kwh_purchased,
      powerhouse_consumption_kwh,
      peak_consumption_kw,
      residential_kwh_sold = residential_sold_to,
      commercial_kwh_sold = commercial_sold_to,
      community_kwh_sold = com_facil_sold_to,
      government_kwh_sold = govt_facil_sold_to,
      total_sales, 
      unbilled_kwh = unbilled_sold_to,
      residential_customers,
      commercial_customers,
      community_customers = com_facil_customers,
      government_customers = govt_facil_customers,
      unbilled_customers,
      other_customers,
      other_customers_description,
      notes,
      cleaned_during_energy_statistics,
      # imputed_residential_rate,
      # imputed_pce_rate,
      # imputed_pro_rata_rate,
      # imputed_effective_rate,
      # imputed_fuel_price,
      imputed_fuel_used_gal = imputed_fuel_used_gallons,
      # imputed_fuel_cost,
      # imputed_nonfuel_expenses,
      # imputed_diesel_efficiency,
      imputed_diesel_kwh_generated,
      imputed_hydro_kwh_generated,
      imputed_powerhouse_consumption_kwh,
      imputed_peak_consumption_kw = imputed_peak_consumption_kw, 
      imputed_residential_kwh_sold = imputed_residential_sold_to,
      imputed_commercial_kwh_sold = imputed_commercial_sold_to,
      imputed_community_kwh_sold = imputed_com_facil_sold_to,
      imputed_government_kwh_sold = imputed_govt_facil_sold_to,
      imputed_unbilled_kwh = imputed_unbilled_sold_to,
      imputed_residential_customers,
      imputed_commercial_customers,
      imputed_community_customers = imputed_com_facil_customers,
      imputed_government_customers = imputed_govt_facil_customers,
      imputed_unbilled_customers
      # imputed_other_customers,
      # other_2_kwh_generated_imputed,
      # purchased_from_imputed,
      # total_kwh_purchased_imputed,
      # pce_eligible_community_kwh_imputed
    )



  return(df_out)
}



target <- read_csv('data/l3/consolidated/l3_pce_historical_2001-2020.csv')

target_cols <- names(target) %>%
  tibble(column_name = .)

rbind(df_out, target)


current <- l3_align_current_pce(
  path_in = 'data/l3/consolidated/l3_pce.csv',
  path_to_lookup_sales_report = 'data/l1/lookup/l1_lookup_sales_report.csv',
  path_to_lookup_pce_floor = 'data/l1/lookup/l1_lookup_pce_floor.csv',
  path_to_lookup_interties = 'data/l1/lookup/l1_lookup_interties.csv',
  path_to_lookup_pce_utility_operators = 'data/l1/lookup/l1_lookup_pce_utility_operators.csv',
  path_to_lookup_operators = 'data/l1/lookup/l1_lookup_operators.csv',
  path_out = 'data/l3/consolidated/l3_pce_aligned.csv'
)






