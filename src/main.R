source('src/l0_extract.R')
source('src/l0_consolidate.R')
source('src/l1_lookup.R')
source('src/l1_quality_check.R')
source('src/l1_transform.R')
source('src/l2_outlier_check.R')
source('src/l2_imputation_options.R')
source('src/l2_combine_imputation_decisions.R')
source('src/l2_flag_decided_imputations.R')
source('src/l3_impute.R')


unlink('data/l0', recursive = T)
unlink('data/l1', recursive = T)
unlink('data/l2', recursive = T)
unlink('data/l3', recursive = T)
unlink('data/l4', recursive = T)


l0_extract_pce_dir(
  dir_in = 'data/raw',
  pattern = 'raw_pce'
)

l0_consolidate_pce_data(
  path_with_pattern = "data/l0/monthly/l0_pce_rate_line",
  join_by_columns = c("identifier", "line_no")
)

l0_consolidate_pce_data(
  path_with_pattern = "data/l0/monthly/l0_pce_header",
  join_by_columns = c("identifier", "line_no")
)



l1_clean_lookup_sales_report(
  dir_raw = "data/raw/lookup",
  path_out = "data/l1/lookup/l1_lookup_sales_report.csv"
)

l1_clean_lookup_plants(
  dir_raw = "data/raw/lookup",
  path_out = "data/l1/lookup/l1_lookup_plants.csv"
)

l1_clean_lookup_operators(
  dir_raw = "data/raw/lookup",
  path_out = "data/l1/lookup/l1_lookup_operators.csv"
)

l1_clean_lookup_pce_floor(
  dir_raw = "data/raw/lookup",
  path_out = "data/l1/lookup/l1_lookup_pce_floor.csv"
)



l1_check_quality_pce_dir(
  dir_in = 'data/l0/consolidated',
  pattern = 'l0_pce',
  config = 'config/check_data/l1_pce_quality_check.yml'
)



l1_transform_pce(
  l1_consolidated_dir = "data/l1/consolidated",
  l1_lookup_sales_report_path = "data/l1/lookup/l1_lookup_sales_report.csv",
  l1_lookup_plants_path = "data/l1/lookup/l1_lookup_plants.csv",
  l1_lookup_operators_path = "data/l1/lookup/l1_lookup_operators.csv",
  l1_lookup_pce_floor_path = "data/l1/lookup/l1_lookup_pce_floor.csv",
  config = "config/schema/l1_pce_schema.yml"
)



l2_check_outliers(
  path_in         = "data/l1/consolidated/l1_pce.csv",
  path_config     = "config/check_data/l2_pce_outlier_check.yml",
  output_log_path = "data/l2/logs/l2_pce_outliers_log.csv",
  path_out = "data/l2/consolidated/l2_pce.csv"
)

l2_generate_imputation_options(
  pce_path = 'data/l1/consolidated/l1_pce.csv',
  pce_outliers_log_path = 'data/l2/logs/l2_pce_outliers_log.csv',
  path_out = 'data/l2/logs/l2_pce_imputation_options.csv')

l2_combine_imputation_decisions(
  path_in_dir = 'data/raw/imputation_decisions/sessions',
  path_out = 'data/raw/imputation_decisions/pce_imputation_decisions_cumulative.csv'
)

l2_flag_decided_imputations(
  pce_inputation_options_path = 'data/l2/logs/l2_pce_imputation_options.csv',
  pce_inputation_decisions_path = 'data/raw/imputation_decisions/pce_imputation_decisions_cumulative.csv',
  path_out = 'data/l2/logs/l2_pce_imputations_flagged.csv'
)


start_time <- Sys.time()
l3_impute_columns(
  path_in = "data/l2/consolidated/l2_pce.csv",
  path_config = "config/imputations/l3_impute.yml",
  path_out = "data/l3/consolidated/l3_pce.csv"
)
end_time <- Sys.time()
print(end_time - start_time)
