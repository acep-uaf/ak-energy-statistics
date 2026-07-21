source('src/l0_extract.R')
source('src/l1_quality_check.R')
source('src/l1_consolidate.R')
source('src/l2_transform.R')
source('src/l3_outlier_check.R')

unlink('data/l0_extracted', recursive = T)
unlink('data/l1_quality_checked', recursive = T)
unlink('data/l2_transformed', recursive = T)
unlink('data/l3_outliers_checked', recursive = T)

l0_extract_pce_dir(
  dir_in = 'data/raw',
  pattern = 'raw_pce'
)

l1_check_quality_pce_dir(
  dir_in = 'data/l0_extracted/monthly',
  pattern = 'l0_pce',
  config = 'config/check_data/l1_pce_quality_check.yml'
)


l1_consolidate_pce_data(
  path_with_pattern = "data/l1_quality_checked/monthly/l1_pce_rate_line",
  join_by_columns = c("identifier", "line_no")
)

l1_consolidate_pce_data(
  path_with_pattern = "data/l1_quality_checked/monthly/l1_pce_header",
  join_by_columns = c("identifier", "line_no")
)

l2_transform_pce(l1_consolidated_dir = 'data/l1_quality_checked/consolidated')

l3_generate_outlier_log(
  path_in         = "data/l2_transformed/consolidated/l2_pce.csv",
  path_config     = "config/check_data/l3_pce_outlier_check.yml",
  output_log_path = "data/l3_outliers_checked/logs/l3_pce_outliers_log.csv"
)
