source('src/l0_extract.R')
source('src/l1_test.R')
source('src/l1_consolidate.R')
source('src/l2_transform.R')

dir_delete('data/l0_extracted')
dir_delete('data/l1_tested')
dir_delete('data/l2')

l0_extract_pce_dir(dir_in = 'data/raw', pattern = 'raw_pce')

l1_test_pce_dir(dir_in = 'data/l0_extracted/monthly', pattern = 'l0_pce')


l1_consolidate_pce_data(path_with_pattern = "data/l1_tested/monthly/l1_pce_rate_line", join_by_columns = c("identifier", "line_no"))
l1_consolidate_pce_data(path_with_pattern = "data/l1_tested/monthly/l1_pce_header", join_by_columns = c("identifier", "line_no"))

l2_transform_pce(l1_consolidated_dir = 'data/l1_tested/consolidated')
