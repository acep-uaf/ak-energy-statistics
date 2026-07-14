source('src/l0_extract.R')
source('src/l1_test.R')
source('src/l2_consolidate.R')

l0_extract_pce_dir()

l1_test_pce_dir()

l2_consolidate_pce_data(path_with_pattern = "data/l1/l1_pce_header", id_col = "identifier")
