
source('src/l2_transform.R')

l2_transform_pce(l1_consolidated_dir = 'data/l1_quality_checked/consolidated', config = "config/l2_pce_schema.yml")


l2_pce <- read_csv('data/l2_transformed/consolidated/l2_pce.csv')
l3_pce <- read_csv('data/l3_outliers_checked/consolidated/l3_pce.csv')

tmp <- filter(l3_pce, !is.na("purchased_from")) %>% arrange(purchased_from)
