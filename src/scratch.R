library(janitor)

tmp <- read_csv('data/l1_quality_checked/lookup/l1_lookup_sales_report.csv') %>%
  filter(!is.na(pce_reporting_id))


tmp2 <- tmp %>%
  group_by(pce_reporting_id) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(pce_reporting_id)



l2_pce <- read_csv('data/l2_transformed/consolidated/l2_pce.csv')

l3_pce <- read_csv('data/l3_outliers_checked/consolidated/l3_pce.csv')




# Manley Hot Springs
# should only be one, possibly 108?


# FIX: pull operator from operator lookup
# pull intertie from sales_reporting_lookup
