library(dplyr)
library(lubridate)

header <- read_csv('data/l1/l1_header_2026_06-12.csv')

rates <- read_csv('data/l1/l1_rates_2026-06-12.csv')


tmp <- header %>%
  # split identifier
  mutate(
    id1 = str_sub(identifier, 1, 3), .before = identifier, # All records are 'A33', not sure significance, drop?
    community_id = str_sub(identifier, 4, 7), # Foreign key, community_id
    fiscal_month_year = str_sub(identifier, 8, 11), # Fiscal month and year combo identifier
    id4 = str_sub(identifier, 12, 17) # All records are '225003', not sure significance, drop?
  ) %>%

  mutate(
    month = as.integer(month(parse_date_time(umr_month, 'B'))),
    fiscal_year = as.integer(fiscal_year),
    calendar_year = as.integer(calendar_year),
    community_id = as.integer(community_id),
    fiscal_month_year = as.integer(fiscal_month_year),
    other_customers_description = as.character(other_customers_description)
  ) %>%


  arrange(calendar_year, month)





tmp2 <- tmp %>%
  filter(community_id == 1720) %>%
  select(
    community_id,
    fiscal_month_year,
    umr_month,
    fiscal_year,
    calendar_year,
    month
  )
