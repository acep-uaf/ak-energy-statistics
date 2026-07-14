library(dplyr)
library(lubridate)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(cli)

header_raw <- read_csv('data/l1_tested/consolidated/l1_pce_header_consolidated.csv')

rate_line_raw <- read_csv('data/l1_tested/consolidated/l1_pce_rate_line_consolidated.csv')


header <- header_raw %>%
  # split identifier
  mutate(
    # id1 = str_sub(identifier, 1, 3), .before = identifier, # All records are 'A33', not sure significance, drop?
    community_id = str_sub(identifier, 4, 7), # Foreign key, community_id
    # fiscal_month_year = str_sub(identifier, 8, 11), # Fiscal month and year combo identifier, captured elsewhere, could drop
    # id4 = str_sub(identifier, 12, 17) # All records are '225003', not sure significance, drop?
  .before = community) %>%

  mutate(
    month = as.integer(month(parse_date_time(umr_month, 'B'))),
    fiscal_year = as.integer(fiscal_year),
    calendar_year = as.integer(calendar_year),
    community_id = as.integer(community_id),
    other_customers_description = as.character(other_customers_description)
  ) %>%

  arrange(calendar_year, month)



tmp2 <- header %>%
  filter(community_id == 1720) %>%
  select(
    community_id,
    umr_month,
    fiscal_year,
    calendar_year,
    month
  )
