library(readxl)
raw_header <- read_xlsx(path = 'data/raw/raw_pce_2026-06.xlsx', sheet = 'HeaderData')
raw_rate_line <- read_xlsx(path = 'data/raw/raw_pce_2026-06.xlsx', sheet = 'RateLineData')


l0_header <- read_csv('data/l0/l0_pce_header_2026-06.csv')
l0_rate_line <- read_csv('data/l0/l0_pce_rate_line_2026-06.csv')

l1_header <- read_csv('data/l1/l1_pce_header_2026-06.csv')
l1_rate_line <- read_csv('data/l1/l1_pce_rate_line_2026-06.csv')


l1_header_2026_06 <- read_csv('data/l1/l1_pce_header_2026-06.csv')
l1_header_2026_07 <- read_csv('data/l1/l1_pce_header_2026-07.csv')

l2_pce_header <- read_csv('data/l2/l2_pce_header.csv')
