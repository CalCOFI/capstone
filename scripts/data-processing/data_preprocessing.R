library(tidyverse)
library(lubridate)

# filtering conditions
min_yr <- 1970

# stable_url <- 'https://calcofi.org/downloads/database/CalCOFI_Database_194903-202001_csv_22Sep2021.zip'
# bottle_raw <- read_csv(file = stable_url, n_max = 15)

# read in column names from bottle data
bottle_cols <- read_csv(file = 'data/raw/194903-202001_Bottle.zip',
                        col_names = F,
                        col_types = 'c',
                        n_max = 1) %>%
  pivot_longer(cols = everything(), 
               names_to = 'col', 
               values_to = 'name') %>%
  mutate(colnum = row_number()) %>%
  select(colnum, name)

# obtain indices for columns of interest
colname_strings <- 'O2ml|Salnty|Depth|Cnt|ID|Date|Quarter|degC'
selected_cols <- bottle_cols %>%
  filter(str_detect(name, colname_strings)) %>%
  arrange(colnum)

# store column indices
selected_col_ix <- selected_cols %>% pull(colnum)

# filter cast data by year
selected_casts <- read_csv(file = 'data/raw/194903-202001_Cast.csv',
                           col_select = c(Cst_Cnt, Year)) %>%
  filter(Year >= min_yr)

# store cast count of selected casts
selected_cast_cnt <- selected_casts %>% pull(Cst_Cnt)

## selective read -- FAILS IF SKIP TOO MANY ROWS
# # read cast column only of bottle data
# bottle_casts <- read_csv(file = '194903-202001_Bottle.zip',
#                          n_max = Inf,
#                          col_select = Cst_Cnt)
# 
# # extract row numbers of bottle data matching selected casts
# selected_row_ix <- bottle_casts %>%
#   mutate(rownum = row_number()) %>%
#   filter(Cst_Cnt %in% selected_cast_cnt) %>%
#   pull(rownum)
# 
# # check that rows are consecutive
# sum(diff(selected_row_ix) == 1) == (length(selected_row_ix) - 1)
# 
# # obtain first row at which to start reading
# bottle_read_start <- min(selected_row_ix) - 1

# read bottle data with column selection
bottle_raw <- read_csv(file = 'data/raw/194903-202001_Bottle.zip',
                       skip = 0,
                       n_max = Inf,
                       col_select = all_of(selected_col_ix),
                       col_types = cols(pH1 = col_double(),
                                        pH2 = col_double())) %>%
  filter(Cst_Cnt %in% selected_cast_cnt)

# merge cast data
bottle <- read_csv(file = 'data/raw/194903-202001_Cast.csv',
                   col_select = c(Cst_Cnt, Sta_ID, Cast_ID, 
                                  Date, Quarter, Lat_Dec, Lon_Dec, Distance)) %>%
  # join spatial info from cast data
  right_join(bottle_raw, cast_raw, by = c('Cst_Cnt', 'Sta_ID')) %>%
  # parse date column to datetime
  mutate(date = mdy(Date)) %>%
  # direct names
  rename(quarter = Quarter,
         lat = Lat_Dec,
         lon = Lon_Dec,
         depth = Depthm,
         oxygen = O2ml_L,
         temperature = T_degC,
         salinity = Salnty,
         id = Sta_ID,
         cast = Cst_Cnt) %>%
  # drop unused variables
  select(-c(Cast_ID, Btl_Cnt, Depth_ID, R_Depth, Date)) %>%
  # split id into station and line
  separate(id, c('line', 'station'), sep = ' ')


# row dimensions shouldn't change
nrow(bottle) == nrow(bottle_raw)

# export
save(bottle, file = 'data/processed/bottle.RData') # as r binary
# write_csv(bottle, file = 'bottle-processed.csv') # as csv