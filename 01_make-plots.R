## script to create plots for Columbia River spending evaluation

#### setup ####

library(dplyr)


#### load & munge data ####

## get data file
data_raw <- read.csv(here::here("data", "columbia_spending.csv"))

returns <- data_raw %>%
  select(year, ends_with("returns")) %>%
  mutate(chin_returns = chin_returns / 1000,
         coho_returns = coho_returns / 1000,
         sock_returns = sock_returns / 1000,
         sthd_returns = sthd_returns / 1000)

