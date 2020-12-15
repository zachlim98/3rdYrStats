library(dplyr)
library(tidyquant)
library(riingo)

riingo_set_token("9c884b918420c9220dcf832c83a6ba8025711e1e")

symbols <- 
  c("SPY", "AGG", "EFA", "BIL")

symbols %>% 
  riingo_prices(., 
                start_date = "2000-01-01",
                end_date = "2020-11-30") %>%
  mutate(date = ymd(date))  
  # see what happens if don't group_by here
  # group_by(ticker) %>% 
  # slice(1)