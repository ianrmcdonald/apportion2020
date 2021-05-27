#Generates hunhill based on basic algorithm


library(conflicted)
library(tidyverse)
source("apportion_functions.R")
conflicted::conflict_prefer("filter", "dplyr")

SEATS <- 435

hist_pop_csv <- "data/hist_pop_post_census.csv"

#  read_csv(df, col_types = cols(.default = "d", date = "D"))

population_main <- read_csv(hist_pop_csv, col_types = cols(.default = "d", stcd = "c")) %>% 
  pivot_longer(!stcd, names_to = "year", values_to = "population") %>% 
  filter(!(population == 0)) %>% 
  filter(year >= 1940) %>% 
  group_split(year) %>% 
  purrr::map_dfr(generic) 





