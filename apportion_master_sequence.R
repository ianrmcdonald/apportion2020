library(tidyverse)
library(tidycensus)

source("apportion_functions.R")
SEATS <- 435
METHOD <- "hunhill"

hist_pop_csv <- "data/hist_pop_post_census.csv"

population_main <- read_csv(hist_pop_csv, col_types = cols(.default = "d", stcd = "c")) %>% 
  pivot_longer(!stcd, names_to = "year", values_to = "population") %>% 
  filter(year >= 1940) %>% 
  filter(!(population == 0)) %>% 
  group_split(year) 

five_winners_agg <- tibble()
five_losers_agg <- tibble()
allocation_agg <- tibble()
log_agg <- tibble()

for(seq in 1:9) {
  
  pm <- population_main[[seq]]
  states_number <- nrow(pm)
  
  seats_to_allocate <- SEATS - states_number
  
  pm <- init_population_list(pm, method = METHOD)


  for (i in 1:seats_to_allocate) {
      pm <- apportion_and_log(pm, method = METHOD)
    }
    
  pm2 <- pm
  
  #this gets five more assignments past the last valid one  
  for (i in 1:5) {
      pm2 <- apportion_and_log(pm2, method = METHOD)
    }
    
  
  priority_last_winner <- pm2$b$priority[SEATS]
  priority_first_loser <- pm2$b$priority[SEATS + 1]
    
  five_winners <- pm2$b[(SEATS-4):SEATS, ] %>%
      mutate(population_margin = population - priority_first_loser / multiplier) %>%
      mutate(population_margin_pct = population_margin / population)
    
  five_losers <- pm2$b[(SEATS + 1):(SEATS + 5), ] %>%
      mutate(population_margin = -(population - priority_last_winner / multiplier)) %>%
      mutate(population_margin_pct = population_margin / population)
    
  pm$a <- pm$a %>%
      mutate(quota = population / sum(population) * SEATS) %>%
      mutate(seats_minus_quota = seat_counter - quota) %>%
      mutate(geom_mean = sqrt(floor(quota) * (floor(quota) + 1)),
             seats_minus_gm = seat_counter - geom_mean) %>% 
      mutate(arith_mean = (2 * floor(quota) + 1) / 2,
         seats_minus_am = seat_counter - arith_mean)
  
  allocation_agg <- bind_rows(allocation_agg, pm$a)
  log_agg <- bind_rows(log_agg, pm$b)
  five_losers_agg <- bind_rows(five_losers_agg, five_losers)
  five_winners_agg <- bind_rows(five_winners_agg, five_winners)
  
}

