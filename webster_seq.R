library(tidyverse)

source("apportion_functions.R")

hist_pop <- "data/hist_pop_revised.csv"
population_main <- read_csv(hist_pop)

for (i in seq(1920, 2020, by=10)) {
  
      year <- as.character(i)
      population_df <- population_main %>% 
        select(stcd, population = !!year) 

      if(i <= 1950) {
          seats_to_allocate <- 387
      } else {
          seats_to_allocate <- 385
      }
      
      webster_sequence_df <- init_population_list(population_df)
    
      for(i in 1:seats_to_allocate) {
        webster_df <- apportion_and_log(webster_sequence_df)
      }
    
      webster_sequence_2 <- webster_sequence_df  
    
      for(i in 1:5) {
        webster_sequence_2 <- apportion_and_log(webster_sequence_2)
      }
    
      priority_435 <- webster_sequence_2$b$priority[435]
      priority_436 <- webster_sequence_2$b$priority[436]
    
      five_winners <- webster_sequence_2$b[431:435,] %>% 
         mutate(population_margin = population - priority_436/multiplier) %>% 
         mutate(population_margin_pct = population_margin/population) %>% 
         mutate(seat_counter = seat_counter + 1)
    
      five_losers <- webster_sequence_2$b[436:440,] %>% 
        mutate(population_margin = -(population - priority_435/multiplier)) %>% 
        mutate(population_margin_pct = population_margin/population)
    
      webster_sequence_df$a <- webster_sequence_df$a %>% 
        mutate(quota = population / sum(population) * 435) %>%
        mutate(seats_minus_quota = seat_counter - quota) %>% 
        mutate(quota = population / sum(population) * 435) %>%
        mutate(seats_minus_quota = seat_counter - quota) %>% 
        mutate(geom_mean = sqrt(floor(quota) * (floor(quota) + 1)), 
              seats_minus_gm = seat_counter - geom_mean)
      
      assign(str_c("webster_sequence_df_", year), webster_sequence_df)
      assign(str_c("five_winners_", year), five_winners)
      assign(str_c("five_losers_", year), five_losers)
}

webster_sequence_result <- bind_rows(webster_sequence_df_1920$a, 
                                       webster_sequence_df_1930$a, 
                                       webster_sequence_df_1940$a, 
                                       webster_sequence_df_1950$a, 
                                       webster_sequence_df_1960$a,
                                       webster_sequence_df_1970$a, 
                                       webster_sequence_df_1980$a, 
                                       webster_sequence_df_1990$a, 
                                       webster_sequence_df_2000$a, 
                                       webster_sequence_df_2010$a,
                                       webster_sequence_df_2020$a) %>%
  
          filter(as.numeric(year) >= 1940) %>% 
          mutate(s_display = ifelse(seat_counter <6, seat_counter, 7))

webster_sequence_result %>% 
  ggplot(aes(x = log(pop_pct), seats_minus_quota, col=s_display)) + 
  geom_point() +
  geom_smooth(method = lm)

model_hhill <- lm(seats_minus_quota ~ log(pop_pct), data = webster_sequence_result)
summary(model_hhill)

#locate different seats_minus_quota values in the distribution; which ones are worse?  Are they worse for big states?

webster_sequence_result <- webster_sequence_result %>% 
  group_by(stcd) %>% 
  mutate(rank = rank(seats_minus_quota)) %>% 
  filter(rank == 7) %>% 
  ungroup()

webster_sequence_2_all <- webster_sequence_result %>% 
  group_by(stcd) %>% 
  slice_min(seats_minus_quota,n=1) %>% 
  ungroup()
