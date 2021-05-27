#functions for use in the apportioment scripts

geometric_mean <- function(a) {
  sqrt(a * (a + 1))
}

arithmetic_mean <- function(a) {
  (2*a + 1) / 2
}

pull_maximum_priority <- function(df) {
  maxvalue <- df %>% 
    top_n(1, priority)
}

apportion_update <- function(df, method_app = "hunhill") {
  maximum_value <- pull_maximum_priority(df) %>% 
    mutate(seat_counter = seat_counter + 1,
           multiplier = 1/sqrt(seat_counter * (seat_counter + 1)),
           multiplier = case_when(
             method_app == "webster" ~ 1/((2 * seat_counter + 1)/2),
             method_app == "hunhill" ~ 1/sqrt(seat_counter * (seat_counter + 1)),
             TRUE ~ 1/sqrt(seat_counter * (seat_counter + 1))
           ),
           priority = population * multiplier)
  df <- df %>% rows_update(tibble(maximum_value), by = "stcd")
  return(df)
}

init_population_list <- function(population_df, method = "hunhill")  {
  population_df <- population_df %>%
    filter(stcd %in% state.abb & population > 0) %>% 
    mutate(seat_counter = 1,
           multiplier = case_when(
             method == "webster" ~ 1/1.5,
             method == "hunhill" ~ 1/sqrt(2),
             TRUE ~ 1/sqrt(2) #should be managed by an error handler
           ),
           priority = population * multiplier,
           pop_pct = population / sum(population) * 100,
           year = year,
           app = str_c(method,"_seq"),
           seat_num = row_number()
    )
  c_log <- population_df
  population_df <- population_df %>% select(-seat_num)
  return(list(a = population_df, b = c_log))
}

apportion_and_log <- function(p_list, method = "hunhill"){
  max_row <- pull_maximum_priority(p_list$a)
  max_row$seat_counter <- max_row$seat_counter + 1
  max_row <- max_row %>% mutate(seat_num = nrow(p_list$b) + 1)
  
  p_list$b <- rbind(p_list$b, max_row)
  p_list$a <- apportion_update(p_list$a, method_app = method)
  return(list(a = p_list$a, b = p_list$b))
}

webster_calc <- function(df, popsum = 0, seats = SEATS) {
  
  if(popsum == 0) popsum <- sum(df$population)
  df <- df %>%
    mutate(quota = population/popsum * seats,
           arith_mean = arithmetic_mean(floor(quota)),
           residual = quota - floor(quota),
           seat_counter = case_when(
             quota >= arith_mean ~ floor(quota) + 1,
             quota < arith_mean ~ floor(quota)
           )
    )

}

webster <- function(df, ADJUST = TRUE, TEST_CHANGE = 0, seats = SEATS) {
  
  popsum_input <- sum(df$population)
  df <- webster_calc(df, popsum = popsum_input, seats = seats) 
  seat_sum <- sum(df$seat_counter)
  
  if(ADJUST) {  
    popsum_adj <- popsum_input
    while(seat_sum < seats){
      popsum_adj <- popsum_adj - 1000
      df <- webster_calc(df, popsum = popsum_adj, seats = seats)
      seat_sum <- sum(df$seat_counter)
    }
    
    while(seat_sum > seats){
      popsum_adj <- popsum_adj + 1000
      df <- webster_calc(df, popsum = popsum_adj, seats = seats)
      seat_sum <- sum(df$seat_counter)
    }
  }
  df <- df %>% select(year, stcd, seat_counter, quota) %>% 
    mutate(app = "webster")
  
  return(df)
}    

hunhill_calc <- function(df, popsum = 0, seats = SEATS) {
  
  if(popsum == 0) popsum <- sum(df$population)
  df <- df %>%
    mutate(quota = population/popsum * seats,
           geom_mean = geometric_mean(floor(quota)),
           residual = quota - floor(quota),
           seat_counter = case_when(
             quota >= geom_mean ~ floor(quota) + 1,
             quota < geom_mean ~ floor(quota)
           )
    )
}

hunhill <- function(df, ADJUST = TRUE, TEST_CHANGE = 0, seats = SEATS) {
  
  popsum_input <- sum(df$population)
  df <- hunhill_calc(df, popsum = popsum_input, seats = seats) 
  seat_sum <- sum(df$seat_counter)
  
  if(ADJUST) {  
    popsum_adj <- popsum_input
    while(seat_sum < seats){
      popsum_adj <- popsum_adj - 1000
      df <- hunhill_calc(df, popsum = popsum_adj, seats = seats)
      seat_sum <- sum(df$seat_counter)
    }
    
    while(seat_sum > seats){
      popsum_adj <- popsum_adj + 1000
      df <- hunhill_calc(df, popsum = popsum_adj, seats = seats)
      seat_sum <- sum(df$seat_counter)
    }
  }
  df <- df %>% select(year, stcd, seat_counter, quota) %>% 
    mutate(app = "hunhill")
  
  return(df)
}    

generic_calc <- function(df, popsum = 0, seats = SEATS, method_calc = "hunhill") {
  
  if(popsum == 0) popsum <- sum(df$population)
  df <- df %>%
    mutate(quota = population/popsum * seats,
           mean = case_when (
             method_calc == "webster" ~ arithmetic_mean(floor(quota)),
             method_calc == "hunhill" ~ geometric_mean(floor(quota))),
           residual = quota - floor(quota),
           seat_counter = case_when(
             quota >= mean ~ floor(quota) + 1,
             quota < mean ~ floor(quota)
           )
    )
}


generic <- function(df, ADJUST = TRUE, TEST_CHANGE = 0, seats = SEATS, method = "hunhill") {
  
  popsum_input <- sum(df$population)
  df <- generic_calc(df, popsum = popsum_input, seats = seats, method_calc = method) 
  seat_sum <- sum(df$seat_counter)
  
  if(ADJUST) {  
    popsum_adj <- popsum_input
    while(seat_sum < seats){
      popsum_adj <- popsum_adj - 1000
      df <- generic_calc(df, popsum = popsum_adj, seats = seats)
      seat_sum <- sum(df$seat_counter)
    }
    
    while(seat_sum > seats){
      popsum_adj <- popsum_adj + 1000
      df <- generic_calc(df, popsum = popsum_adj, seats = seats)
      seat_sum <- sum(df$seat_counter)
    }
  }
  df <- df %>% select(year, stcd, seat_counter) %>% 
    mutate(app = method)
  
  return(df)
}    


