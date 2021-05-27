# block to compare hunhill to webster

merge_all <- bind_rows(hunt_hill_sequence_result, hunhill_result, webster_result) 
states <- read_csv(str_c(post_dir, "data/state_names.csv"))
merge_all <-  merge_all %>% 
  select(stcd, seat_counter, year, app) %>% 
  pivot_wider(id_cols = c("stcd", "year"), names_from = app, values_from = seat_counter) %>%
  mutate(huntest = hunhill_seq - hunhill,
         web_v_hun = webster - hunhill) %>% 
  filter(web_v_hun != 0)

merge_all <-  left_join(merge_all, webster_result) %>% 
  select(stcd, year, hunhill, webster, web_v_hun, quota)

merge_all <- left_join(merge_all, states, by = "stcd")  %>% 
  select(st_name, year, hunhill, webster, web_v_hun, quota)
merge_all %>% mutate(remainder = hunhill - quota) %>% ggplot(aes(x = remainder)) + geom_histogram(aes(y=..density..), colour="black", fill="white", bins=20) +  geom_density(alpha=.2, fill="#FF6666") 
 
save.image(file = "my_work_space.RData")
