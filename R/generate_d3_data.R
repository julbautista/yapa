load("results/state_results")
load("results/state_simulations")
load("results/p_biden")
load("results/ec_sims")
load("results/results_biden")
load("results/results_trump")
load("results/ge_trend")

electoral_votes <- read_csv("data/state_results_16.csv") %>% select(state, ev) %>% rename(State = state)

names(state_results) <- stringr::str_replace(make.names(names(state_results)),"\\.", "_")

state_results <-
state_results %>% 
  mutate(Lower_Biden = as.numeric(str_remove(Lower_Biden,"%"))/100,
         Mean_Biden = as.numeric(str_remove(Mean_Biden,"%"))/100,
         Upper_Biden = as.numeric(str_remove(Upper_Biden,"%"))/100,
         Lower_Trump = as.numeric(str_remove(Lower_Trump,"%"))/100,
         Mean_Trump = as.numeric(str_remove(Mean_Trump,"%"))/100,
         Upper_Trump = as.numeric(str_remove(Upper_Trump,"%"))/100) %>%
  left_join(electoral_votes) %>%
  mutate(uncertainty = abs(Mean_Biden - Mean_Trump))


list(state_results, state_simulations,
     p_biden, ec_sims,
     results_biden, results_trump, ge_trend)

current_rates <- ge_trend %>% filter(day == max(day)) %>% pull(prop)
popular_vote <- ge_trend %>% 
  filter(day == max(day)) %>% 
  mutate(lower = paste0(floor(lower*100), "%"), 
         upper = paste0(ceiling(upper*100), "%")) %>% 
  select(candidate, lower, upper)

state_ts <- read_csv("results/state_ts.csv")
prior_results <- read_csv("data/state_results_16.csv")

biden <- round(quantile(ec_sims[, 2], c(0.05, 0.95)))
trump <- round(quantile(ec_sims[, 1], c(0.05, 0.95)))

hist_data <-
data_frame(
  electoral_votes = c(ec_sims[, 1], ec_sims[, 2]),
  candidate = c(rep("Trump", nrow(ec_sims)), rep("Biden", nrow(ec_sims)))
) 

table_data <-
p_biden %>%
  mutate(cat = case_when(p_biden >= 0.9 ~ "Likely Biden",
                         p_biden > 0.7 & p_biden < 0.9 ~ "Lean Biden",
                         p_biden <= 0.1 ~ "Likely Trump",
                         p_biden > 0.1 & p_biden < 0.3 ~ "Lean Trump", 
                         TRUE ~ "Uncertain")) %>%
  group_by(cat) %>%
  arrange(p_biden) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(rank = max(rank) - rank)


md <- map_data("state")

map_data <- md %>%
  left_join(p_biden %>%
              mutate(region = tolower(state))) %>%
  left_join(prior_results %>%
              select(state, ev)) %>%
  mutate(state = paste0(state, " (", ev, ")")) %>%
  arrange(group, order) %>%
  select(state, lat, long, p_biden, group) %>%
  mutate(p_biden = ifelse(p_biden > 0.99, 0.99, p_biden),
         p_biden = ifelse(p_biden < 0.01, 0.01, p_biden),
         `p(biden)` = round(p_biden, 2))




plot_state_ts <- function(s) {
  p <- state_ts %>% 
    filter(state == s) %>% select(-state) %>%
    gather(metric, value, -date) %>%
    mutate(candidate = sapply(strsplit(metric, "_"), tail, 1),
           metric = sapply(strsplit(metric, "_"), head, 1)) %>%
    spread(metric, value) 
  return(p)
  }

p_win <- function(s, c = "Biden") {
  oppo <- ifelse(c == "Biden", "Trump", "Biden")
  res <- state_simulations %>%
    filter(state == s) %>%
    summarise(pwin = mean(value[candidate == c] > value[candidate == oppo])) %>%
    pull(pwin) %>%
    round(3)
  res
}

for(s in p_biden$state) {
  pwin <- p_win(s, "Biden")
  pwin <- case_when(pwin < 0.01 ~ "less than 1%",
                    pwin > 0.99 ~ "more than 99%", 
                    TRUE ~ paste0(round(pwin*100, 0), "%"))
  ptrump <- p_win(s, "Trump")
  ptrump <- case_when(ptrump < 0.01 ~ "less than 1%",
                      ptrump > 0.99 ~ "more than 99%", 
                      TRUE ~ paste0(round(ptrump*100, 0), "%"))
  plot_state_ts(s)
}

write_csv(state_ts, "d3plots/data/state_ts.csv")
write_csv(hist_data, "d3plots/data/hist_data.csv")
write_csv(table_data, "d3plots/data/table_data.csv")
write_csv(map_data, "d3plots/data/map_data.csv")
write_csv(results_biden, "d3plots/data/results_biden.csv")
write_csv(state_results, "d3plots/data/state_results.csv")


fte <- read_csv("https://projects.fivethirtyeight.com/polls-page/president_polls.csv")
