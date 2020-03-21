library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# Data --------------------------------------------------------------------

# Results from 2016 (priors) and electoral college votes by state
prior_results <- read_csv("data/state_results_16.csv")

# Links to real clear politics polls links for each state
source("data/rcpp_links.R")

# Read polls data from RCP and process 
source("R/process_polls.R")
polls_data <- vector('list', length(rcpp_links))
for(i in 1:length(rcpp_links)) {
  polls_data[[i]] <- process_rcp(rcpp_links[[i]], n = 10) %>%
    mutate(state = names(rcpp_links)[i])
}
polls <- bind_rows(polls_data) %>%
  arrange(state)

# Vector of state names (51 = all states + DC)
state <- prior_results$state




# Model data --------------------------------------------------------------

# Counts for each option in each poll
y <- polls %>%
  select(-state, -Sample, -days_out) %>%
  as.matrix()

# Number of polls
N <- nrow(y)

# Prior data
priors <- prior_results %>%
  select(rep, dem, other) %>%
  as.matrix()

# Numeric identifier for each state
state_id <- match(polls$state, unique(polls$state))

# Number of states
n_states <- n_distinct(polls$state)

# Number of candidates
n_options <- ncol(y)

# Days out from election (for weighting)
days_out <- polls$days_out

# Combine into list
model_data <- list(n_options = n_options, 
                   n_states = n_states, 
                   N = N,
                   y = y,
                   state_id = state_id,
                   priors = priors,
                   days_out = days_out,
                   decay_param = 60)




# Fit model ---------------------------------------------------------------

m <- stan(file = "stan/yapa_states.stan", data = model_data,
          chains = 4, iter = 4000)

em <- rstan::extract(m)



# Visualize ---------------------------------------------------------------

# Results
means_trump <- apply(em$theta, 2, function(x) mean(x[, 1]))
quantiles_trump <- apply(em$theta, 2, function(x) quantile(x[, 1], c(0.1, 0.9)))

means_biden <- apply(em$theta, 2, function(x) mean(x[, 2]))
quantiles_biden <- apply(em$theta, 2, function(x) quantile(x[, 2], c(0.1, 0.9)))

results_biden <- data_frame(
  state = state,
  lower = quantiles_biden[1, ],
  mean  = means_biden,
  upper = quantiles_biden[2, ],
  cand  = 'biden')

results_trump <- data_frame(
  state = state,
  lower = quantiles_trump[1, ],
  mean  = means_trump,
  upper = quantiles_trump[2, ],
  cand  = 'trump')

# Save
data.table::fwrite(results_biden, "results/results_biden.csv")
data.table::fwrite(results_trump, "results/results_trump.csv")

# Formatted Table
state_results <- results_biden %>%
  rename(`Lower Biden` = lower,
         `Upper Biden` = upper,
         `Mean Biden`  = mean) %>%
  select(-cand) %>%
  left_join(results_trump %>%
              rename(`Lower Trump` = lower,
                     `Upper Trump` = upper,
                     `Mean Trump`  = mean) %>%
              select(-cand)) %>%
  rename(State = state) %>%
  arrange(-`Mean Biden`) %>%
  mutate_if(is.numeric, function(x) paste0(round(x*100), "%"))

data.table::fwrite(state_results, "results/state_results.csv")


# Plot
state_plot <- results_biden %>%
  ggplot() +
  aes(x = mean, y = reorder(state, mean),
      xmin = lower, xmax = upper) +
  geom_point(col = "blue") +
  geom_errorbarh(height = 0, col = "blue") +
  geom_point(data = results_trump, 
             aes(x = mean, y = state,
                 xmin = lower, xmax = upper),
             col = "red") +
  geom_errorbarh(data = results_trump,
                 aes(x = mean, y = state,
                     xmin = lower, xmax = upper), 
                 height = 0, col = "red") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1, 0.25),
                     labels = paste0(seq(0, 100, 25), "%")) +
  labs(x = "vote share", y = NULL,
       title = "estimated vote share by state",
       subtitle = paste0("source: state polls via RCP as of ", Sys.Date())) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 7),
        axis.text.y = element_text(size = 9))

ggsave("docs/state_distributions.png", state_plot, height = 9)



# P-win --------------------------------------------------------------------

# Probability of winning the state
p_biden <- round(apply(em$theta, 2, function(x) mean(x[, 2] > x[, 1])), 3)
names(p_biden) <- state

write.csv(data.frame(p_biden) %>%
            tibble::rownames_to_column("state"),
          "results/p_biden.csv", row.names = F)




# Simulate electoral college ----------------------------------------------

er <- matrix(0, nrow = dim(em$theta)[1], ncol = dim(em$theta)[3])

# Simulate elections and results
for(i in 1:dim(em$theta)[1]) {
  for(s in 1:dim(em$theta)[2]) {
    for(o in 1:dim(em$theta)[3]) {
      if(em$theta[i, s, o] == max(em$theta[i, s, ])) {
        er[i, o] <- er[i, o] + prior_results$ev[s]
      }
    }
  }
}
data.table::fwrite(er, "results/electoral_college_sims.csv")

# Plot
ec_plot <- data_frame(
  electoral_votes = c(er[, 1], er[, 2]),
  candidate = c(rep("Trump", nrow(er)), rep("Biden", nrow(er)))
) %>% 
  ggplot() +
  aes(x = electoral_votes, fill = candidate, y = ..density..) +
  geom_histogram(alpha = 0.7, bins = 60,
                 position = "identity") +
  scale_x_continuous(limits = c(0, 538),
                     breaks = c(seq(0, 538, 100), 270)) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(x = "electoral college votes", y = NULL, fill = NULL,
       title = "distribution of electoral college votes") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15, face = "bold", hjust = .5),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11))

ggsave("docs/ec_distributions.png", ec_plot)




# State simulations -------------------------------------------------------

simulations <- data_frame(
  value = c(c(em$theta[, , 1]), c(em$theta[, , 2]), c(em$theta[, , 3])),
  state = rep(rep(state,  each = dim(em$theta)[1]), times = 3),
  candidate = rep(c("Trump", "Biden", "Other"), each = dim(em$theta)[1]*51)
) %>%
  group_by(state, candidate) %>%
  mutate(mean = mean(value)) %>%
  ungroup() %>%
  arrange(state)

data.table::fwrite(simulations, "results/state_simulations.csv", row.names = F)




