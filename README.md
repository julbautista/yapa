
``` r
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# DATA --------------------------------------------------------------------

# Results from 2016 (priors) and electoral college votes by state
prior_results <- read_csv("data/state_results_16.csv")

# Links to real clear politics polls links for each state
source("data/rcpp_links.R")

# Read polls data from RCP and process 
source("R/process_polls.R")
polls_data <- vector('list', length(rcpp_links))
for(i in 1:length(rcpp_links)) {
  polls_data[[i]] <- process_rcp(rcpp_links[[i]],  wt_function = function(x) 1/x) %>%
    mutate(state = names(rcpp_links)[i])
}
polls <- bind_rows(polls_data) %>%
  select(-end_date) %>%
  arrange(state)

# Vector of state names (51 = all states + DC)
state <- prior_results$state




# MODEL DATA --------------------------------------------------------------

# Weighted sample for each poll
n <- polls %>%
  pull(Sample)

# Weighted counts for each option in each poll
y <- polls %>%
  select(-state, -Sample) %>%
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

# Combine into list
model_data <- list(n_options = n_options, 
                   n_states = n_states, 
                   N = N,
                   n = n,
                   y = y,
                   state_id = state_id,
                   priors = priors)




# Fit model ---------------------------------------------------------------

m <- stan(file = "stan/yapa_states.stan", data = model_data,
          chains = 4, iter = 2000)

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

# Plot
results_biden %>%
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
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 7),
        axis.text.y = element_text(size = 11))
```

<img src="README_files/figure-markdown_github/some code-1.png" style="display: block; margin: auto;" />

``` r
# P-win --------------------------------------------------------------------

# Probability of winning the state
p_biden <- round(apply(em$theta, 2, function(x) mean(x[, 2] > x[, 1])), 3)
names(p_biden) <- state
data.frame(p_biden)
```

    ##                      p_biden
    ## Alabama                0.197
    ## Alaska                 0.298
    ## Arizona                0.471
    ## Arkansas               0.174
    ## California             0.996
    ## Colorado               0.555
    ## Connecticut            0.812
    ## Delaware               0.828
    ## District of Columbia   0.997
    ## Florida                0.496
    ## Georgia                0.592
    ## Hawaii                 0.866
    ## Idaho                  0.128
    ## Illinois               0.727
    ## Indiana                0.261
    ## Iowa                   0.552
    ## Kansas                 0.184
    ## Kentucky               0.069
    ## Louisiana              0.246
    ## Maine                  0.524
    ## Maryland               0.825
    ## Massachusetts          0.935
    ## Michigan               0.913
    ## Minnesota              0.516
    ## Mississippi            0.294
    ## Missouri               0.254
    ## Montana                0.238
    ## Nebraska               0.182
    ## Nevada                 0.522
    ## New Hampshire          0.726
    ## New Jersey             0.669
    ## New Mexico             0.780
    ## New York               0.857
    ## North Carolina         0.576
    ## North Dakota           0.104
    ## Ohio                   0.566
    ## Oklahoma               0.096
    ## Oregon                 0.649
    ## Pennsylvania           0.810
    ## Rhode Island           0.682
    ## South Carolina         0.187
    ## South Dakota           0.140
    ## Tennessee              0.214
    ## Texas                  0.372
    ## Utah                   0.274
    ## Vermont                0.815
    ## Virginia               0.544
    ## Washington             0.813
    ## West Virginia          0.070
    ## Wisconsin              0.588
    ## Wyoming                0.062

``` r
# Simulate 4000 elections
er <- matrix(0, nrow = dim(em$theta)[1], ncol = dim(em$theta)[3])

for(i in 1:dim(em$theta)[1]) {
  for(s in 1:dim(em$theta)[2]) {
    for(o in 1:dim(em$theta)[3]) {
      if(em$theta[i, s, o] == max(em$theta[i, s, ])) {
        er[i, o] <- er[i, o] + prior_results$ev[s]
      }
    }
  }
}

# Prob biden wins
mean(er[, 2] > er[, 1])
```

    ## [1] 0.87825

``` r
data_frame(
  electoral_votes = c(er[, 1], er[, 2]),
  candidate = c(rep("Trump", nrow(er)), rep("Biden", nrow(er)))
) %>% 
  ggplot() +
  aes(x = electoral_votes, fill = candidate, y = ..density..) +
  geom_histogram(alpha = 0.75, bins = 60,
                 position = "identity") +
  scale_x_continuous(limits = c(0, 538),
                     breaks = c(seq(0, 538, 100), 270)) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(x = "Electoral College Total", y = NULL, fill = NULL) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_blank(),
        axis.text.y = element_blank())
```

![](README_files/figure-markdown_github/sim-1.png)