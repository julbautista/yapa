library(rvest)
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



# function to calculate numeric difference in two dates in days.
date_diff <- function(t1, t2) {
  round(
    as.numeric(
      difftime(t1, t2, unit = 'days')
    )
  )
}

# Function to process state-level polls from RCP for the 2020 presidential election.
# Parameters:
## site: character string to web address of rcp site;
## election day: character string of election day, used for weighting polls;
## n: how many recent polls should we look at?
process_rcp_16 <- function(site, election_day = "2016-11-06", n = Inf) {
  
  # If site is NULL, assume there is no data
  if(is.null(site)) {
    polls <- data_frame(
      Sample = 0, `Trump (R)` = 0, `Clinton (D)` = 0, 
      Other = 0, days_out = 365
    )
  } else {
    
    # Read html from site
    poll_tbl <- site %>%
      read_html() %>%
      html_table() 
    
    # Process data: format sample and date; drop unnecessary rows and columns; weight.
    raw_data <- poll_tbl %>%
      .[[length(poll_tbl)]] %>%
      filter(!Poll %in% c("RCP Average", "Final Results")) %>%
      mutate(Sample = as.numeric(gsub("([0-9]+).*$", "\\1", Sample))) %>%
      mutate(end_date = sapply(strsplit(Date, " - "), tail, 1),
             end_date = as.Date(paste0(end_date, "/2016"), "%m/%d/%Y"),
             end_date = if_else(end_date > "2016-11-06", as.Date(gsub("2016", "2015", end_date)),
                                end_date)) %>%
      mutate_if(is.character, function(x) gsub("--", "0", x)) %>%
      mutate(days_out = date_diff(election_day, end_date)) %>%
      select(-Poll, -Date, -Spread, -end_date) %>%
      mutate_if(is.character, as.numeric)  %>%
      na.omit() %>%
      filter(Sample < Inf) %>%
      head(n = n)
    
    # Drop MoE column if exists (not in all for some reason)
    if(any(names(raw_data) == "MoE")) raw_data <- select(raw_data, -MoE)
    
    # Process raw data into weighted counts.  Account for "other"
    polls <- raw_data 
    for(n in names(polls)[!names(polls) %in% c("Sample", "days_out")]) {
      polls[, n] <- round(polls[, 1]*polls[, n]/100)
    }
    polls$Other <- polls$Sample - apply(polls[!names(polls) %in% c("Sample", "days_out")], 1, sum)
    polls$Other <- if_else(polls$Other < 0, 0, polls$Other)
  }
  
  polls
  
}


# Data --------------------------------------------------------------------

# Results from 2016 (priors) and electoral college votes by state
prior_results <- read_csv("data/state_results_12.csv")

# Links to real clear politics polls links for each state
source("data/rcpp_links_16.R")

polls_data <- vector('list', length(rcpp_links))
for(i in 1:length(rcpp_links)) {
  polls_data[[i]] <- process_rcp_16(rcpp_links[[i]], n = 10) %>%
    mutate(state = names(rcpp_links)[i])
}
polls <- bind_rows(polls_data) %>%
  select(-c(`Johnson (L)`, `Stein (G)`, `McMullin (I)`)) %>%
  arrange(state)

# Vector of state names (51 = all states + DC)
state <- prior_results$state




# Model data --------------------------------------------------------------

# Counts for each option in each poll
y <- polls %>%
  select(-state, -Sample, -days_out) %>%
  as.matrix()

# Total sample in each poll
n <- polls %>% pull(Sample)

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
                   n = n,
                   state_id = state_id,
                   priors = priors,
                   days_out = days_out,
                   decay_param = 60)




# Fit model ---------------------------------------------------------------

m <- stan(file = "stan/yapa_states.stan", data = model_data,
          chains = 10, iter = 5000)

em <- rstan::extract(m)


# Visualize ---------------------------------------------------------------

# Results
means_trump <- apply(em$results, 2, function(x) mean(x[, 1]))
quantiles_trump <- apply(em$results, 2, function(x) quantile(x[, 1], c(0.05, 0.95)))

means_clinton <- apply(em$results, 2, function(x) mean(x[, 2]))
quantiles_clinton <- apply(em$results, 2, function(x) quantile(x[, 2], c(0.05, 0.95)))

results_clinton <- data_frame(
  state = state,
  lower = quantiles_clinton[1, ],
  mean  = means_clinton,
  upper = quantiles_clinton[2, ],
  cand  = 'clinton')

results_trump <- data_frame(
  state = state,
  lower = quantiles_trump[1, ],
  mean  = means_trump,
  upper = quantiles_trump[2, ],
  cand  = 'trump')

# Formatted Table
state_results <- results_clinton %>%
  rename(`Lower Clinton` = lower,
         `Upper Clinton` = upper,
         `Mean Clinton`  = mean) %>%
  select(-cand) %>%
  left_join(results_trump %>%
              rename(`Lower Trump` = lower,
                     `Upper Trump` = upper,
                     `Mean Trump`  = mean) %>%
              select(-cand)) %>%
  rename(State = state) %>%
  arrange(-`Mean Clinton`) 

save(state_results, file = "results/state_results_16")





# Simulate electoral college ----------------------------------------------

ec_sims <- matrix(0, nrow = dim(em$results)[1], ncol = dim(em$results)[3])

# Simulate elections and results
for(s in 1:dim(em$results)[2]) {
  for(o in 1:dim(em$results)[3]) {
    for(i in 1:dim(em$results)[1]) {
      if(em$results[i, s, o] == max(em$results[i, s, ])) {
        ec_sims[i, o] <- ec_sims[i, o] + prior_results$ev[s]
      }
    }
  }
}

save(ec_sims, file = "results/ec_sims_16")

# Plot
data_frame(
  electoral_votes = c(ec_sims[, 1], ec_sims[, 2]),
  candidate = c(rep("Trump", nrow(ec_sims)), rep("Clinton", nrow(ec_sims)))
) %>% 
  ggplot() +
  aes(x = electoral_votes, fill = candidate, y = ..density..) +
  geom_histogram(alpha = 0.7, bins = 538,
                 position = "identity") +
  scale_x_continuous(limits = c(0, 538),
                     breaks = c(seq(0, 538, 100), 270)) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_vline(xintercept = 270, lty = 2, col = "black") +
  theme_minimal() +
  labs(x = "electoral college votes", y = NULL, fill = NULL,
       title = "distribution of electoral college votes") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15, face = "bold", hjust = .5),
        plot.subtitle = element_text(size = 8),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11))



# PPC
# dataframe of states and abbreviations for plotting
sdf <- data.frame(abb = c(state.abb, "DC"), 
                  state = c(state.name, "District of Columbia"))

# Join predictions with actual results
ppc16 <- read_csv("data/state_results_16.csv") %>% 
  left_join(state_results %>% rename(state = State)) %>% 
  select(dem, mean = `Mean Clinton`, lower = `Lower Clinton`, 
         upper = `Upper Clinton`, state) %>%
  left_join(sdf)

save(ppc16, file = "results/ppc16")

# Check calibration (80% should be in 80% interval)
mean(ppc16$dem > ppc16$lower & ppc16$dem < ppc16$upper)

# Plot
ppc16 %>%
  ggplot() +
  aes(x = mean, y = dem, xmin = lower, xmax = upper, 
      label = abb, col = dem) + 
  geom_abline(intercept = 0, slope = 1, lty = 2, col = 'darkgrey') +
  geom_point(alpha = 0.8, cex = 0.7) +  
  geom_errorbarh(height = 0, alpha = 0.7, lwd = 0.4) +
  ggrepel::geom_text_repel(size = 3, lwd = 0.4) +
  xlim(0, 1) + ylim(0, 1) + 
  scale_color_gradient(low = "red", high = "blue") +
  guides(col = F) +
  theme_minimal() +
  labs(x = "prediction", y = "result",
       title = "Model predictions and uncertainty intervals vs election results: 2016") +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  annotate("text", x = 0.2, y = 0.8, label = "Clinton beat prediction") +
  annotate("text", x = 0.8, y = 0.2, label = "Trump beat prediction")

# Table
ppc16 %>%
  select(state, `lower prediction` = lower,
         prediction = mean, `upper prediction` = upper, 
         result = dem) %>%
  mutate_if(is.numeric, function(x) paste0(round(x*100, 1), "%"))

