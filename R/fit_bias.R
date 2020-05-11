library(tidyverse)
library(rstan)


# Data --------------------------------------------------------------------


# Download historical poll averages from 538
pd <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.csv")

# Process
ge_polls <- pd %>%
  filter(str_detect(type_simple, 'Pres-G')) %>%
  mutate(polldate = as.Date(polldate, '%m/%d/%Y'),
         electiondate = as.Date(electiondate, '%m/%d/%Y'),
         days_out = date_diff(electiondate, polldate)) %>%
  mutate(cand1_actual = cand1_actual/100,
         cand2_actual = cand2_actual/100,
         cand3_actual = 1 - cand1_actual - cand2_actual,
         cand1_pct = cand1_pct/100,
         cand2_pct = cand2_pct/100,
         cand3_pct = 1 - cand1_pct - cand2_pct) %>%
  mutate(cand1_error = cand1_actual - cand1_pct,
         cand2_error = cand2_actual - cand2_pct,
         cand3_error = cand3_actual - cand3_pct) %>%
  select(year, location, samplesize, days_out, contains('cand')) 

# Helper function for mean of binomial
get_est <- function(y, n , wt) {
  (sum(y*wt) + 1)/(sum(y*wt) + 1 + sum(n*wt) - sum(y*wt) + 1)
}

# Calculate historical bias in state poll averages
bias <- ge_polls %>%
  mutate(cand1_count = cand1_pct*samplesize,
         cand2_count = cand2_pct*samplesize,
         cand3_count = cand3_pct*samplesize,
         wt = exp(-days_out/40)) %>%
  group_by(year, location) %>%
  summarise(cand1_est = get_est(cand1_count, samplesize, wt),
            cand1_actual = unique(cand1_actual),
            cand2_est = get_est(cand2_count, samplesize, wt),
            cand2_actual = unique(cand2_actual),
            cand3_est = get_est(cand3_count, samplesize, wt),
            cand3_actual = unique(cand3_actual),
            n = n()) %>%
  mutate(cand1_bias = (cand1_actual - cand1_est),
         cand2_bias = (cand2_actual - cand2_est),
         cand3_bias = (cand3_actual - cand3_est)) %>%
  ungroup() %>%
  filter(location != 'US') %>%
  left_join(
    data_frame(state = state.name, location = state.abb)
  ) %>%
  arrange(state)

bias_model_data <- list(
  'N' = nrow(bias),
  'n_states' = length(unique(bias$location)),
  'n_options' = 3,
  'state_id' = match(bias$location, unique(bias$location)),
  'bias' = bias %>% select(cand1_bias, cand2_bias, cand3_bias) %>% as.matrix()
)


# Model -------------------------------------------------------------------

bias_fit <- stan("stan/bias_model.stan", data = bias_model_data,
            chains = 3, iter = 1000)

ebf <- extract(bias_fit)

# Save historical bias mean and sd 
bias_mat <- colMeans(ebf$mu_bias)
bias_sd_mat <- round(colMeans(ebf$tau_bias), 3)

write.csv(bias_mat, "results/bias_mat.csv", row.names = F)
write.csv(bias_sd_mat, "results/bias_sd_mat.csv", row.names = F)
