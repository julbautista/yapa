library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

source("R/process_polls.R")


# Model data --------------------------------------------------------------

# GE Model
polls_ge <- process_538_ge() 

# Counts for each GE week
# 0.2 and 0.6 due to historical split of “undecided/other” 
# voters in polls to major party candidates
y_ge <- polls_ge %>%
  select(`Trump (R)`, `Biden (D)`, Other) %>%
  mutate(`Trump (R)` = `Trump (R)` + 0.2*Other,
         `Biden (D)` = `Biden (D)` + 0.2*Other) %>%
  mutate(Other = 0.6*Other) %>%
  as.matrix()


# Total weeks
N_ge <- nrow(y_ge)

# Number of candidates
n_options <- ncol(y_ge)

# Days out from election
days_out_ge <- as.numeric(polls_ge$days_out)

# Combine into list
model_data_ge <- list(N_ge = N_ge,
                      y_ge = y_ge,
                      n_options = n_options,
                      days_out_ge = days_out_ge,
                      decay_param = 40)


# Model -------------------------------------------------------------------

fit_ge <- stan("stan/yapa_general.stan", data = model_data_ge,
               chains = 10, iter = 5000)
efge <- extract(fit_ge)


# Results -----------------------------------------------------------------

# Append results to tracker and save
qs <- apply(efge$results, 2, quantile, c(0.1, 0.5, 0.9))

load("results/ge_trend")

ge_today <- t(qs[2, ]) %>%
  as_data_frame() %>%
  rename(trump = V1, biden = V2, other = V3) %>%
  mutate(day = Sys.Date()) %>%
  gather(candidate, prop, -day) %>%
  left_join(t(qs[1, ]) %>%
              as_data_frame() %>%
              rename(trump = V1, biden = V2, other = V3) %>%
              mutate(day = Sys.Date()) %>%
              gather(candidate, lower, -day)) %>%
  left_join(t(qs[3, ]) %>%
              as_data_frame() %>%
              rename(trump = V1, biden = V2, other = V3) %>%
              mutate(day = Sys.Date()) %>%
              gather(candidate, upper, -day))

ge_trend <- ge_trend %>%
  filter(day != Sys.Date()) %>%
  rbind(ge_today)

save(ge_trend, file = "results/ge_trend")

pv_sims <- efge$results
save(pv_sims, file = "results/pv_sims")
