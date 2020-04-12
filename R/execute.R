# Fit model and render site
source("R/fit_general.R") 
source("R/fit_states.R") 
setwd("docs")
rmarkdown::render_site()
setwd("..")


df <- data_frame(
  bias = (em$bias[, 2] - 100)/100,
  pwin = ifelse(ec_sims[, 2] > ec_sims[, 1], 1, 0)
)
df

df %>%
  group_by(bias = round(bias, 2)) %>%
  filter(bias > -.15) %>%
  summarise(p_win = mean(pwin), n = n()) %>%
  ggplot() +
  aes(x = bias, y = p_win) +
  geom_smooth(col = "black", se = F) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25),
                     labels = paste0(seq(0, 100, 25), "%")) +
  scale_x_continuous(limits = c(-0.05, 0.05),
                     breaks = seq(-.1, .1, 0.025),
                     labels = paste0(seq(-10, 10, 2.5), "%")) + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "polling error", y = "probability of Biden victory")


n <- 15
res <- 0
for(i in 1:n) res <- res + i^2

ok <- seq(1:n)^2

res
sum(ok)
