er <- matrix(0, nrow = dim(em$theta)[1], ncol = dim(em$theta)[3])

# Simulate some elections and their results
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
