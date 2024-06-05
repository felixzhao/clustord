library(ggplot2)

set.seed(123)

G <- 2
q <- 6

alpha <- c(-1, 1)
mu <- c(0, round(runif(5, 0.1, 2), 1))
phi <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

probs_list <- vector("list", G)

for (g in 1:G) {
  probs <- numeric(q)
  for (k in 1:q) {
    linear <- mu[k] + phi[k] * alpha[g]
    probs[k] <- if (k > 1) exp(linear) else 1
  }
  probs_list[[g]] <- probs / sum(probs)
}

samples <- lapply(seq_along(probs_list), function(g) {
  data.frame(
    sample_value = sample(1:q, size = 500, replace = TRUE, prob = probs_list[[g]]),
    cluster = g
  )
})

samples <- do.call(rbind, samples)

ggplot(samples, aes(x = sample_value, fill = as.factor(cluster))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Sampled Data",
       x = "Sample Value",
       y = "Density",
       fill = "Cluster") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", name = "Cluster")
