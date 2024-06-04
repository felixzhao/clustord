library(ggplot2)

set.seed(123)  # Setting seed for reproducibility

G <- 1
q <- 2

alpha <- c(1, -1)
mu <- c(0, 0.6)
phi <- c(0, 1)

ord_cluster_simulate.plot <- function

probs <- numeric(q)

for (g in 1:G) {
  for (k in 1:q) {
    linear <- mu[k] + phi[k] * alpha[g]
    if (k > 1) {
      probs[k] <- exp(linear)
    } else {
      probs[k] <- 1
    }
  }
}
prob <- probs/sum(probs)

data_val <- sample(1:q, size = 1000, replace = TRUE, prob = prob)

# Creating a data frame for ggplot
samples <- data.frame(data_val = data_val)

# Generating the density plot
ggplot(samples, aes(x = data_val)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Overall Density Plot of Sampled Data",
       x = "Sample Value",
       y = "Density") +
  theme_minimal()

