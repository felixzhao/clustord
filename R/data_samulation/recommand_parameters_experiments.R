set.seed(123)  # Setting seed for reproducibility

G <- 2
q <- 6

alpha <- c(-1, 1)
mu <- c(0, round(runif(5, 0.1, 2), 1))
phi <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

probs <- numeric(6)

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
