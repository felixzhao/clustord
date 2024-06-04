library(ggplot2)
library(gridExtra)

set.seed(123)  # Setting seed for reproducibility


ord_cluster_simulate.plot <- function( q=3, alpha=c(1,-1), mu=c(0, 0.8, 0.6), phi=c(0, 0.7,1)) {
  probs <- numeric(q)
  
  G=1
  for (g in 1:G) {
    for (k in 1:q) {
      linear <- mu[k] + phi[k] * alpha[g]
      if (k > 1) {
        probs[k] <- exp(linear)
      } else {
        probs[k] <- 1
      }
    }
    prob <- probs/sum(probs)
    data_val <- sample(1:q, size = 1000, replace = TRUE, prob = prob)
  }
  
  # due to G=1, thus can plot directly
  
  # Creating a data frame for ggplot
  samples <- data.frame(data_val = data_val)
  
  # Generating the density plot
  return( ggplot(samples, aes(x = data_val)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = "Overall Density Plot of Sampled Data",
         x = "Sample Value",
         y = "Density") +
    theme_minimal()
  )
}

# diff alpha
# no mean to have multi alpha, due to only has one cluster
# thus, alpha always take the first
q <- 2
alpha=c(0.2)
p1 <- ord_cluster_simulate.plot(q=q, alpha=alpha)
alpha=c(1)
p2 <- ord_cluster_simulate.plot(q=q, alpha=alpha)
alpha=c(3)
p3 <- ord_cluster_simulate.plot(q=q, alpha=alpha)

grid.arrange(p1, p2, p3, ncol = 3)

# diff mu
mu <- c(0, 0.7, 0.5)
p4 <- ord_cluster_simulate.plot(mu=mu)
mu <- c(0, 0.2, 0.9)
p5 <- ord_cluster_simulate.plot(mu=mu)
mu <- c(0, 0.9, 0.2)
p6 <- ord_cluster_simulate.plot(mu=mu)

grid.arrange(p4, p5, p6, ncol = 3)

# diff phi

phi <- c(0, 0.2, 1)
p7 <- ord_cluster_simulate.plot(phi = phi)
phi <- c(0, 0.5, 1)
p8 <- ord_cluster_simulate.plot(phi = phi)
phi <- c(0, 0.9, 1)
p9 <- ord_cluster_simulate.plot(phi = phi)

grid.arrange(p7, p8, p9, ncol = 3)
