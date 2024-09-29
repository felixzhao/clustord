# Load the ggplot2 package
library(ggplot2)

cut_1 <- -1
cut_2 <- 1

# Specify the means and variance
means <- c(-3, -1.5, 0, 1.5, 3)
variance <- 1.5
sd <- sqrt(variance)

# Create a data frame with x values and corresponding normal densities for all distributions
data <- data.frame(x = seq(min(means - 4 * sd), max(means + 4 * sd), length = 100))

# Add the densities for each distribution to the data frame
data$y1 <- dnorm(data$x, mean = means[1], sd = sd)
data$y2 <- dnorm(data$x, mean = means[2], sd = sd)
data$y3 <- dnorm(data$x, mean = means[3], sd = sd)
data$y4 <- dnorm(data$x, mean = means[4], sd = sd)
data$y5 <- dnorm(data$x, mean = means[5], sd = sd)

# Convert data to long format for ggplot2
library(tidyr)
data_long <- pivot_longer(data, cols = starts_with("y"), names_to = "Distribution", values_to = "Density")

# Plot the normal distributions using ggplot2
ggplot(data_long, aes(x = x, y = Density, color = Distribution)) +
  geom_line() +
  scale_color_manual(values = c("y1" = "blue", "y2" = "red", "y3" = "green", "y4" = "purple", "y5" = "orange"),
                     labels = c(paste("Cluster 1,","Mean =", means[1], ", Var =", variance),
                                paste("Cluster 2,","Mean =", means[2], ", Var =", variance),
                                paste("Cluster 3,","Mean =", means[3], ", Var =", variance),
                                paste("Cluster 4,","Mean =", means[4], ", Var =", variance),
                                paste("Cluster 5,","Mean =", means[5], ", Var =", variance))) +
  ggtitle("Five Normal Distributions with Variance 1.5") +
  xlab("x") +
  ylab("Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(floor(min(data$x)), ceiling(max(data$x)), by = 1)) +
  geom_vline(xintercept = cut_1, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = cut_2, linetype = "dashed", color = "black", size = 1)
