# Load the ggplot2 package
library(ggplot2)

cut_1 <- -1
cut_2 <- 1

# Specify the means and variances
mean1 <- -1
variance1 <- 1
sd1 <- sqrt(variance1)

mean2 <- 1.5
variance2 <- 2
sd2 <- sqrt(variance2)

# Create a data frame with x values and corresponding normal densities for both distributions
data <- data.frame(x = seq(min(mean1 - 4 * sd1, mean2 - 4 * sd2), max(mean1 + 4 * sd1, mean2 + 4 * sd2), length = 100))
data$y1 <- dnorm(data$x, mean = mean1, sd = sd1)
data$y2 <- dnorm(data$x, mean = mean2, sd = sd2)

# Convert data to long format for ggplot2
library(tidyr)
data_long <- pivot_longer(data, cols = c("y1", "y2"), names_to = "Distribution", values_to = "Density")

# Plot the normal distributions using ggplot2
ggplot(data_long, aes(x = x, y = Density, color = Distribution)) +
  geom_line() +
  scale_color_manual(values = c("y1" = "blue", "y2" = "red"),
                     labels = c(paste("Mean =", mean1, ", Var =", variance1),
                                paste("Mean =", mean2, ", Var =", variance2))) +
  ggtitle("Two Normal Distributions") +
  xlab("x") +
  ylab("Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(floor(min(data$x)), ceiling(max(data$x)), by = 1)) +
  geom_vline(xintercept = cut_1, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = cut_2, linetype = "dashed", color = "black", size = 1)
  