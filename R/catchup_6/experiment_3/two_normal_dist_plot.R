# Load the ggplot2 package
library(ggplot2)

# Specify the means and variances
## close
mean1 <- 0
variance1 <- 6
mean2 <- 3
variance2 <- 8
cut1 <- 0
cut2 <- 2.5

## medium
mean1 <- -1
variance1 <- 1
mean2 <- 1.5
variance2 <- 2
cut1 <- -1
cut2 <- 1

## far
mean1 <- 0
variance1 <- 1
mean2 <- 4
variance2 <- 2
cut1 <- 1
cut2 <- 2.5

sd1 <- sqrt(variance1)
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
  # ggtitle("Two Normal Distributions") +
  xlab("x") +
  ylab("Density") +
  theme_minimal()  +
theme(
  legend.title = element_text(size = 20),    # Increase legend title size
  legend.text = element_text(size = 20),     # Increase legend text size
  legend.key.size = unit(1.5, "lines"),       # Increase legend key size
  legend.position = c(0.8, 0.8),
  legend.background = element_rect(fill = "white", color = "black"),
  axis.text.x = element_text(size = 14),
  axis.title.x = element_text(size = 20)
) +
  scale_x_continuous(breaks = seq(floor(min(data$x)), ceiling(max(data$x)), by = 1)) +
  geom_vline(xintercept = cut1, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = cut2, linetype = "dashed", color = "black", size = 1)
  