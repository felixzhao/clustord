library(ggplot2)

# Set the number of observations
n <- 1000  # Total number of data points

# Proportions from each distribution
p1 <- 0.6  # 60% from approx N(4, sqrt(0.5))
p2 <- 0.4  # 40% from approx N(1, sqrt(0.5))

# Generate a random sample of labels according to the proportions
labels <- sample(c(2, 1), size = n, replace = TRUE, prob = c(p1, p2))

# Generate data based on the labels
data <- ifelse(labels == 2, rnorm(sum(labels == 2), mean = 4, sd = sqrt(0.5)),
               rnorm(sum(labels == 1), mean = 1, sd = sqrt(0.5)))

# Round data to nearest integer and ensure it stays within [1,9]
data <- pmin(pmax(round(data), 1), 9)

# Create a data frame to store the data and labels
df <- data.frame(value = data, label = labels)

# Plot both individual densities and overall density
plot <- ggplot(df, aes(x = value, group = label, fill = as.factor(label))) +
  geom_histogram(position = "identity", binwidth = 1, alpha = 0.6, colour = "black") +
  geom_histogram(data = df, aes(y = ..density..), binwidth = 1, fill = "gray", alpha = 0.3) +
  scale_fill_manual(values = c("2" = "blue", "1" = "red")) +
  labs(title = "Histogram of Data from Discretized Mixture of Two Normal Distributions",
       subtitle = "Blue: Label 2, Red: Label 1, Gray: Combined",
       x = "Discrete Value",
       y = "Density",
       fill = "Label") +
  theme_minimal()

# Print the plot
print(plot)

# Save the plot to a file
ggsave("discrete_density_plot.png", plot = plot, width = 10, height = 6, dpi = 300)

# Save the data frame to a CSV file
write.csv(df, "discrete_mixture_data.csv", row.names = FALSE)
