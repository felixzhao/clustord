# Load necessary library
library(ggplot2)

# Set the number of observations
n <- 1000  # Total number of data points

# Proportions from each distribution
p1 <- 0.6  # 60% from N(2, 0.5)
p2 <- 0.4  # 40% from N(1, 0.5)

# Generate a random sample of labels according to the proportions
labels <- sample(c("N(2, 0.5)", "N(1, 0.5)"), size = n, replace = TRUE, prob = c(p1, p2))

# Generate data based on the labels
data <- ifelse(labels == "N(2, 0.5)", rnorm(sum(labels == "N(2, 0.5)"), mean = 4, sd = sqrt(0.5)),
               rnorm(sum(labels == "N(1, 0.5)"), mean = 1, sd = sqrt(0.5)))

# Create a data frame to store the data and labels
df <- data.frame(value = data, label = labels)

# Plot both individual densities and overall density
plot <- ggplot(df, aes(x = value)) +
  geom_density(aes(fill = label), alpha = 0.6) +  # Individual densities
  geom_density(data = df, aes(x = value, y = ..scaled..), fill = "gray", alpha = 0.3) +  # Overall density scaled
  scale_fill_manual(values = c("N(2, 0.5)" = "blue", "N(1, 0.5)" = "red")) +
  labs(title = "Density Plot of Data from Mixture of Two Normal Distributions",
       subtitle = "Blue: N(2, 0.5), Red: N(1, 0.5), Gray: Combined",
       x = "Value",
       y = "Density",
       fill = "Distribution") +
  theme_minimal()

# Print the plot
print(plot)

# Save the plot to a file
ggsave("density_plot.png", plot = plot, width = 10, height = 6, dpi = 300)

# Save the data frame to a CSV file
write.csv(df, "mixture_data.csv", row.names = FALSE)