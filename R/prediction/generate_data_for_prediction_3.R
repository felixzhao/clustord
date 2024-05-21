# Load necessary library
library(ggplot2)

# Set the number of observations
n <- 1000  # Total number of data points

# Proportions from each distribution
p1 <- 0.6  # 60% from N(2, 0.5)
p2 <- 0.4  # 40% from N(1, 0.5)

# Generate data based on the labels
data <- ifelse(labels == "N(2, 0.5)", rnorm(sum(labels == "N(2, 0.5)"), mean = 4, sd = sqrt(0.5)),
               rnorm(sum(labels == "N(1, 0.5)"), mean = 1, sd = sqrt(0.5)))

quantile(data, probs = c(0.25, 0.50, 0.75))

# Calculate the 25th, 50th, and 75th percentiles
percentiles <- quantile(data, probs = c(0.25, 0.50, 0.75))

# Add 0 and 1 to the percentiles to cover the whole range
breaks <- c(-Inf, percentiles, Inf)

# Define the labels
data_labels <- c(1, 2, 3, 4)

# Use the cut() function to label the data
labels <- cut(data, breaks = breaks, labels = labels, include.lowest = TRUE)


# Create a data frame to store the data and labels
df <- data.frame(value = data, label = labels)

# Plot density grouped by label
ggplot(df, aes(x = value, fill = label)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Data Grouped by Label",
       x = "Value",
       y = "Density",
       fill = "Label") +
  theme_minimal()

# Print the plot
print(plot)

# Save the plot to a file
ggsave("density_plot.png", plot = plot, width = 10, height = 6, dpi = 300)

# Save the data frame to a CSV file
write.csv(df, "mixture_data.csv", row.names = FALSE)