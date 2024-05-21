# Load necessary library
library(ggplot2)

# Set the number of observations
n <- 1000  # Total number of data points

# Proportions from each distribution
p1 <- 0.6  # 60% from N(2, 0.5)
p2 <- 0.4  # 40% from N(1, 0.5)

# Generate labels based on proportions
labels <- c(rep("N(2, 0.5)", n * p1), rep("N(1, 0.5)", n * p2))

# Shuffle the labels to mix them
set.seed(123)  # For reproducibility
labels <- sample(labels)

# Generate data based on the labels
data <- ifelse(labels == "N(2, 0.5)", rnorm(sum(labels == "N(2, 0.5)"), mean = 2, sd = sqrt(0.5)),
               rnorm(sum(labels == "N(1, 0.5)"), mean = 1, sd = sqrt(0.5)))

# Calculate the 25th, 50th, and 75th percentiles
percentiles <- quantile(data, probs = c(0.25, 0.50, 0.75))

# Add -Inf and Inf to the percentiles to cover the whole range
breaks <- c(-Inf, percentiles, Inf)

# Define the labels for the percentiles
data_labels <- c(1, 2, 3, 4)

# Use the cut() function to label the data
labels <- cut(data, breaks = breaks, labels = data_labels, include.lowest = TRUE)

# Create a data frame to store the data and labels
df <- data.frame(value = data, label = labels)

# Plot individual densities and overall density by label
ggplot(df, aes(x = value, fill = label)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of Data by Label",
       x = "Value",
       y = "Density",
       fill = "Label") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green", "purple")) +
  geom_density(aes(y = ..density..), color = "black", size = 1, adjust = 1.5)
