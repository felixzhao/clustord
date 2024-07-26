# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate 1000 data points from a normal distribution
data <- rnorm(1000)

# Convert the data into a data frame
data_frame <- data.frame(values = data)

# Plot the data using ggplot2
ggplot(data_frame, aes(x = values)) +
  geom_histogram(binwidth = 0.2, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Histogram of Normally Distributed Data",
       x = "Values",
       y = "Frequency") +
  theme_minimal()

# Alternatively, you can use a density plot
ggplot(data_frame, aes(x = values)) +
  geom_density(fill = "blue", alpha = 0.7) +
  labs(title = "Density Plot of Normally Distributed Data",
       x = "Values",
       y = "Density") +
  theme_minimal()


# ordinal data

# Define the cut points to split the data into 3 parts based on quantiles
cuts <- quantile(data, probs = seq(0, 1, length.out = 4))

# Convert the continuous data into ordinal data
ordinal_data <- cut(data, breaks = cuts, labels = c(0, 1, 2), include.lowest = TRUE)

# Convert to a numeric vector
ordinal_data <- as.numeric(as.character(ordinal_data))

# Print summary of the ordinal data
summary(ordinal_data)

# Visualize the ordinal data
data_frame <- data.frame(values = data, ordinal = factor(ordinal_data, levels = c(0, 1, 2)))
ggplot(data_frame, aes(x = values, fill = ordinal)) +
  geom_histogram(binwidth = 0.2, color = "black", alpha = 0.7) +
  labs(title = "Histogram of Normally Distributed Data with Ordinal Labels",
       x = "Values",
       y = "Frequency") +
  scale_fill_manual(values = c("0" = "blue", "1" = "green", "2" = "red"),
                    name = "Ordinal Levels") +
  theme_minimal()

# Alternatively, you can use a density plot
ggplot(data_frame, aes(x = values, fill = ordinal)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Normally Distributed Data with Ordinal Labels",
       x = "Values",
       y = "Density") +
  scale_fill_manual(values = c("0" = "blue", "1" = "green", "2" = "red"),
                    name = "Ordinal Levels") +
  theme_minimal()