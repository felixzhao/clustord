# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define parameters
mu <- c(-0.5, 0, 0.5, 1.0, 1.5)   # example intercepts for each category
phi <- c(0, 0.25, 0.5, 0.75, 1)   # ordered constraint for phi

# Different effects for each cluster
alpha_1 <- c(-0.3, 0, 0.3)        # effect for cluster 1
alpha_2 <- c(0.3, -0.1, -0.2)     # effect for cluster 2
alpha_3 <- c(0.1, 0.2, 0.3)       # effect for cluster 3

# Define the number of samples per cluster
n <- 100

# Create a data frame for the clusters
data <- expand.grid(cluster = factor(1:3),
                    category = factor(1:5),
                    replicate = 1:n)

# Map alphas to clusters
data$alpha <- 0
data$alpha[data$cluster == 1] <- rep(alpha_1, each = 500)
data$alpha[data$cluster == 2] <- rep(alpha_2, each = 500)
data$alpha[data$cluster == 3] <- rep(alpha_3, each = 500)

# Calculate log-odds
data$log_odds <- with(data, mu[as.numeric(category)] + phi[as.numeric(category)] * alpha)

# Convert log-odds to probabilities using softmax function
data$prob <- with(data, exp(log_odds) / ave(exp(log_odds), list(cluster, replicate), FUN = sum))

# Sample from the multinomial distribution
set.seed(123)
data$sample <- with(data, rbinom(n * 5 * 3, 1, prob))

# Reshape data to have one row per draw
final_data <- data %>%
  filter(sample == 1) %>%
  select(cluster, category)

# View the sample distribution
table(final_data$cluster, final_data$category)



#### 1

library(dplyr)

# Assuming 'data' is your original dataframe
filtered_data <- data %>%
  filter(cluster == 2)

# You can check the contents of filtered_data to ensure it's what you expect
print(head(filtered_data))


library(ggplot2)

# Plot the density for filtered data
plot <- ggplot(filtered_data, aes(x = as.numeric(as.character(category)), fill = cluster)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  labs(title = "Density Plot for Cluster 1",
       subtitle = "Visualizing Distribution of Category",
       x = "Category",
       y = "Density") +
  theme_minimal()

# Print the plot
print(plot)
