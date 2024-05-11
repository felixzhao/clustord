library("ggplot2")

set.seed(123)  # for reproducibility

# Parameters for categories
mu <- c(0.000000000, 0.694108798, -0.615179483, 0.002634307, -0.808299781, -3.353559668)
phi <- c(0.0000000, 0.5130781, 0.8956817, 0.9862056, 0.9905825, 1.0000000)

# Cluster weights
alpha <- c(5.073905, 3.690511, -8.764416)

# Adjust alpha by subtracting the min(alpha) to make the range less extreme
alpha_adj <- alpha - min(alpha)

# Normalize adjusted alpha to get the probabilities (weights) of each cluster
alpha_normalized <- exp(alpha_adj) / sum(exp(alpha_adj))

# Assuming equal distribution of categories across the 3 clusters
cluster_categories <- split(1:length(mu), rep(1:3, each = 2))  # Split mu and phi equally across 3 clusters

# Number of samples
n <- 1000

# Initialize arrays to store samples, their corresponding cluster and category labels
samples <- numeric(n)
clusters <- numeric(n)
categories <- numeric(n)

for (i in 1:n) {
  # Select cluster based on normalized alpha
  cluster <- sample(1:length(alpha), 1, prob = alpha_normalized)
  clusters[i] <- cluster
  
  # Get category indices for the current cluster
  category_indices <- cluster_categories[[cluster]]
  
  # Compute probabilities for categories within the selected cluster
  # Normalize phi values for categories in the cluster
  if (length(category_indices) > 1) {
    category_probs <- diff(c(0, phi[category_indices]))
  } else {
    category_probs <- 1  # Only one category, probability is 1
  }
  
  # Select category based on computed probabilities
  category <- sample(category_indices, 1, prob = category_probs)
  categories[i] <- category
  
  # Generate discrete samples
  # Assume the actual value of samples are integers from 1 to 6
  # You can adjust this part to fit the actual distribution or specific values required
  samples[i] <- sample(1:6, 1, prob = rep(1/6, 6))  # Equal probability for simplicity, adjust as needed
}

# Results
data <- data.frame(Sample = samples, Cluster = as.factor(clusters), Category = categories)

# Plot
plot <- ggplot(data, aes(x = factor(Sample), fill = Cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Samples by Cluster and Sample Value",
       x = "Sample Value",
       y = "Count") +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  theme_minimal()

print(plot)

# Save the plot to a file
# ggsave("discrete_distribution_by_cluster.png", plot = plot, width = 10, height = 6, dpi = 300)

# Save the data frame to a CSV file
# write.csv(data, "discrete_mixture_data.csv", row.names = FALSE)
