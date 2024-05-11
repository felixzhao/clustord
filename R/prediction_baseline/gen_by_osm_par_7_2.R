library("ggplot2")

set.seed(123)  # for reproducibility

# Parameters for categories
mu <- c(0.000000000, 0.694108798, -0.615179483, 0.002634307, -0.808299781, -3.353559668)
phi <- c(0.0000000, 0.5130781, 0.8956817, 0.9862056, 0.9905825, 1.0000000)

# Cluster weights
alpha <- c(5.073905, 3.690511, -8.764416)

# Adjust and normalize alpha to get the probabilities (weights) of each cluster
alpha_adj <- alpha - min(alpha)
alpha_normalized <- exp(alpha_adj) / sum(exp(alpha_adj))

# Assuming equal distribution of categories across the 3 clusters
cluster_categories <- split(1:length(mu), rep(1:3, each = 2))

# Number of samples
n <- 1000

# Initialize arrays
samples <- numeric(n)
clusters <- numeric(n)
categories <- numeric(n)

# Adjust the generation of sample values based on mu
sample_values <- 1:6

for (i in 1:n) {
  # Select cluster based on normalized alpha
  cluster <- sample(1:length(alpha), 1, prob = alpha_normalized)
  clusters[i] <- cluster
  
  # Get category indices for the current cluster
  category_indices <- cluster_categories[[cluster]]
  
  # Compute probabilities for categories within the selected cluster based on mu
  category_probs <- exp(mu[category_indices]) / sum(exp(mu[category_indices]))
  
  # Select category based on computed probabilities
  category <- sample(category_indices, 1, prob = category_probs)
  categories[i] <- category
  
  # Generate sample value
  # Adjust probabilities based on mu of selected category
  value_probs <- exp(mu[category] * (1:6))  # This line adjusts the distribution within the range 1-6
  value_probs <- value_probs / sum(value_probs)  # Normalize the probabilities
  samples[i] <- sample(sample_values, 1, prob = value_probs)
}

# Resultant data frame
data <- data.frame(Sample = samples, Cluster = as.factor(clusters), Category = categories)

# Plot the histogram
plot <- ggplot(data, aes(x = factor(Sample), fill = Cluster)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Adjusted Histogram of Samples by Cluster",
       x = "Sample Value",
       y = "Count") +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  theme_minimal()

print(plot)

# Save the plot and data
