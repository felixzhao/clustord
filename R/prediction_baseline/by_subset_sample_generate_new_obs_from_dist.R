# Load necessary library
library(ggplot2)

# Define the function to generate observations from a specific cluster
generate_cluster_observation <- function(sample_space, probabilities, cluster_indices, n) {
  # Subset the sample space and probabilities for the specific cluster
  cluster_space <- sample_space[cluster_indices]
  cluster_probabilities <- probabilities[cluster_indices]
  
  # Normalize the cluster probabilities
  cluster_probabilities <- cluster_probabilities / sum(cluster_probabilities)
  
  # Generate n observations based on the provided cluster space and probabilities
  observations <- sample(cluster_space, size = n, replace = TRUE, prob = cluster_probabilities)
  
  return(observations)
}

# Example usage
sample_space <- 1:6
probabilities <- c(0.2, 0.3, 0.5, 0.25, 0.05, 0.5)
cluster_indices <- c(2, 5)  # Indices for cluster 2
n <- 1000

observations <- generate_cluster_observation(sample_space, probabilities, cluster_indices, n)

# Create a data frame for plotting
df <- data.frame(x = factor(observations, levels = sample_space[cluster_indices]))

# Plot the bar plot
ggplot(df, aes(x = x)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = "blue", alpha = 0.7) +
  scale_x_discrete(limits = as.character(sample_space[cluster_indices])) +
  labs(title = "Bar Plot of Generated Observations from Cluster 2",
       x = "Sample Space",
       y = "Proportion") +
  theme_minimal()
