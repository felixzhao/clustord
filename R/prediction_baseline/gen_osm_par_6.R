# Parameters
mu <- c(0.000000000, 0.694108798, -0.615179483, 0.002634307, -0.808299781, -3.353559668)
phi <- c(0.0000000, 0.5130781, 0.8956817, 0.9862056, 0.9905825, 1.0000000)
alpha <- c(5.073905, 3.690511, -8.764416)

# Convert phi to probabilities for each category
p <- diff(c(0, phi))

# Compute cluster probabilities using logistic transformation of alpha
p_cluster <- exp(alpha) / (1 + exp(alpha))
p_cluster <- p_cluster / sum(p_cluster)  # normalize to sum to 1

# Number of samples
n_samples <- 1000

# Generate samples for categories and clusters
set.seed(123)
category_samples <- sample(1:6, n_samples, replace = TRUE, prob = p)
cluster_samples <- sample(1:3, n_samples, replace = TRUE, prob = p_cluster)

# Output some of the generated samples
data.frame(Category = category_samples, Cluster = cluster_samples)

# Create a data frame
data1 <- data.frame(Sample = category_samples, Cluster = as.factor(cluster_samples))

# Plot
ggplot(data1, aes(x = Sample, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Samples by Cluster",
       x = "Sample Value",
       y = "Density") +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  theme_minimal()

# Print the plot
print(plot)

# Save the plot to a file
ggsave("regenerate_from_para_discrete_density_plot.png", plot = plot, width = 10, height = 6, dpi = 300)

# Save the data frame to a CSV file
write.csv(df, "regenerate_from_para_discrete_mixture_data.csv", row.names = FALSE)
