
set.seed(123)

G=2 
q=3 
alpha=c(1,-1) 
mu=c(0, 0.6, 0.3) 
phi=c(0, 0.8, 1)
cluster_pi = c(0.3, 0.7)
sample_size <- 1000
total_sample_size <- sample_size * G


cluster_probs <- lapply(1:G, function(x) numeric(q))

for (g in 1:G) {
  probs <- numeric(q)
  for (k in 1:q) {
    linear <- mu[k] + phi[k] * alpha[g]
    if (k > 1) {
      probs[k] <- exp(linear)
    } else {
      probs[k] <- 1
    }
  }
  cluster_probs[[g]] <- probs #/sum(probs)
}

# normalize

# Adjust each cluster's probabilities by multiplying with corresponding pi
adjusted_probs <- mapply(function(cluster, p) {
  cluster * p
}, cluster_probs, cluster_pi, SIMPLIFY = FALSE)

# Flatten the list to calculate the global sum
all_probs <- unlist(adjusted_probs)
total_sum <- sum(all_probs)

# Normalize each cluster by the global sum
normalized_cluster_probs <- lapply(adjusted_probs, function(cluster) {
  cluster / total_sum
})

normalized_cluster_probs

# sampling

data_list <- lapply(1:G, function(x) numeric(sample_size))

for (g in 1:G) {
  cluster_sample_size <- total_sample_size * cluster_pi[g]
  data_list[[g]] <- sample(1:q, size = cluster_sample_size, replace = TRUE, prob = normalized_cluster_probs[[g]])
}

# Flatten the list of lists into a single vector
data_val <- unlist(data_list)

# Create a vector indicating the sublist number for each value
cluster_idx <- unlist(lapply(1:G, function(g) rep(g, length(data_list[[g]]))))

# Creating a data frame for ggplot
samples <- data.frame(category = data_val, cluster = cluster_idx)

# save to csv file
write.csv(samples, "./data/simulation_catgories_n_cluster_2.csv", row.names=FALSE)

# Plotting the density plot
p1 <- ggplot(samples, aes(x = category)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Categories",
       x = "Categories",
       y = "Density") +
  theme_minimal()

p2 <- ggplot(samples, aes(x = cluster)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Clusters",
       x = "Clusters",
       y = "Density") +
  theme_minimal()

# plot
# grid.arrange(p1, p2, ncol = 2)


# Create the bar plot
p3 <- ggplot(samples, aes(x = factor(category), fill = factor(cluster))) +
  geom_bar(position = "dodge") +
  labs(x = "Category", y = "Count", fill = "Cluster") +
  theme_minimal() +
  ggtitle("Bar Plot of Categories Grouped by Cluster")

p4 <- ggplot(samples, aes(x = factor(cluster), fill = factor(category))) +
  geom_bar(position = "dodge") +
  labs(x = "Cluster", y = "Count", fill = "Category") +
  theme_minimal() +
  ggtitle("Bar Plot of Clusters Grouped by Category")


# Create the stacked column chart
p5 <- ggplot(samples, aes(x = factor(category), fill = factor(cluster))) +
  geom_bar(stat = "count", position = "stack") +
  labs(x = "Category", y = "Count", fill = "Cluster") +
  theme_minimal() +
  ggtitle("Stacked Column Chart of Categories by Cluster")

p6 <- ggplot(samples, aes(x = factor(cluster), fill = factor(category))) +
  geom_bar(stat = "count", position = "stack") +
  labs(x = "Cluster", y = "Count", fill = "Category") +
  theme_minimal() +
  ggtitle("Stacked Column Chart of Clusters by Category")

# plot
# grid.arrange(p3, p4, p5, p6, ncol = 2, nrow = 2)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

