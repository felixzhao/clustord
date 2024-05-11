set.seed(123)  # for reproducibility

# Parameters
mu <- c(0.000000000, 0.694108798, -0.615179483, 0.002634307, -0.808299781, -3.353559668)
phi <- c(0.0000000, 0.5130781, 0.8956817, 0.9862056, 0.9905825, 1.0000000)
sd <- rep(1, length(mu))  # Assuming a standard deviation of 1 for each component

# Compute the probabilities from phi
pi <- diff(c(0, phi))  # Adding 0 at the beginning for the correct length

# Number of samples
n <- 1000

# Initialize arrays to store samples and their corresponding cluster labels
samples <- numeric(n)
clusters <- numeric(n)

for (i in 1:n) {
  # Select component based on pi
  component <- sample(1:length(mu), 1, prob = pi)
  # Draw from the normal distribution of the selected component
  samples[i] <- rnorm(1, mean = mu[component], sd = sd[component])
  # Record the component from which the sample was drawn
  clusters[i] <- component
}

print(unique(clusters))

# Results
data.frame(Sample = samples, Cluster = clusters)

# You can plot the histogram of the samples to visualize the distribution
hist(samples, breaks = 30, main = "Histogram of Samples from Mixture Distribution")


# Create a data frame
data <- data.frame(Sample = samples, Cluster = as.factor(clusters))

# Plot
ggplot(data, aes(x = Sample, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Samples by Cluster",
       x = "Sample Value",
       y = "Density") +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  theme_minimal()
