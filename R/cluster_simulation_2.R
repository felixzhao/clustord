
set.seed(123)

G=2 
q=3 
alpha=c(1,-1) 
mu=c(0, 0.6, 0.3) 
phi=c(0, 0.8, 1)
sample_size <- 1000

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
  cluster_probs[[g]] <- probs/sum(probs)
}

# sampling

data_list <- lapply(1:G, function(x) numeric(sample_size))

for (g in 1:G) {
  data_list[g] <- sample(1:q, size = 1000, replace = TRUE, prob = cluster_probs[[g]])
}

# Flatten the list of lists into a single vector
data_val <- unlist(data_list)

# Create a vector indicating the sublist number for each value
cluster_idx <- rep(1:G, each = sample_size)

# Creating a data frame for ggplot
samples <- data.frame(category = data_val, cluster = cluster_idx)

