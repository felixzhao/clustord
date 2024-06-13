library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)

set.seed(123)

G=3 
q=5 
alpha=c(1,0.7, -1) 
mu=c(0, 0.6, 0.3, 0.7, 0.1) 
phi=c(0, 0.3, 0.5, 0.8, 1)
cluster_pi = c(0.3, 0.5, 0.2)
sample_size <- 1000
total_sample_size <- sample_size * G

number_of_y = 10

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

# normalize

# Adjust each cluster's probabilities by multiplying with corresponding pi
adjusted_probs <- mapply(function(cluster, p) {
  cluster * p
}, cluster_probs, cluster_pi, SIMPLIFY = FALSE)

normalized_cluster_probs <- adjusted_probs

normalized_cluster_probs

# sampling

y_sampling <- function(sample_size, total_sample_size, cluster_pi, q, normalized_cluster_probs, y_idx) {
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
  samples <- samples %>% mutate(id = row_number()) # Add a unique identifier
  names(samples)[1] <- paste0("Y", y_idx)
  return(samples)
}

# Create a list of N dataframes
dataframes <- lapply(1:number_of_y, function(i) y_sampling(sample_size, total_sample_size, cluster_pi, q, normalized_cluster_probs, i))

# Merge all dataframes
merged_df <- reduce(dataframes, function(df1, df2) {
  inner_join(df1, df2, by = c("cluster", "id"))
})

# Remove the unique identifier column
merged_df <- merged_df %>% select(-id)

# Display the merged dataframe
# print(merged_df)


# save to csv file
write.csv(merged_df, "./data/simulation_catgories_n_cluster_c3_2.csv", row.names=FALSE)


# Plot
plot_y <- function(df, y_idx) {
  sample_name <- paste0("Y", y_idx)
  # Plot
  data1 <- data.frame(Sample = df[[sample_name]], Cluster = as.factor(df$cluster))
  
  plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Samples by Cluster",
         x = "Sample Value",
         y = "Density") +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    theme_minimal()
}

plots <- list()
for (i in 1:number_of_y){
  plots[[i]] <- plot_y(merged_df, i)
}

# Calculate number of rows and columns dynamically
ncol <- 3  # Number of columns
nrow <- ceiling(number_of_y / ncol)  # Number of rows

# Ensure the grid has enough cells
stopifnot(nrow * ncol >= length(plots))

# Arrange the plots dynamically and add a title
grid.arrange(
  grobs = plots,
  ncol = ncol,
  nrow = nrow,
  top = textGrob("Density Plots of Categories", gp = gpar(fontsize = 16, fontface = "bold"))
)
