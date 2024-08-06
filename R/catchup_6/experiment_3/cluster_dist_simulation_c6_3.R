# Load necessary library
library(ggplot2)
library(gridExtra)
library(tidyr)

# parameters
n <- 100
n_y <- 1

save_path <- paste0("./data/dist_simulation_y_", n_y, "_c6_3.csv")

# Set seed for reproducibility
set.seed(123)

# # Function to generate a vector of unique cuts starting from 0 and ending at 1
# generate_cuts <- function(n) {
#   if (n <= 2) {
#     return(sort(c(0, 1)))
#   }
#   cuts <- unique(round(runif(n - 2, min = 0, max = 1), 2))
#   while(length(cuts) < (n - 2)) {
#     cuts <- unique(c(cuts, round(runif(n - length(cuts) - 2, min = 0, max = 1), 2)))
#   }
#   cuts <- sort(c(0, cuts[1:(n - 2)], 1))
#   return(cuts)
# }

# Function to generate a vector of unique cuts from a uniform distribution
generate_cuts <- function(n) {
  cuts <- sort(runif(n - 2, min = 0, max = 1))
  cuts <- unique(c(0, cuts, 1)) # Ensure cuts start from 0 and end at 1 and are unique
  while(length(cuts) < n) {
    # If the length of cuts is less than n, generate a new cut and add it
    new_cut <- runif(1, min = 0, max = 1)
    cuts <- sort(unique(c(cuts, new_cut)))
  }
  return(round(cuts, 2))
}

# generate cuts prob vectors
list_of_cuts <- lapply(1:20, function(x) generate_cuts(4))



# Function to generate ordinal data
generate_ordinal_data <- function(n, mean, var, probs) {
  print(probs)
  print(mean)
  print(var)
  data <- rnorm(n, mean, var)
  print(min(data))
  print(max(data))
  cuts <- quantile(data, probs = probs)
  print(cuts)
  ordinal_data <- cut(data, breaks = cuts, labels = c(1, 2, 3), include.lowest = TRUE)
  ordinal_data <- as.numeric(as.character(ordinal_data))
  return(ordinal_data)
}

# Generate datasets

## Generate Cluster
generate_cluster_data <- function(n, mean, var, list_of_probs, cluster_idx) {
  ordinal_datasets <- lapply(1:n_y, function(i) {
    prob_vector <- list_of_probs[[i]]
    generate_ordinal_data(n, mean, var, prob_vector)
  }
  )
  
  # Create a data frame
  ordinal_data_frame <- do.call(cbind, ordinal_datasets)
  colnames(ordinal_data_frame) <- paste0("Y", 1:n_y)
  
  # Print summary of the generated data frame
  print(summary(ordinal_data_frame))
  
  # Add random clusters
  cluster <- cluster_idx
  
  df2 <- cbind(ordinal_data_frame, cluster)
  return(df2)
}

## Plot Cluster
plot_cluster <- function(df){
  # Reshape the data to long format using pivot_longer
  ordinal_data_long <- pivot_longer(as.data.frame(df[, 1:1]), 
                                    cols = everything(), 
                                    names_to = "Variable", 
                                    values_to = "Value")
  
  # Plot density for the first 10 columns
  p <- ggplot(ordinal_data_long, aes(x = Value)) +
    geom_density(aes(fill = Variable), alpha = 0.3) +
    labs(title = "Density Plots of the First 10 Columns", x = "Value", y = "Density") +
    theme_minimal() +
    theme(legend.position = "none") +
    facet_wrap(~ Variable, ncol = 3)
  
  print(p)
}

# Generate 
# cluster 1
mean_cluster_1 <- 0
var_cluster_1 <- 1
n_cluster_1 <- n / 2
cluster_1_idx <- 1
df_cluster1 <- generate_cluster_data(n_cluster_1, mean_cluster_1, var_cluster_1, list_of_cuts, cluster_1_idx)
plot_cluster(df_cluster1)

# cluster 2
mean_cluster_2 <- 30
var_cluster_2 <- 60
n_cluster_2 <- n / 2
cluster_2_idx <- 2
df_cluster2 <- generate_cluster_data(n_cluster_2, mean_cluster_2, var_cluster_2, list_of_cuts, cluster_2_idx)
plot_cluster(df_cluster2)

# Save the dataframe to a CSV file for further use if needed
#write.csv(df2, save_path, row.names = FALSE)
