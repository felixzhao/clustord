# Load necessary library
library(ggplot2)
library(gridExtra)
library(tidyr)

# parameters
n <- 1000
n_y <- 20

cut_1 <- -0.5
cut_2 <- 0.5

save_path <- paste0("./data/dist_simulation_y_", n_y, "_c6_3_3.csv")

# Set seed for reproducibility
set.seed(123)

# Function to generate ordinal data
generate_ordinal_data <- function(n, mean, var) {
  print(mean)
  print(var)
  data <- rnorm(n,  mean = mean, sd = sqrt(var))
  print(min(data))
  print(max(data))
  ordinal_data <- cut(data, breaks = c(-Inf, cut_1, cut_2, Inf), labels = c(1, 2, 3), include.lowest = TRUE)
  ordinal_data <- as.numeric(as.character(ordinal_data))
  return(ordinal_data)
}

# Generate datasets

## Generate Cluster
generate_cluster_data <- function(n_y, n, mean, var, cluster_idx) {
  ordinal_datasets <- replicate(n_y, generate_ordinal_data(n, mean, var), simplify = FALSE)
  
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

## Plot dataframe
plot_df <- function(df){
  # Reshape the data to long format using pivot_longer
  ordinal_data_long <- pivot_longer(as.data.frame(df[, 1:10]), 
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
df_cluster1 <- generate_cluster_data(n_y, n_cluster_1, mean_cluster_1, var_cluster_1, cluster_1_idx)
plot_df(df_cluster1)

# cluster 2
mean_cluster_2 <- 0
var_cluster_2 <- 1
n_cluster_2 <- n / 2
cluster_2_idx <- 2
df_cluster2 <- generate_cluster_data(n_y, n_cluster_2, mean_cluster_2, var_cluster_2, cluster_2_idx)
plot_df(df_cluster2)

combined_df <- rbind(df_cluster1, df_cluster2)
print(summary(combined_df))
print(head(combined_df, 5))
print(tail(combined_df, 5))
plot_df(combined_df)

# Save the dataframe to a CSV file for further use if needed
write.csv(combined_df, save_path, row.names = FALSE)
