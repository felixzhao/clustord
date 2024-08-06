# Load necessary library
library(ggplot2)
library(gridExtra)
library(tidyr)

# parameters
n <- 1000
n_y <- 20

save_path <- paste0("./data/dist_simulation_y_", n_y, "_c6_3.csv")

# Set seed for reproducibility
set.seed(123)

# Function to generate a vector of unique cuts starting from 0 and ending at 1
generate_cuts <- function(n) {
  if (n <= 2) {
    return(sort(c(0, 1)))
  }
  cuts <- unique(round(runif(n - 2, min = 0, max = 1), 2))
  while(length(cuts) < (n - 2)) {
    cuts <- unique(c(cuts, round(runif(n - length(cuts) - 2, min = 0, max = 1), 2)))
  }
  cuts <- sort(c(0, cuts[1:(n - 2)], 1))
  return(cuts)
}

# generate cuts prob vectors
list_of_vectors <- lapply(1:20, function(x) generate_cuts(4))

# cluster 1
mean_cluster_1 <- 0
var_cluster_1 <- 1
n_cluster_1 <- n / 2

# Function to generate ordinal data
generate_ordinal_data <- function(n, mean, var, probs) {
  print(probs)
  data <- rnorm(n, mean, var)
  cuts <- quantile(data, probs = probs)
  print(cuts)
  ordinal_data <- cut(data, breaks = cuts, labels = c(1, 2, 3), include.lowest = TRUE)
  ordinal_data <- as.numeric(as.character(ordinal_data))
  return(ordinal_data)
}

# Generate datasets
ordinal_datasets <- lapply(1:n_y, function(i) {
  prob_vector <- list_of_vectors[[i]]
  generate_ordinal_data(n_cluster_1, mean_cluster_1, var_cluster_1, prob_vector)
}
  )

# Create a data frame
ordinal_data_frame <- do.call(cbind, ordinal_datasets)
colnames(ordinal_data_frame) <- paste0("Y", 1:n_y)

# Print summary of the generated data frame
print(summary(ordinal_data_frame))

# Reshape the data to long format using pivot_longer
ordinal_data_long <- pivot_longer(as.data.frame(ordinal_data_frame[, 1:10]), 
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

# Add random clusters
cluster <- 1

df2 <- cbind(ordinal_data_frame, cluster)

# Save the dataframe to a CSV file for further use if needed
#write.csv(df2, save_path, row.names = FALSE)
