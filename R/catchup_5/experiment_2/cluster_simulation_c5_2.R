# Load necessary library
library(ggplot2)
library(gridExtra)
library(tidyr)

# parameters
n <- 1000
n_y <- 20

# Set seed for reproducibility
set.seed(123)

# Function to generate ordinal data
generate_ordinal_data <- function(n) {
  data <- rnorm(n)
  cuts <- quantile(data, probs = c(0, 0.2, 0.7, 1))
  ordinal_data <- cut(data, breaks = cuts, labels = c(1, 2, 3), include.lowest = TRUE)
  ordinal_data <- as.numeric(as.character(ordinal_data))
  return(ordinal_data)
}

# Generate datasets
ordinal_datasets <- replicate(n_y, generate_ordinal_data(n), simplify = FALSE)

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
cluster <- c(sample(1:2, n, replace = TRUE))

df2 <- cbind(ordinal_data_frame, cluster)

# Save the dataframe to a CSV file for further use if needed
write.csv(df2, "./data/ordinal_data_frame_c5_2.csv", row.names = FALSE)
