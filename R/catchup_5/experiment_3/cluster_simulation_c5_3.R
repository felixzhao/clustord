# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Function to generate ordinal data
generate_ordinal_data <- function(n) {
  data <- rnorm(n)
  cuts <- quantile(data, probs = seq(0, 1, length.out = 4))
  ordinal_data <- cut(data, breaks = cuts, labels = c(0, 1, 2), include.lowest = TRUE)
  ordinal_data <- as.numeric(as.character(ordinal_data))
  return(ordinal_data)
}

# Generate 20 datasets
ordinal_datasets <- replicate(20, generate_ordinal_data(1000), simplify = FALSE)

# Create a data frame
ordinal_data_frame <- do.call(cbind, ordinal_datasets)
colnames(ordinal_data_frame) <- paste0("Dataset_", 1:20)

# Print summary of the generated data frame
print(summary(ordinal_data_frame))

# Plot the first 10 datasets in a 3x4 grid of density plots
par(mfrow = c(3, 4))
for (i in 1:10) {
  data <- data.frame(values = rnorm(1000))
  cuts <- quantile(data$values, probs = seq(0, 1, length.out = 4))
  data$ordinal <- cut(data$values, breaks = cuts, labels = c(0, 1, 2), include.lowest = TRUE)
  data$ordinal <- as.numeric(as.character(data$ordinal))
  data_frame <- data.frame(values = data$values, ordinal = factor(data$ordinal, levels = c(0, 1, 2)))
  
  p <- ggplot(data_frame, aes(x = values, fill = ordinal)) +
    geom_density(alpha = 0.7) +
    labs(title = paste("Density Plot of Dataset", i),
         x = "Values",
         y = "Density") +
    scale_fill_manual(values = c("0" = "blue", "1" = "green", "2" = "red"),
                      name = "Ordinal Levels") +
    theme_minimal()
  
  print(p)
}
par(mfrow = c(1, 1))
