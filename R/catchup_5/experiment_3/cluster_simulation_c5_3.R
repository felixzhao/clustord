# Load necessary library
library(ggplot2)
library(gridExtra)

# parameters
n <- 1000
n_y <- 20

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

# Generate datasets
ordinal_datasets <- replicate(n_y, generate_ordinal_data(n), simplify = FALSE)

# Create a data frame
ordinal_data_frame <- do.call(cbind, ordinal_datasets)
colnames(ordinal_data_frame) <- paste0("Y", 1:n_y)

# Print summary of the generated data frame
print(summary(ordinal_data_frame))

# Plot the first 10 datasets in a 3x4 grid of density plots
plots <- list()
for (i in 1:10) {
  data <- data.frame(values = rnorm(n))
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
  
  plots[[i]] <- p
}

# Arrange the plots in a 3x4 grid
grid.arrange(grobs = plots, ncol = 4)

# Add random clusters
cluster <- c(sample(1:2, n, replace = TRUE))

df2 <- cbind(ordinal_data_frame, cluster)

# Save the dataframe to a CSV file for further use if needed
write.csv(df2, "./data/ordinal_data_frame.csv", row.names = FALSE)
