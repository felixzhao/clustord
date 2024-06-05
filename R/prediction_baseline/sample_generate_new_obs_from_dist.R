generate_observation <- function(sample_space, probabilities, n) {
  # Ensure the probabilities sum to 1
  if (sum(probabilities) != 1) {
    stop("The sum of probabilities must be 1.")
  }
  
  # Generate n observations based on the provided sample space and probabilities
  observations <- sample(sample_space, size = n, replace = TRUE, prob = probabilities)
  
  return(observations)
}

# Example usage
sample_space <- 1:5
probabilities <- c(0.2, 0.3, 0.1, 0.25, 0.15)
n <- 100000

observations <- generate_observation(sample_space, probabilities, n)

# Create a data frame for plotting
df <- data.frame(x = factor(observations, levels = sample_space))

# Plot the bar plot
ggplot(df, aes(x = x)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = "blue", alpha = 0.7) +
  scale_x_discrete(limits = as.character(sample_space)) +
  labs(title = "Bar Plot of Generated Observations",
       x = "Sample Space",
       y = "Proportion") +
  theme_minimal()
