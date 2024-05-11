# install.packages("dplyr")
# Load necessary library
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Define parameters
mu <- c(0.000000000, 0.694108798 -0.615179483, 0.002634307, -0.808299781, -3.353559668)  # example intercepts for each category
phi <- c(0.0000000, 0.5130781, 0.8956817, 0.9862056, 0.9905825, 1.0000000)  # ordered constraint for phi
alpha <- c(5.073905, 3.690511, -8.764416)         # effect of each cluster

# Define the number of samples per cluster
n <- 100

# Create a data frame for the clusters
data <- expand.grid(cluster = factor(1:3),
                    category = factor(1:5),
                    replicate = 1:n)

# Calculate log-odds
data$log_odds <- with(data, mu[as.numeric(category)] + phi[as.numeric(category)] * alpha[as.numeric(cluster)])

# Convert log-odds to probabilities using softmax function
data$prob <- with(data, exp(log_odds) / ave(exp(log_odds), cluster, FUN = sum))

# Sample from the multinomial distribution
set.seed(123)
data$sample <- with(data, rbinom(n * 5 * 3, 1, prob))

# Reshape data to have one row per draw
# final_data <- data %>%
#   filter(sample == 1) %>%
#   select(cluster, category)

# View the sample distribution
# table(final_data$cluster, final_data$category)

# Assuming 'data' dataframe has been prepared as per your earlier specifications

library(ggplot2)
library(dplyr)

# Assuming 'data' dataframe has been prepared as per your earlier specifications

# Convert factors to numeric if needed
data$category <- as.numeric(as.character(data$category))

# Plot both individual densities and overall density
plot <- ggplot(data, aes(x = category, color = as.factor(cluster))) +
  geom_density(aes(y = ..density..), adjust = 1.5, alpha = 0.6) +  # Individual cluster densities
  geom_density(data = data, aes(x = category, y = ..density.., color = "Overall"), adjust = 1.5, alpha = 0.2) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "Overall" = "black")) +
  labs(title = "Density Plot of Categorical Data by Cluster",
       subtitle = "Individual and Combined Densities",
       x = "Category",
       y = "Density",
       color = "Cluster") +
  theme_minimal()

# Print the plot
print(plot)

####

library(ggplot2)
library(dplyr)

# Check unique values in clusters
print(unique(data$cluster))

# Convert categories and clusters to factors to ensure correct handling
data$category <- as.factor(data$category)
data$cluster <- as.factor(data$cluster)

# Plot both individual densities and overall density
plot <- ggplot(data, aes(x = as.numeric(as.character(category)), group = cluster, color = cluster)) +
  geom_density(aes(y = ..density..), adjust = 1.5, alpha = 0.6) +
  geom_density(data = data, aes(y = ..density.., color = "Overall"), adjust = 1.5, alpha = 0.2) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "Overall" = "black")) +
  labs(title = "Density Plot of Categorical Data by Cluster",
       subtitle = "Individual and Combined Densities",
       x = "Category",
       y = "Density",
       color = "Cluster") +
  theme_minimal()

# Print the plot
print(plot)


#### 1

library(dplyr)

# Assuming 'data' is your original dataframe
filtered_data <- data %>%
  filter(cluster == 1)

# You can check the contents of filtered_data to ensure it's what you expect
print(head(filtered_data))


library(ggplot2)

# Plot the density for filtered data
plot <- ggplot(filtered_data, aes(x = as.numeric(as.character(category)), fill = cluster)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  labs(title = "Density Plot for Cluster 1",
       subtitle = "Visualizing Distribution of Category",
       x = "Category",
       y = "Density") +
  theme_minimal()

# Print the plot
print(plot)


#### 2

library(dplyr)

# Assuming 'data' is your original dataframe
filtered_data <- data %>%
  filter(cluster == 2)

# You can check the contents of filtered_data to ensure it's what you expect
print(head(filtered_data))


library(ggplot2)

# Plot the density for filtered data
plot <- ggplot(filtered_data, aes(x = as.numeric(as.character(category)), fill = cluster)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  labs(title = "Density Plot for Cluster 1",
       subtitle = "Visualizing Distribution of Category",
       x = "Category",
       y = "Density") +
  theme_minimal()

# Print the plot
print(plot)
