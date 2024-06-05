library(ggplot2)
set.seed(123)
data <- data.frame(
  x = 1:20,
  y1 = runif(20),
  y2 = runif(20)
)

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Reshape the data to long format
data_long <- melt(data, id.vars = "x")

# Create the plot
ggplot(data_long, aes(x = x, y = value, fill = variable, color = variable)) +
  geom_area(position = "identity", alpha = 0.4) +
  geom_line(size = 1) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = "", color = "")

# DEMO 2

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Sample data frame
set.seed(123)
df <- data.frame(
  category = sample(1:3, 100, replace = TRUE),
  cluster = sample(1:2, 100, replace = TRUE)
)

# Calculate the proportions
proportion_data <- df %>%
  group_by(cluster, category) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(cluster) %>%
  mutate(proportion = count / sum(count))

# Print the proportion data
print(proportion_data)

# Plot with geom_bar
ggplot(proportion_data, aes(x = factor(category), y = proportion, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Proportion of Categories by Cluster",
       x = "Category",
       y = "Proportion",
       fill = "Cluster") +
  theme_minimal()

