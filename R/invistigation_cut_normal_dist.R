
# Load necessary library
library(dplyr)

# Set the seed for reproducibility
set.seed(123)

# Generate continuous data from N(0, 6)
n <- 1000 # Number of observations
# data <- rnorm(n, mean = 0, sd = sqrt(6))
data <- rnorm(n, mean = 3, sd = sqrt(8))

# Cut the data into ordinal categories based on specified cut-off values
ordinal_data <- cut(data, breaks = c(-Inf, 0, 2.5, Inf), labels = c("Low", "Medium", "High"))

# Show the first few rows of the ordinal data
head(ordinal_data)

# Optionally, you can create a data frame to combine the continuous and ordinal data
df <- data.frame(Continuous = data, Ordinal = ordinal_data)

# Display the first few rows of the data frame
head(df)

summary(df)
