# install.packages("caret")

# Load necessary library
library("ggplot2")
library("clustord")
library(caret)


# Load the data from the CSV file
df <- read.csv("./data/simulation_catgories_n_cluster_2.csv", stringsAsFactors = FALSE)

# Check the structure of the loaded data
str(df)

# View the first few rows of the data
head(df)


##

data1 <- data.frame(Sample = df$category, Cluster = as.factor(df$cluster))

# Plot
plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Samples by Cluster",
       x = "Sample Value",
       y = "Density") +
  scale_fill_brewer(palette = "Set1", name = "Cluster") +
  theme_minimal()

# Print the plot
print(plot)

###


# Assume df is your original dataframe
set.seed(123)  # for reproducibility
indexes <- sample(1:nrow(df), size = 0.7 * nrow(df))

train_df <- df[indexes, ]
test_df <- df[-indexes, ]




train_clust_df <- mat2df(mat = train_df["category"])

str(train_clust_df)

# test no need mat2df
# test_clust_df <- mat2df(mat = test_df["category"])

# str(test_clust_df)


##



###


# training
# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 3 row clustering groups:
results <- clustord(Y~ROWCLUST,model="OSM",2,long.df=train_clust_df, EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)

cat("parlist.out")
print(results$parlist.out)

cat("results$pi.out")
print(results$pi.out)


# prediction

predict_osm_category <- function(k, mu, phi, alpha, pi) {
  if (k > length(mu) || k < 1) {
    stop("k is out of bounds for the length of mu")
  }
  
  # Adjusted to correctly use the first value of mu and phi as 0
  mu_k <- mu[k]
  phi_k <- phi[k]
  RG <- length(alpha)
  
  # The first value for each is always 0, so start with that as a base
  probabilities <- rep(0.01, RG)  # Initialize probabilities with 0
  
  # Compute the probabilities for each group
  for (g in 1:RG) {
    probabilities[g] <- exp(mu_k + phi_k * alpha[g])
  }
  
  cluster_probs <-  probabilities * pi
  print(paste("cluster_probs", cluster_probs))
  
  total_cluster_probs <- sum(probabilities * pi)
  print(paste("total_cluster_probs", total_cluster_probs))
  
  # Normalize probabilities so they sum to 1
  probabilities <- cluster_probs / total_cluster_probs
  print(paste("probabilities", probabilities))
  
  # Return the group with the highest probability
  predicted_group <- which.max(probabilities)
  
  return(probabilities)
}
parlist <- results$parlist.out
pi <- results$pi.out
mu <- parlist$mu 
phi <- parlist$phi
alpha <- parlist$rowc
print(mu)
print(phi)
print(alpha)
print(pi)



new_obs_predictors <- test_df[1,]
new_obs_predictors <- as.numeric(as.character(test_df[1,]))
print(new_obs_predictors)
print(new_obs_predictors[1])
probs <- predict_osm_category(new_obs_predictors[1], mu, phi, alpha, pi)
print(probs)

# print("estimate")
# print(which.max(probs))
# print(test_df[1,])


actual <- test_df[,2]
predicted <- c()

for (i in 1:nrow(test_df)){
    new_obs_predictors <- as.numeric(as.character(test_df[i,]))
    probs <- predict_osm_category(new_obs_predictors[1], mu, phi, alpha,pi)
    predict_cluster <- which.max(probs)
    # print(paste("i",i,"estimate", which.max(probs), "actual:", test_df[i,][2]))
    predicted[i] <- predict_cluster
}

# Accuracy

# Calculate accuracy
accuracy <- sum(actual == predicted) / length(actual)

# Print the accuracy
print(paste("Accuracy:", accuracy))


# confusion matrix

# Convert predicted and actual vectors to factors with the same levels
predicted <- factor(predicted, levels = c(1, 2))
actual <- factor(actual, levels = c(1, 2))

# Calculate confusion matrix
conf_matrix <- confusionMatrix(predicted, actual)

# Print the confusion matrix
print(conf_matrix)

