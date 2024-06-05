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


# prob matrix

get_cluster_prob_matrix <- function(mu, phi, alpha, cluster_pi) {
  # number of clusters
  G <- length(alpha)
  # number of categories
  q <- length(mu)
  
  cluster_probs <- lapply(1:G, function(x) numeric(q))
  
  for (g in 1:G) {
    probs <- numeric(q)
    for (k in 1:q) {
      linear <- mu[k] + phi[k] * alpha[g]
      if (k > 1) {
        probs[k] <- exp(linear)
      } else {
        probs[k] <- 1
      }
    }
    cluster_probs[[g]] <- probs #/sum(probs)
  }
  
  # normalize
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  adjusted_probs <- mapply(function(cluster, p) {
    cluster * p
  }, cluster_probs, cluster_pi, SIMPLIFY = FALSE)
  
  # Flatten the list to calculate the global sum
  all_probs <- unlist(adjusted_probs)
  total_sum <- sum(all_probs)
  
  # Normalize each cluster by the global sum
  normalized_cluster_probs <- lapply(adjusted_probs, function(cluster) {
    cluster / total_sum
  })
  
  normalized_cluster_probs
  
  return(normalized_cluster_probs)
}

parlist <- results$parlist.out
cluster_pi <- results$pi.out
mu <- parlist$mu 
phi <- parlist$phi
alpha <- parlist$rowc
print(mu)
print(phi)
print(alpha)
print(cluster_pi)

probs <- get_cluster_prob_matrix(mu, phi, alpha, cluster_pi)
print(paste("Cluster Prob matrix:", probs))


# prediction

predict_cluster_by_category_osm <- function (k, probs){
  if (k < 1 || k > length(probs[[1]])) {
    stop("category id must be positive and less than categories count.")
  }
  return(which.max(sapply(probs, function(x) x[k])))
}

## prediction one observation
new_obs_predictors <- test_df[1,]
new_obs_predictors <- as.numeric(as.character(test_df[1,]))
print(new_obs_predictors)
print(new_obs_predictors[1])
print(predict_cluster_by_category_osm(new_obs_predictors[1], probs))

## prediction all test data
actual <- test_df[,2]
predicted <- c()

for (i in 1:nrow(test_df)){
    new_obs <- as.numeric(as.character(test_df[i,]))
    predicted[i] <- predict_cluster_by_category_osm(new_obs[1], probs)
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

