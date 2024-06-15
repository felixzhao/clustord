# install.packages("caret")
# install.packages("dplyr")

# Load necessary library
library(dplyr)
library("clustord")
library(caret)

library(ggplot2)
library(gridExtra)
library(grid)


set.seed(123)

# funtions
## read data
load_data <- function(data_path){
  # Load the data from the CSV file
  df <- read.csv(data_path, stringsAsFactors = FALSE)
  
  ### split train & test data
  
  
  indexes <- sample(1:nrow(df), size = 0.7 * nrow(df))
  
  train_df <- df[indexes, ]
  test_df <- df[-indexes, ]
  
  
  ## data preparation
  
  ### train data
  train_clust_df <- mat2df(mat = train_df %>% select(-cluster))
  
  ### test data
  test_Y <- test_df %>% select(-cluster)
  
  test_cluster <- test_df["cluster"]
  
  return(list(train_df=train_clust_df, test_Y=test_Y, test_cluster=test_cluster))
  
}

## get prob matrix

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
    # normalize
    cluster_probs[[g]] <- probs/sum(probs)
  }
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  # adjusted_probs <- mapply(function(cluster, p) {
  #   cluster * p
  # }, cluster_probs, cluster_pi, SIMPLIFY = FALSE)
  
  return(do.call(rbind, lapply(cluster_probs, unlist)))
}


## training

training <- function(df){
  # training
  # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 2 row clustering groups:
  results <- clustord(Y~ROWCLUST,model="OSM",2,long.df=df, EM.control=list(EMcycles=100,startEMcycles=5), nstarts=5)
  

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
  print("Cluster Prob matrix:")
  probs
  
  return(list(probs=probs, cluster_pi=cluster_pi))

}

### prediction

z_prediction <- function(y, probs, cluster_pi) {
  # Extract columns based on y values and combine into matrix p
  p <- t(sapply(y, function(k) probs[, k]))
  # print(p)
  # Calculate the products of each column
  z <- apply(p, 2, prod)
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  adjusted_z <- mapply(function(cluster, p) {
    cluster * p
  }, z, cluster_pi, SIMPLIFY = FALSE)
  
  return(adjusted_z)
}

cluster_prediction <- function(y, probs, cluster_pi){
  z <- z_prediction(y, probs, cluster_pi)
  return(which.max(z))
}

prediction <- function(test_Y, probs, cluster_pi){
  ## prediction all test data
  predicted <- c()
  
  for (i in 1:nrow(test_Y)){
    new_obs <- as.numeric(as.character(test_Y[i,]))
    predicted[i] <- cluster_prediction(new_obs, probs, cluster_pi)
  }
  return(predicted)
}

### evaluation 
evaluation <- function(predicted, actual){
  # confusion matrix
  # Convert predicted and actual vectors to factors with the same levels
  predicted <- factor(predicted, levels = c(1, 2))
  actual <- factor(actual, levels = c(1, 2))
  
  # Calculate confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual)
  
  # return the confusion matrix
  return(conf_matrix)
}

# main

main <- function(df_path){
  data_dfs <- load_data(df_path)
  train_df <- data_dfs$train_df
  test_Y <- data_dfs$test_Y
  test_cluster <- data_dfs$test_cluster$cluster
  
  model <- training(data_dfs$train_df)
  
  probs <- model$probs
  cluster_pi <- model$cluster_pi
  
  predicted <- prediction(test_Y = test_Y, probs = probs, cluster_pi = cluster_pi)
  
  conf_matrix <- evaluation(predicted = predicted, actual = test_cluster)
}

df_path  <- "./data/simulation_y_20_c4_1.csv"
conf_matrix <- main(df_path)
print(conf_matrix)


