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

# arguments
number_of_cluster <- 5
number_of_y <- 100
df_path = paste0("./data/dist_simulation_y_",number_of_y,"_c10_2.csv")

model_save_path = paste0("./data/model_y_",number_of_y,"_c10_2.rds")

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

get_cluster_prob_matrix <- function(mu, phi, alpha, beta, cluster_pi, number_of_y) {
  # number of clusters
  G <- length(alpha)
  # number of categories
  q <- length(mu)
  
  cluster_probs <- lapply(1:G, function(x) numeric(q))
  
  for (g in 1:G) {
    probs <- numeric(q)
    for (k in 1:q) {
      for (j in 1: number_of_y) {
        linear <- mu[k] + phi[k] * (alpha[g] + beta[j])
        if (k > 1) {
          probs[k] <- exp(linear)
        } else {
          probs[k] <- 1
        }
      }
    }
    cluster_probs[[g]] <- probs/sum(probs)
  }
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  # adjusted_probs <- mapply(function(cluster, p) {
  #   cluster * p
  # }, cluster_probs, cluster_pi, SIMPLIFY = FALSE)
  
  return(do.call(rbind, lapply(cluster_probs, unlist)))
}


## training

training <- function(df, number_of_y){
  # training
  # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 5 row clustering groups:
  results <- clustord(Y~ROWCLUST+COL,model="OSM",5,long.df=df, EM.control=list(EMcycles=100,startEMcycles=2), nstarts=20)
  

  parlist <- results$parlist.out
  cluster_pi <- results$pi.out
  mu <- parlist$mu 
  phi <- parlist$phi
  alpha <- sort(parlist$rowc)
  beta <- parlist$col
  
  print(mu)
  print(phi)
  print(alpha)
  print(cluster_pi)
  
  probs <- get_cluster_prob_matrix(mu, phi, alpha, beta, cluster_pi, number_of_y)
  print("Cluster Prob matrix:")
  probs
  
  return(list(probs=probs, cluster_pi=cluster_pi, results=results))

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
  levels <- seq(1:number_of_cluster)
  print(levels)
  # Convert predicted and actual vectors to factors with the same levels
  predicted <- factor(predicted, levels = levels)
  actual <- factor(actual, levels = levels)
  
  # Calculate confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual)
  
  # return the confusion matrix
  return(conf_matrix)
}

# main

# main <- function(df_path, number_of_y){
  data_dfs <- load_data(df_path)
  train_df <- data_dfs$train_df
  test_Y <- data_dfs$test_Y
  test_cluster <- data_dfs$test_cluster$cluster
  
  model <- training(data_dfs$train_df, number_of_y)
  
  probs <- model$probs
  cluster_pi <- model$cluster_pi
  results <- model$results
  
  saveRDS(model, file=model_save_path)
  
  predicted <- prediction(test_Y = test_Y, probs = probs, cluster_pi = cluster_pi)
  
  conf_matrix <- evaluation(predicted = predicted, actual = test_cluster)
# }

conf_matrix <- main(df_path, number_of_y)
print(conf_matrix)


# n_of_y_list <- c(30, 50)
# conf_matrix_list <- list()
# 
# for (number_of_y in n_of_y_list){
#   print(number_of_y)
#   
#   data_path <- paste0("./data/simulation_y_",number_of_y,"_c4_1.csv")
#   conf_matrix <- main(data_path)
#   print(conf_matrix)
#   conf_matrix_list[[paste(number_of_y)]] <- conf_matrix
# }
# 
