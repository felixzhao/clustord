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
number_of_y <- 20
df_path = paste0("./data/simulation_y_",number_of_y,"_c7_1.csv")

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
    category_probs <- lapply(1:q, function(x) numeric(number_of_y))
    for (k in 1:q) {
      probs <- numeric(number_of_y)
      for (j in 1: number_of_y) { # j loop must be out of k loop
        linear <- mu[k] + phi[k] * (alpha[g] + beta[j])
        if (k > 1) {
          probs[j] <- exp(linear)
        } else {
          probs[j] <- 1
        }
      }
      category_probs[[k]] <- probs #/ sum(probs) # normalise k for each j # 2 dim, j, k
    }
    sum_flattened_list <- sum(unlist(category_probs))
    cluster_probs[[g]] <- category_probs # 3 dim, g, j, k
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
  # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 2 row clustering groups:
  results <- clustord(Y~ROWCLUST+COL,model="OSM",2,long.df=df, EM.control=list(EMcycles=100,startEMcycles=5), nstarts=5)
  
  
  parlist <- results$parlist.out
  cluster_pi <- results$pi.out
  mu <- parlist$mu 
  phi <- parlist$phi
  alpha <- sort(parlist$rowc)
  beta <- parlist$col
  
  print(mu)
  print(phi)
  print(alpha)
  print(beta)
  print(cluster_pi)
  
  probs <- get_cluster_prob_matrix(mu, phi, alpha, beta, cluster_pi, number_of_y)
  print("Cluster Prob matrix:")
  probs
  
  col_clu_probs <- lapply(1:number_of_y, function(i) {
         col_cluster_probs <- lapply(cluster_probs, function(cluster) {
               sapply(cluster, function(sublist) sublist[i])
         })})
  
  return(list(probs=col_clu_probs, cluster_pi=cluster_pi))
  
}

### prediction

z_prediction <- function(y, probs, cluster_pi) {
  probs_matrix <- do.call(rbind, probs)
  # Extract columns based on y values and combine into matrix p
  p <- t(sapply(y, function(k) probs_matrix[, k]))
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

prediction <- function(test_Y, model_probs, cluster_pi){
  ## prediction all test data
  predicted <- c()
  
  for (i in 1:nrow(test_Y)){
    print(i)
    new_obs <- as.numeric(as.character(test_Y[i,]))
    col_probs <- model_probs[[i]]
    predicted[i] <- cluster_prediction(new_obs, col_probs, cluster_pi)
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

main <- function(df_path, number_of_y){
  data_dfs <- load_data(df_path)
  train_df <- data_dfs$train_df
  test_Y <- data_dfs$test_Y
  test_cluster <- data_dfs$test_cluster$cluster
  
  model <- training(data_dfs$train_df, number_of_y)
  
  model_probs <- model$probs
  cluster_pi <- model$cluster_pi
  
  predicted <- prediction(test_Y = test_Y, probs = model_probs, cluster_pi = cluster_pi)
  
  conf_matrix <- evaluation(predicted = predicted, actual = test_cluster)
}

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
