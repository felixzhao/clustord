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

source("R/catchup_9/osm_col_effect_functions.R")

# arguments
number_of_y <- 100
df_path = paste0("./data/simulation_y_",number_of_y,"_c10_1.csv")

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




## training

training <- function(df, number_of_y){
  # training
  # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 2 row clustering groups:
  results <- clustord(Y~ROWCLUST+COL,model="OSM",5,long.df=df, EM.control=list(EMcycles=100,startEMcycles=5), nstarts=20)
  
  
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

  return(list(probs=probs, cluster_pi=cluster_pi, model=results))
  
}

### prediction

z_prediction <- function(y_i, model_probs, cluster_pi) {
  row_probs <- lapply(1:length(y_i), function(x) numeric(length(cluster_pi)))
  for (j in 1: number_of_y) {
    row_probs[[j]] <- t(sapply(y_i[j], function(k) model_probs[,j,k]))
  }
  
  row_probs_matrix <- do.call(rbind, row_probs)
  # Calculate the products of each column
  z <- apply(row_probs_matrix, 2, prod)
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  adjusted_z <- cluster_pi * z
  
  return(adjusted_z)
}

cluster_prediction <- function(y, probs, cluster_pi){
  z <- z_prediction(y, probs, cluster_pi)
  # the sum of z for each y must be equal to 1
  return(which.max(z))
}

prediction <- function(test_Y, model_probs, cluster_pi){
  ## prediction all test data
  predicted <- c()
  
  for (i in 1:nrow(test_Y)){
    print(i)
    new_obs <- as.numeric(as.character(test_Y[i,]))
    predicted[i] <- cluster_prediction(new_obs, model_probs, cluster_pi)
  }
  return(predicted)
}

### evaluation 
evaluation <- function(predicted, actual){
  # confusion matrix
  # Convert predicted and actual vectors to factors with the same levels
  predicted <- factor(predicted, levels = c(1, 2, 3,4,5))
  actual <- factor(actual, levels = c(1, 2, 3,4,5))
  
  # Calculate confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual)
  
  # return the confusion matrix
  return(conf_matrix)
}

# main

#main <- function(df_path, number_of_y){
  data_dfs <- load_data(df_path)
  train_df <- data_dfs$train_df
  test_Y <- data_dfs$test_Y
  test_cluster <- data_dfs$test_cluster$cluster
  
  model <- training(data_dfs$train_df, number_of_y)
  
  model_probs <- model$probs
  cluster_pi <- model$cluster_pi
  
  predicted <- prediction(test_Y = test_Y, model_probs = model_probs, cluster_pi = cluster_pi)
  
  conf_matrix <- evaluation(predicted = predicted, actual = test_cluster)
#}

print(conf_matrix)
