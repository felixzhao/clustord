# install.packages("dplyr")

# Load necessary library
library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)
library(dplyr)


library("clustord")
library(caret)




source("R/catchup_lib/osm_col_effect_functions.R")
# prob matrix for each Y, in current stage suppose all same.

# default parameters
number_of_y <- 20
G <- 2
q <- 3
alpha <- c(-1, 1)
beta <- runif(number_of_y, min = -1, max = 1)
mu <- c(0, 0, 0)
phi <- c(0, 0.5, 1)
cluster_pi <- c(0.5, 0.5)
sample_size <- 1000
total_sample_size <- sample_size * G


# Data Simulation functions
## sampling

y_sampling <- function(sample_size, total_sample_size, cluster_pi, q, col_cluster_probs, y_idx) {
  data_list <- lapply(1:G, function(x) numeric(sample_size))

  for (g in 1:G) {

    cluster_sample_size <- total_sample_size * cluster_pi[g]
    data_list[[g]] <- sample(1:q, size = cluster_sample_size, replace = TRUE, prob = col_cluster_probs[[g]])
  }

  # Flatten the list of lists into a single vector
  data_val <- unlist(data_list)

  # Create a vector indicating the sublist number for each value
  cluster_idx <- unlist(lapply(1:G, function(g) rep(g, length(data_list[[g]]))))

  # Creating a data frame for ggplot
  samples <- data.frame(category = data_val, cluster = cluster_idx)
  samples <- samples %>% mutate(id = row_number()) # Add a unique identifier
  names(samples)[1] <- paste0("Y", y_idx)
  return(samples)
}

data_samping <- function(sample_size, total_sample_size, cluster_pi, q, 
                         cluster_probs, number_of_y) {

  data_list <- list()
  clusters <- vector()
  # Create a list of N dataframes
  for (g in 1:G) {
    cluster_sample_size <- total_sample_size * cluster_pi[g]
    clust_matrix <- matrix(0, cluster_sample_size, number_of_y)
    for (j in 1: number_of_y) {
      clust_matrix[,j] <- sample(1:q, size = cluster_sample_size, replace = TRUE, prob = cluster_probs[g,j,])
    }
    data_list[[g]] <- clust_matrix
    clusters <- c(clusters, rep(g, cluster_sample_size))
  }
  res1 <- do.call(rbind, data_list)
  res2 <- as.data.frame(cbind(res1, clusters))
  colnames(res2) <- c(paste0("Y",1:number_of_y), "cluster")

  return(res2)

}

# prediction funtions
## read data
load_data <- function(df){
  # # Load the data from the CSV file
  # df <- read.csv(data_path, stringsAsFactors = FALSE)
  
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

  return(list(probs = probs, cluster_pi = cluster_pi, model = results))

}

### prediction

z_prediction <- function(y_i, model_probs, cluster_pi) {
  row_probs <- lapply(1:length(y_i), function(x) numeric(length(cluster_pi)))
  for (j in 1: number_of_y) {
    # Extract columns based on y values and combine into matrix p
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
  predicted <- factor(predicted, levels = c(1, 2))
  actual <- factor(actual, levels = c(1, 2))
  
  # Calculate confusion matrix
  conf_matrix <- confusionMatrix(predicted, actual)
  
  # return the confusion matrix
  return(conf_matrix)
}

# replicate
replicate_function <- function(random_seed){
    set.seed(random_seed)

    # Data Simulation
    cluster_probs <- get_cluster_prob_matrix(mu, phi, alpha, beta, cluster_pi, number_of_y)
    raw_df <- data_samping(sample_size, total_sample_size, cluster_pi, q, cluster_probs, number_of_y)

    # prediction
    data_dfs <- load_data(raw_df)
    train_df <- data_dfs$train_df
    test_Y <- data_dfs$test_Y
    test_cluster <- data_dfs$test_cluster$cluster

    model <- training(data_dfs$train_df, number_of_y)

    model_probs <- model$probs
    cluster_pi <- model$cluster_pi

    predicted <- prediction(test_Y = test_Y, model_probs = model_probs, cluster_pi = cluster_pi)

    conf_matrix <- evaluation(predicted = predicted, actual = test_cluster)
    print(conf_matrix)
    accuracy_value <- as.numeric(conf_matrix$overall["Accuracy"])

    return(accuracy_value)
}

# Call replicate_function with different random seeds 10 times and store the results
accuracy_values <- replicate(10, replicate_function(random_seed = sample(1:1000, 1)))

# Calculate the standard deviation of the accuracy values
accuracy_std <- sd(accuracy_values)


print(accuracy_values)

# Print the standard deviation
print(accuracy_std)
