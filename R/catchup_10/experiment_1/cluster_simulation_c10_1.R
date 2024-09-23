# install.packages("dplyr")

library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)
library(dplyr)

set.seed(123)

source("R/catchup_lib/osm_col_effect_functions.R")
# prob matrix for each Y, in current stage suppose all same.

# default parameters
number_of_y <- 100
G <- 5
alpha <- c(-2, -1, 0, 1, 2)
cluster_pi <- c(0.2, 0.2, 0.2, 0.2, 0.2)

q <- 3
beta <- runif(number_of_y, min = -1, max = 1)
mu <- c(0, 0, 0)
phi <- c(0, 0.5, 1)
sample_size <- 2500
total_sample_size <- sample_size * G



cluster_probs <- get_cluster_prob_matrix(mu, phi, alpha, beta, cluster_pi, number_of_y)

# sampling

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

save_data <- function(df, save_path = "./data/simulation_catgories_n_cluster_c10_1.csv"){
  # save to csv file
  write.csv(df, save_path , row.names=FALSE)
  print(paste("Save data to",save_path,"Done."))
}


save_path <- paste0("./data/simulation_y_",number_of_y,"_c10_1.csv")

df_y <- data_samping(sample_size, total_sample_size, cluster_pi, q, 
                         cluster_probs, number_of_y
                         )

save_data(df_y, save_path = save_path)
