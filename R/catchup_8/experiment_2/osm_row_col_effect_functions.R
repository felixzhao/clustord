
# functions
generate_cluster_probs <- function(G, q, alpha, beta, mu, phi, cluster_pi, delta,
                                   sample_size, total_sample_size, number_of_y, number_of_row
                                   , X){
  cluster_probs <- array(0,dim = c( G, number_of_row, number_of_y, q))
  
  for (g in 1:G) {
    #category_probs <- array() #list() #lapply(1:number_of_y, function(x) numeric(q))
    for (i in 1: number_of_row) {
        # for()
        for (j in 1: number_of_y) { # j loop must be out of k loop
            for (k in 1:q) {
                if (k > 1) {
                  linear <- mu[k] + phi[k] * (alpha[g] + beta[j] + sum(delta*X[i,]))
                  cluster_probs[g,i, j, k] <- exp(linear)
                } else {
                  cluster_probs[g,i, j, k] <- 1
                }
            }
            cluster_probs[g,i,j,] <- cluster_probs[g,i,j,] / sum(cluster_probs[g,i,j,])
        }
    }
  }
  return(cluster_probs)
}

# sampling

y_sampling <- function(sample_size, total_sample_size, cluster_pi, q, col_cluster_probs, y_idx) {
  data_list <- list()#lapply(1:G, function(x) numeric(sample_size))
  
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
                         cluster_probs, number_of_y, number_of_row, X) {
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