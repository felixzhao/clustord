## get prob matrix

get_cluster_prob_matrix <- function(mu, phi, alpha, beta, cluster_pi, number_of_y) {
  # number of clusters
  G <- length(alpha)
  # number of categories
  q <- length(mu)
  
  cluster_probs <- array(0,dim = c( G, number_of_y, q)) #lapply(1:G, function(x) numeric(q))
  
  for (g in 1:G) {
    #category_probs <- array() #list() #lapply(1:number_of_y, function(x) numeric(q))
    for (j in 1: number_of_y) { # j loop must be out of k loop
      #probs <- numeric(q)
      for (k in 1:q) {
        if (k > 1) {
          linear <- mu[k] + phi[k] * (alpha[g] + beta[j])
          cluster_probs[g,j, k] <- exp(linear)
        } else {
          cluster_probs[g,j, k] <- 1
        }
        # print(paste( g,k,j))
        # print(paste( mu[k], phi[k], alpha[g], beta[j]))
        # print(paste('linear', linear))
      }
      # print(paste('probs', probs))
      # print(paste('norm probs', probs / sum(probs)))
      cluster_probs[g,j,] <- cluster_probs[g,j,] / sum(cluster_probs[g,j,])
      # category_probs[[j]] <- probs / sum(probs) # normalise k for each j # 2 dim, j, k
    }
    # sum_flattened_list <- sum(unlist(category_probs))
    # cluster_probs[[g]] <- category_probs # 3 dim, g, j, k
  }
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  # adjusted_probs <- mapply(function(cluster, p) {
  #   cluster * p
  # }, cluster_probs, cluster_pi, SIMPLIFY = FALSE)
  
  return(cluster_probs)#(do.call(rbind, lapply(cluster_probs, unlist)))
}