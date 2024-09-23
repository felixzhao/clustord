## get prob matrix

get_cluster_prob_matrix <- function(mu, phi, alpha, beta, cluster_pi, number_of_y) {
  # number of clusters
  G <- length(alpha)
  # number of categories
  q <- length(mu)
  
  cluster_probs <- array(0,dim = c( G, number_of_y, q))
  
  for (g in 1:G) {
    for (j in 1: number_of_y) { 
      for (k in 1:q) {
        if (k > 1) {
          linear <- mu[k] + phi[k] * (alpha[g] + beta[j])
          cluster_probs[g,j, k] <- exp(linear)
        } else {
          cluster_probs[g,j, k] <- 1
        }
      }
      cluster_probs[g,j,] <- cluster_probs[g,j,] / sum(cluster_probs[g,j,])
    }
  }
  
  return(cluster_probs)#(do.call(rbind, lapply(cluster_probs, unlist)))
}