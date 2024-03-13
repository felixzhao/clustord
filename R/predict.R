predict_cluster <- function(new_observation, final_parameters) {
  pi <- final_parameters$pi
  mus <- final_parameters$parameters$mu
  phis <- final_parameters$parameters$phi
  
  # Initialize a vector to store the probabilities of the new observation for each cluster
  cluster_probs <- numeric(length(pi))
  
  for (cluster in 1:length(pi)) {
    mu <- mus[[cluster]]
    phi <- phis[[cluster]]
    
    # Compute the probability of the new observation in the current cluster
    # This is a simplified example; you'd replace this with the actual computation based on the OSM model specifics
    likelihood <- dnorm(new_observation, mean=mu, sd=1) # Example: Gaussian likelihood; replace with appropriate computation
    
    # Weight the likelihood by the mixing coefficient
    cluster_probs[cluster] <- likelihood * pi[cluster]
  }
  
  # Predict the cluster with the highest probability
  predicted_cluster <- which.max(cluster_probs)
  return(predicted_cluster)
}

# Example usage
# new_observation <- 0.5 # Example new observation
# final_parameters <- list(
#   pi=c(0.0904054993015109, 0.630375222872896, 0.279219277825593),
#   parameters=list(
#     mu=c(0, 0.656703919589771, 0.446979919400921),
#     phi=c(0, 0.999999823124001, 1)
#   )
# )

# predicted_cluster <- predict_cluster(new_observation, final_parameters)
# print(predicted_cluster)