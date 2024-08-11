set.seed(123)

# prob matrix for each Y, in current stage suppose all same.

G=3 # number of clusters
q=3 # number of categories
alpha=c(1.5, 0, -1.5) 
beta <- c(0.6, 0.9) # col effects
mu=c(0, 0.6, 0.3) 
phi=c(0, 0.8, 1)
# cluster_pi = c(0.1, 0.3, 0.6)
# sample_size <- 2500
# total_sample_size <- sample_size * G

number_of_y = 2

cluster_probs <- lapply(1:G, function(x) numeric(q))

for (g in 1:G) {
  # probs <- numeric(q)
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
      
      print(paste("G", g, "; q:", k, "; j:", j))
      print(paste("mu", mu[k], "phi", phi[k], "alpha", alpha[g], "beta", beta[j]))
      print(paste("linear", linear))
      print(paste("exp linear", exp(linear)))
    }
    
    print(paste("G", g, "; q:", k))
    print(paste("before", probs))
    
    category_probs[[k]] <- probs / sum(probs) # normalise k for each j # 2 dim, j, k
    
    print(paste("after",category_probs[[k]]))
  }
  cluster_probs[[g]] <- category_probs # 3 dim, g, j, k
}

cluster_probs[[1]][[2]]
