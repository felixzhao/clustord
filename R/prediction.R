# library("clustord")

set.seed(1)
long.df <- data.frame(Y=factor(sample(1:3,5*20,replace=TRUE)),
                      ROW=factor(rep(1:20,times=5)),COL=rep(1:5,each=20))
# training
# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 3 row clustering groups:
results <- clustord(Y~ROWCLUST,model="OSM",3,long.df=long.df, EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)

cat("parlist.out")
print(results$parlist.out)

# prediction

predict_osm_category <- function(k, mu, phi, alpha) {
  if (k > length(mu) || k < 1) {
    stop("k is out of bounds for the length of mu")
  }
  
  # Adjusted to correctly use the first value of mu and phi as 0
  mu_k <- mu[k]
  phi_k <- phi[k]
  RG <- length(alpha)
  
  # The first value for each is always 0, so start with that as a base
  probabilities <- rep(0.01, RG)  # Initialize probabilities with 0
  
  # Compute the probabilities for each group
  for (g in 1:RG) {
    probabilities[g] <- exp(mu_k + phi_k * alpha[g])
  }
  
  # Normalize probabilities so they sum to 1
  probabilities <- probabilities / sum(probabilities)
  
  # Return the group with the highest probability
  predicted_group <- which.max(probabilities)
  
  return(probabilities)
}

parlist = results$parlist.out
mu <- parlist$mu 
phi <- parlist$phi
alpha <- parlist$rowc
print(mu)
print(phi)
print(alpha)

# Now, to call this function for all elements in new_obs_predictors and print results:
new_obs_predictors <- matrix(sample(1:3, 6, replace = TRUE), ncol = 1)  # Adjusted to match mu and phi lengths
print(new_obs_predictors)
print(new_obs_predictors[1])

probs <- predict_osm_category(new_obs_predictors[1], mu, phi, alpha)
print(probs)

which.max(probs)
