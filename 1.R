set.seed(1)
long.df <- data.frame(Y=factor(sample(1:3,5*20,replace=TRUE)),
                ROW=factor(rep(1:20,times=5)),COL=rep(1:5,each=20))

# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 3 row clustering groups:
results <- clustord(Y~ROWCLUST,model="OSM",3,long.df=long.df, EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)

# cat("results")
print(results)


predict_osm_category <- function(k, mu, phi, alpha) {
  mu_k <- mu[k]
  phi_k <- phi[k]
  q <- length(mu)
  RG <- length(alpha)
  
  probabilities <- matrix(NA, nrow=q)
  probabilities[, 1] <- 1
  
  for( g in 1:RG) {
    probabilities[g] <- exp(mu_k + phi_k * alpha[g])
  }
  
  # Normalize probabilities so they sum to 1 across categories for each observation
  for (g in 1:RG) {
    probabilities[g] <- probabilities[g] / sum(probabilities[g])
  }
  
  # Predict the category with the highest probability for each observation
  predicted_categories <- apply(probabilities, 1, which.max)
  
  return(predicted_categories)
  
}

predict_osm_category_2 <- function(k, mu, phi, alpha) {
  if (k > length(mu) || k < 1) {
    stop("k is out of bounds for the length of mu")
  }
  
  mu_k <- mu[k]
  phi_k <- phi[k]
  RG <- length(alpha)
  
  # Calculate the probability for each group
  probabilities <- rep(0, RG)
  for (g in 1:RG) {
    probabilities[g] <- exp(mu_k + phi_k * alpha[g])
  }
  
  # Normalize probabilities so they sum to 1
  probabilities <- probabilities / sum(probabilities)
  
  # Find the group with the highest probability
  predicted_group <- which.max(probabilities)
  
  return(predicted_group)
}

predict_osm_category_3 <- function(k, mu, phi, alpha) {
  if (k > length(mu) || k < 1) {
    stop("k is out of bounds for the length of mu")
  }
  
  # Adjusted to correctly use the first value of mu and phi as 0
  mu_k <- mu[k]
  phi_k <- phi[k]
  RG <- length(alpha)
  
  # The first value for each is always 0, so start with that as a base
  probabilities <- rep(0, RG)  # Initialize probabilities with 0
  
  # Compute the probabilities for each group
  for (g in 1:RG) {
    probabilities[g] <- exp(mu_k + phi_k * alpha[g])
  }
  
  # Normalize probabilities so they sum to 1
  probabilities <- probabilities / sum(probabilities)
  
  # Return the group with the highest probability
  predicted_group <- which.max(probabilities)
  
  return(predicted_group)
}



# coefficients <- results$outvect[-(1:2)]#results$outvect
parlist = results$parlist.out

mu <- parlist$mu #results$EM.status$params.for.best.lli$mu
cat("mu", mu)
print(mu)

phi <- parlist$phi
cat("phi")
print(phi)

rowc <- parlist$rowc
cat("rowc")
print(rowc)


new_obs_predictors <- matrix(sample(1:5, 6, replace = TRUE), ncol = 1)
cat("obs")
print(new_obs_predictors)
print(new_obs_predictors[1])

# Predict categories for new observations
predicted_categories <- predict_osm_category_2(new_obs_predictors[1], mu, phi, rowc)

cat("predictions")
print(predicted_categories)
cat("\n")


results <- sapply(new_obs_predictors, predict_osm_category, mu=mu, phi=phi, alpha=rowc)

print(results)


# Iterate over each k in new_obs_predictors and predict the group
results <- apply(new_obs_predictors, 1, function(k) predict_osm_category_3(k, mu, phi, rowc))

print(results)




predict_osm_category_5 <- function(k, mu, phi, alpha) {
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
  #probabilities <- probabilities / sum(probabilities)
  
  # Return the group with the highest probability
  predicted_group <- which.max(probabilities)
  
  return(probabilities)
}

parlist = results$parlist.out
mu <- parlist$mu 
phi <- parlist$phi
alpha <- parlist$rowc
print(mu)

# Now, to call this function for all elements in new_obs_predictors and print results:
new_obs_predictors <- matrix(sample(1:3, 6, replace = TRUE), ncol = 1)  # Adjusted to match mu and phi lengths
print(new_obs_predictors)
print(new_obs_predictors[1])
print(length(mu))

probs <- predict_osm_category_5(new_obs_predictors[1], mu, phi, alpha)
print(probs)

which.max(probs)
