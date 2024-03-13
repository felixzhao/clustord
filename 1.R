set.seed(1)
long.df <- data.frame(Y=factor(sample(1:3,5*20,replace=TRUE)),
                ROW=factor(rep(1:20,times=5)),COL=rep(1:5,each=20))

# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 3 row clustering groups:
results <- clustord(Y~ROWCLUST,model="OSM",3,long.df=long.df, EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)

# cat("results")
# print(results)


cat('$outvect')
print(results$outvect)

# cat('$parlist.out$mu')
# print(results$parlist.out$mu)


# cat('$parlist.out$phi')
# print(results$parlist.out$phi)


# cat('$EM.status$params.for.best.lli$mu')
# print(results$EM.status$params.for.best.lli$mu)

# cat('$EM.status$params.for.best.lli$phi')
# print(results$EM.status$params.for.best.lli$phi)

# cat('$EM.status$params.for.best.lli$pi')
# print(results$EM.status$params.for.best.lli$pi)

# Function to predict category for new observations (placeholder, modify as needed)
predict_category <- function(predictors, mu, phi) {  
  num_obs <- nrow(predictors)
  num_categories <- length(mu) + 1  # mu separates categories

  probabilities <- matrix(runif(num_obs * num_categories), nrow=num_obs, ncol=num_categories)
  probabilities <- sweep(probabilities, 1, rowSums(probabilities), "/")  # Normalize to sum to 1
  
  # Predict the category with the highest probability for each observation
  predicted_categories <- apply(probabilities, 1, which.max)
  
  return(predicted_categories)
}


# coefficients <- results$outvect[-(1:2)]#results$outvect

mu <- results$EM.status$params.for.best.lli$mu

phi <- results$EM.status$params.for.best.lli$phi

# new_obs_predictors <- matrix(rnorm(20), ncol=2)
set.seed(123)  # For reproducibility
new_obs_predictors <- matrix(sample(1:10, 30, replace = TRUE), ncol = 2)
cat("obs")
print(new_obs_predictors)
cat("\n")


# Predict categories for new observations
predicted_categories <- predict_category(new_obs_predictors, mu, phi)
# predicted_categories <- predict_category_3(new_obs_predictors, mu, coefficients)

cat("predictions")
print(predicted_categories)
cat("\n")
