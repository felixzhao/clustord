# install.packages("caret")
# install.packages("dplyr")

# Load necessary library
library(dplyr)
library("clustord")
library(caret)

library(ggplot2)
library(gridExtra)
library(grid)

## read data
# Load the data from the CSV file
df <- read.csv("./data/simulation_catgories_n_cluster_c3_1.csv", stringsAsFactors = FALSE)

# Check the structure of the loaded data
str(df)

# View the first few rows of the data
head(df)

## parameters

set.seed(123)
number_of_y <- ncol(df) - 1


## plot data

# Plot
plot_y <- function(df, y_idx) {
  sample_name <- paste0("Y", y_idx)
  # Plot
  data1 <- data.frame(Sample = df[[sample_name]], Cluster = as.factor(df$cluster))
  
  plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density by Cluster",
         x = sample_name,
         y = "Density") +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    theme_minimal()
}

plot_all_predictors <- function(df, number_of_y) {
  plots <- list()
  for (i in 1:number_of_y){
    plots[[i]] <- plot_y(df, i)
  }
  
  # Calculate number of rows and columns dynamically
  ncol <- 3  # Number of columns
  nrow <- ceiling(number_of_y / ncol)  # Number of rows
  
  # Ensure the grid has enough cells
  stopifnot(nrow * ncol >= length(plots))
  
  # Arrange the plots dynamically and add a title
  grid.arrange(
    grobs = plots,
    ncol = ncol,
    nrow = nrow,
    top = textGrob("Density of Categories", gp = gpar(fontsize = 16, fontface = "bold"))
  )
}

plot_all_predictors(df, number_of_y = number_of_y)

### split train & test data


indexes <- sample(1:nrow(df), size = 0.7 * nrow(df))

train_df <- df[indexes, ]
test_df <- df[-indexes, ]

## plot train

plot_all_predictors(train_df, number_of_y = number_of_y)

## data preparation

train_clust_df <- mat2df(mat = train_df %>% select(-cluster))

str(train_clust_df)

# test no need mat2df
# test_clust_df <- mat2df(mat = test_df["category"])

# str(test_clust_df)

plot_all_predictors(test_df, number_of_y = number_of_y)

test_Y <- test_df %>% select(-cluster)

test_cluster <- test_df["cluster"]





###


# training
# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 2 row clustering groups:
results <- clustord(Y~ROWCLUST,model="OSM",2,long.df=train_clust_df, EM.control=list(EMcycles=100,startEMcycles=5), nstarts=5)

parlist <- results$parlist.out
cluster_pi <- results$pi.out
mu <- parlist$mu 
phi <- parlist$phi
alpha <- parlist$rowc
print(mu)
print(phi)
print(alpha)
print(cluster_pi)

# prob matrix

get_cluster_prob_matrix <- function(mu, phi, alpha, cluster_pi) {
  # number of clusters
  G <- length(alpha)
  # number of categories
  q <- length(mu)
  
  cluster_probs <- lapply(1:G, function(x) numeric(q))
  
  for (g in 1:G) {
    probs <- numeric(q)
    for (k in 1:q) {
      linear <- mu[k] + phi[k] * alpha[g]
      if (k > 1) {
        probs[k] <- exp(linear)
      } else {
        probs[k] <- 1
      }
    }
    # normalize
    cluster_probs[[g]] <- probs/sum(probs)
  }
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  # adjusted_probs <- mapply(function(cluster, p) {
  #   cluster * p
  # }, cluster_probs, cluster_pi, SIMPLIFY = FALSE)
  
  return(do.call(rbind, lapply(cluster_probs, unlist)))
}



probs <- get_cluster_prob_matrix(mu, phi, alpha, cluster_pi)
print("Cluster Prob matrix:")
probs


# prediction

z_prediction <- function(y, probs, cluster_pi) {
  # Extract columns based on y values and combine into matrix p
  p <- t(sapply(y, function(k) probs[, k]))
  # print(p)
  # Calculate the products of each column
  z <- apply(p, 2, prod)
  
  # Adjust each cluster's probabilities by multiplying with corresponding pi
  adjusted_z <- mapply(function(cluster, p) {
    cluster * p
  }, z, cluster_pi, SIMPLIFY = FALSE)
  
  return(adjusted_z)
}

cluster_prediction <- function(y, probs, cluster_pi){
  z <- z_prediction(y, probs, cluster_pi)
  return(which.max(z))
}

## prediction one observation
new_obs_predictor <- test_Y[1,]

z <- z_prediction(new_obs_predictor, probs, cluster_pi)

one_prediction <- cluster_prediction(new_obs_predictor, probs, cluster_pi)

print(z)
print(one_prediction)

## prediction all test data
actual <- test_df[,2]
predicted <- c()

for (i in 1:nrow(test_Y)){
  new_obs <- as.numeric(as.character(test_Y[i,]))
  predicted[i] <- cluster_prediction(new_obs, probs, cluster_pi)
}

# Accuracy

# Calculate accuracy
accuracy <- sum(actual == predicted) / length(actual)

# Print the accuracy
print(paste("Accuracy:", accuracy))


# confusion matrix

# Convert predicted and actual vectors to factors with the same levels
predicted <- factor(predicted, levels = c(1, 2))
actual <- factor(actual, levels = c(1, 2))

# Calculate confusion matrix
conf_matrix <- confusionMatrix(predicted, actual)

# Print the confusion matrix
print(conf_matrix)

