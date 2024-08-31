library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)

set.seed(123)



# functions
generate_cluster_probs <- function(G, q, alpha, beta, mu, phi, cluster_pi, 
                                   sample_size, total_sample_size, number_of_y){
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
  return(cluster_probs)
}

# Plot
plot_sample <- function(df, desc, y_idx=1) {
  sample_name <- paste0("Y", y_idx)
  # Plot
  data1 <- data.frame(Sample = df[[sample_name]], Cluster = as.factor(df$cluster))
  
  plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    labs(title = sample_name,
         x = "Sample Value",
         y = "Density",
         caption = desc) +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    theme_minimal()
  return(plot)
}

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

# main
G=2
q=3
alpha=c(-3,3)
beta=c(0)
mu=c(0, 0.6, 0.3)
phi=c(0, 0.8, 1)
cluster_pi = c(0.3, 0.7)
sample_size <- 1000
total_sample_size <- sample_size * G

number_of_y = 1

# desc = paste('alpha:',paste(alpha, collapse = ", "))
# desc = paste('mu:',paste(mu, collapse = ", "))
# desc = paste('phi:',paste(phi, collapse = ", "))
desc = paste('pi:',paste(cluster_pi, collapse = ", "))

cluster_probs <- generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
                                        sample_size, total_sample_size, number_of_y )
sample_df <- data_samping(sample_size, total_sample_size, cluster_pi, q, 
                          cluster_probs, number_of_y)
plt_image <- plot_sample(sample_df, desc=desc)
print(plt_image)

# # alpha 
# alpha_list <- list(
#   list(-1,0), list(-1,1), list(0,1)
# )
# 
# for (cur_alpha in alpha_list){
#   sample_df <- data_samping(alpha=cur_alpha)
#   plt_image <- plot_sample(sample_df, desc=paste('alpha:',paste(alpha, collapse = ", ")))
#   print(plt_image)
# }


# plots <- list()
# for (i in 1:number_of_y){
#   plots[[i]] <- plot_y(sample_df, i)
# }
# 
# # Calculate number of rows and columns dynamically
# ncol <- 2  # Number of columns
# nrow <- ceiling(number_of_y / ncol)  # Number of rows
# 
# # Ensure the grid has enough cells
# stopifnot(nrow * ncol >= length(plots))
# 
# # Arrange the plots dynamically and add a title
# grid.arrange(
#   grobs = plots,
#   ncol = ncol,
#   nrow = nrow,
#   top = textGrob("Density Plots of Categories", gp = gpar(fontsize = 16, fontface = "bold"))
# )
