# install.packages("dplyr")

library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)
library(dplyr)

set.seed(123)

source("R/catchup_9/osm_col_effect_functions.R")
# prob matrix for each Y, in current stage suppose all same.

# default parameters
number_of_y <- 20
G <- 3
q <- 3
alpha <- c(-1,0,1)
beta <- runif(number_of_y, min = -1, max = 1) #c(-1, -0.5, 0, 0.5, 1)
mu <- c(0, 0, 0)
phi <- c(0, 0.5, 1)
cluster_pi <- c(0.3, 0.4, 0.3)
sample_size <- 1500
total_sample_size <- sample_size * G



cluster_probs <- get_cluster_prob_matrix(mu, phi, alpha, beta, cluster_pi, number_of_y)

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
  
  # dataframes <- 
  #   
  #   lapply(1:number_of_y, function(i) {
  #   # col_cluster_probs <- cluster_probs[[]]
  #   #   lapply(cluster_probs, function(cluster) {
  #   #   sapply(cluster, function(sublist) sublist[i])
  #   # })
  #   y_sampling(sample_size, total_sample_size, cluster_pi, q, cluster_probs, i)
  #   })
  # 
   # Merge all dataframes
   # merged_df <- reduce(data_list, function(df1, df2) {
   #   inner_join(df1, df2, by = c("cluster", "id"))
   # })
  
   # Remove the unique identifier column
   # merged_df <- res %>% select(-id)
   # 
   # return(merged_df)

}

save_data <- function(df, save_path = "./data/simulation_catgories_n_cluster_c9_1.csv"){
  # save to csv file
  write.csv(df, save_path , row.names=FALSE)
  print(paste("Save data to",save_path,"Done."))
}

# Plot
plot_y <- function(df, y_idx) {
  sample_name <- paste0("Y", y_idx)
  # Plot
  data1 <- data.frame(Sample = df[[sample_name]], Cluster = as.factor(df$cluster))
  
  plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    labs(title = sample_name,
         x = "Sample Value",
         y = "Density") +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    theme_minimal()
}

plot_all_y <- function(df, number_of_y, number_of_y_for_print=10, n_print_col=3)  { 
  # df <- df[,1:3]
  plots <- list()
  for (i in 1:number_of_y_for_print){
    plots[[i]] <- plot_y(df, i)
  }
  
  # Calculate number of rows and columns dynamically
  ncol <- n_print_col # Number of columns in plot
  nrow <- ceiling(number_of_y_for_print / ncol)  # Number of rows
  
  # Ensure the grid has enough cells
  stopifnot(nrow * ncol >= length(plots))
  
  # Arrange the plots dynamically and add a title
  grid.arrange(
    grobs = plots,
    ncol = ncol,
    nrow = nrow,
    top = textGrob(paste("TOP 10 of", number_of_y,"Y Density Plots by Categories"), 
                   gp = gpar(fontsize = 16, fontface = "bold"))
  )
}

# 10 Y
save_path <- paste0("./data/simulation_y_",number_of_y,"_c9_6.csv")

df_10_y <- data_samping(sample_size, total_sample_size, cluster_pi, q, 
                         cluster_probs, number_of_y
                         )
# plot_all_y(df_10_y, number_of_y)
save_data(df_10_y, save_path = save_path)
