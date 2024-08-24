library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)

set.seed(123)

G=2
q=3
alpha=c(1,-1)
beta=c(0)
mu=c(0, 0.6, 0.3)
phi=c(0, 0.8, 1)
cluster_pi = c(0.3, 0.7)
sample_size <- 1000
total_sample_size <- sample_size * G

number_of_y = 1

# functions
generate_cluster_probs <- function(
    G=2, 
    q=3, 
    alpha=c(1,-1), 
    beta=c(0),
    mu=c(0, 0.6, 0.3) ,
    phi=c(0, 0.8, 1),
    cluster_pi = c(0.3, 0.7),
    sample_size = 1000,
    total_sample_size = sample_size * G,
    number_of_y = 1
    ){
  cluster_probs <- lapply(1:G, function(x) numeric(q))
  
  for (g in 1:G) {
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
        print(paste( g,k,j))
        print(paste( mu[k], phi[k], alpha[g], beta[j]))
        print(paste('linear', linear))
      }
      print(paste('probs', probs))
      print(paste('norm probs', probs / sum(probs)))
      category_probs[[k]] <- probs #/ sum(probs) # normalise k for each j # 2 dim, j, k
    }
    sum_flattened_list <- sum(unlist(category_probs))
    cluster_probs[[g]] <- category_probs # 3 dim, g, j, k
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

data_samping <- function(G=2, 
                         q=3, 
                         alpha=c(1,-1), 
                         beta=c(0),
                         mu=c(0, 0.6, 0.3) ,
                         phi=c(0, 0.8, 1),
                         cluster_pi = c(0.3, 0.7),
                         sample_size = 1000,
                         total_sample_size = sample_size * G,
                         number_of_y = 1) {
  
  # cluster_probs <- generate_cluster_probs(G, 
  #                                         q, 
  #                                         alpha, 
  #                                         beta,
  #                                         mu,
  #                                         phi,
  #                                         cluster_pi,
  #                                         sample_size,
  #                                         total_sample_size,
  #                                         number_of_y)
  
  # Create a list of N dataframes
  dataframes <- lapply(1:number_of_y, function(i) {
    col_cluster_probs <- lapply(cluster_probs, function(cluster) {
      sapply(cluster, function(sublist) sublist[i])
    })
    y_sampling(sample_size, total_sample_size, cluster_pi, q, col_cluster_probs, i)
  })
  
  # Merge all dataframes
  merged_df <- reduce(dataframes, function(df1, df2) {
    inner_join(df1, df2, by = c("cluster", "id"))
  })
  
  # Remove the unique identifier column
  merged_df <- merged_df %>% select(-id)
  
  return(merged_df)
  
}

# main
sample_df <- data_samping(sample_size, total_sample_size, cluster_pi, q, 
                        cluster_probs, number_of_y
)


plt_image <- plot_sample(sample_df, 1)
print(plt_image)

# alpha 
alpha_list <- list(
  list(-1,0), list(-1,1), list(0,1)
)

for (cur_alpha in alpha_list){
  sample_df <- data_samping(alpha=cur_alpha)
  plt_image <- plot_sample(sample_df, desc=paste('alpha:', cur_alpha))
  print(plt_image)
}


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
