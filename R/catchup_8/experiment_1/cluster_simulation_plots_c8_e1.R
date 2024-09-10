library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)

set.seed(123)

source("R/catchup_8/experiment_1/OSM_Col_effect_functions.R")
source("R/catchup_8/experiment_1/plots_functions.R")

batch_plots <- function(para_list, para_name, cluster_probs, cluster_pi = c(0.5, 0.5)){
  plots_title <- paste(default_plots_title, para_name)
  save_path <- paste0(default_save_path,para_name,'.png')
  plots <- list()
  for (i in seq_along(para_list)) {
    cur_alpha <- para_list[[i]]
    desc = paste(para_name,": ",paste(cur_alpha, collapse = ", "))
    sample_df <- data_samping(sample_size, total_sample_size, cluster_pi, q,
                              cluster_probs[[i]], number_of_y)
    plt_image <- plot_sample(sample_df, desc=desc)
    plots[[i]] <-plt_image
  }
  show_plots(plots, plots_title, save_path)
}

# default parameters 1
# G=2
# q=3
# alpha=c(-3,3)
# beta=c(0)
# mu=c(0.3, 0.3, 0.3)
# phi=c(0, 0.5, 1)
# cluster_pi = c(0.3, 0.7)
# sample_size <- 1000
# total_sample_size <- sample_size * G
# 
# number_of_y = 1
# 
# reset_default_para_values <- function(){
#   G=2
#   q=3
#   alpha=c(-3,3)
#   beta=c(0)
#   mu=c(0.3, 0.3, 0.3)
#   phi=c(0, 0.5, 1)
#   cluster_pi = c(0.3, 0.7)
#   sample_size <- 1000
#   total_sample_size <- sample_size * G
#   
#   number_of_y = 1
# }

# default parameters 2
G=2
q=3
alpha=c(-1,1)
beta=c(0)
mu=c(0, 0, 0)
phi=c(0, 0.5, 1)
cluster_pi = c(0.5, 0.5)
sample_size <- 1000
total_sample_size <- sample_size * G

number_of_y = 1

default_plots_title <- "Density Plots of Categories for different value of"
default_save_path <- "/Users/felixzhao/Documents/workspace/STAT489/report/images/para_sim/"

reset_default_para_values <- function(){
  G<<-2
  q<<-3
  alpha<<-c(-1,1)
  beta<<-c(0)
  mu<<-c(0, 0, 0)#c(0, 0.2, 0.7)
  phi<<-c(0, 0.5, 1)
  cluster_pi <<- c(0.5, 0.5)
  sample_size <<- 1000
  total_sample_size <<- sample_size * G
  
  number_of_y <<- 1
}

# batch plots

# alpha
reset_default_para_values()
alpha_list <- list(
  c(-0.1, 0.1),
  c(-1,1), 
  c(-3,3)
)
cluster_probs_list <- lapply(alpha_list, function(alpha) {
  generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
                         sample_size, total_sample_size, number_of_y)
})
batch_plots(alpha_list, 'alpha', cluster_probs_list)

# ---------------------------#beta ------------------------
# beta
reset_default_para_values()
number_of_y = 5
beta_list <- list(
  c(-0.1, -0.05, 0, 0.05, 0.1),
  c(-1, -0.5, 0, 0.5, 1),
  c(-3, -1.5, 0, 1.5, 3)
)
cluster_probs_list <- lapply(beta_list, function(beta) {
  generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi,
                         sample_size, total_sample_size, number_of_y)
})

#----------------  beta plot
para_name <- "beta"
plots_title <- paste(default_plots_title, para_name)
save_path <- paste0(default_save_path,para_name)
beta_plots <- list()
for (i in seq_along(beta_list)) {
  cur_beta <- beta_list[[i]]
  
  sample_df <- data_samping(sample_size, total_sample_size, cluster_pi, q,
                            cluster_probs_list[[i]], number_of_y)
  y_plots <- list()
  for (j in seq_along(beta_list[[1]])){
    desc = paste(para_name,": ",paste(cur_beta[[j]], collapse = ", "))
    plt_image <- plot_sample(sample_df, desc=desc, y_idx=j)
    y_plots[[j]] <-plt_image
  }
  # show_plots(plots, plots_title, save_path)
  beta_plots[[i]] <- y_plots
}

for (i in seq_along(beta_list)){
  print(i)


  # Extract Y-limits from all plots to determine the common range
  y_limits <- range(unlist(lapply(beta_plots[[beta_idx]], function(plot) {
    layer_data(plot)$y  # Extract the y-values from each plot
  })))
  
  # Set the same Y-limits for each plot in beta_plots[[1]]
  beta_plots_with_same_y <- lapply(beta_plots[[beta_idx]], function(plot) {
    plot + ylim(y_limits)  # Add common ylim to each plot
  })
  
  title <- paste0("Density Plots of Categories for Ys for Beta",
                  paste(": ",paste(beta_list[[beta_idx]], collapse = ", ")))
  arranged_plots <- grid.arrange(
    grobs = beta_plots[[beta_idx]],
    ncol = 5,
    nrow = 1,
    top = textGrob(title, gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  # Display the arranged plots on the screen
  print(arranged_plots)
  
  # save plot
  file_path <- paste0(save_path, "_", beta_idx, ".png")
  # Check if file_path is provided, then save the plot
  if (!is.null(file_path) && file_path != "") {
    ggsave(file_path, arranged_plots, width = 5 * 4, height = 1 * 4)
  } else {
    message("file_path is empty. Skipping the save step.")
  }
}

# ---------------------------#beta end ------------------------

# mu
reset_default_para_values()
mu_list <- list(
  c(0, -1, -2)
  ,c(0, 2, 1)
  ,c(0, 1, 2)
)
cluster_probs_list <- lapply(mu_list, function(mu) {
  generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
                         sample_size, total_sample_size, number_of_y)
})
batch_plots(mu_list, 'mu', cluster_probs_list)

# phi
reset_default_para_values()
phi_list <- list(
  c(0, 0.2, 1)
  ,c(0, 0.5, 1)
  ,c(0, 0.8, 1)
)
cluster_probs_list <- lapply(phi_list, function(phi) {
  generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
                         sample_size, total_sample_size, number_of_y)
})
batch_plots(phi_list, 'phi', cluster_probs_list)

# pi
reset_default_para_values()
## Generate sequences for p1 and p2
# p1_values <- seq(0.1, 0.9, by = 0.1)
# p2_values <- rev(p1_values)

## Combine p1 and p2 into a list of pairs
# pi_list <- mapply(c, p1_values, p2_values, SIMPLIFY = FALSE)
pi_list <- list(
  c(0.1, 0.9)
  ,c(0.5, 0.5)
  ,c(0.9, 0.1)
)

cluster_probs <- generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
                       sample_size, total_sample_size, number_of_y)

# plot pi
para_list <- pi_list
para_name <- "pi"
plots_title <- paste(default_plots_title, para_name)
save_path <- paste0(default_save_path,para_name,'.png')
plots <- list()
for (i in seq_along(para_list)) {
  cur_alpha <- para_list[[i]]
  desc = paste(para_name,": ",paste(cur_alpha, collapse = ", "))
  sample_df <- data_samping(sample_size, total_sample_size, pi_list[[i]], q,
                            cluster_probs, number_of_y)
  plt_image <- plot_sample_bar(sample_df, desc=desc)
  plots[[i]] <-plt_image
}
show_plots(plots, plots_title, save_path)


