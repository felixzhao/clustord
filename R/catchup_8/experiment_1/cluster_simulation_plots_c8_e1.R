library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)

set.seed(123)

source("R/catchup_8/experiment_1/OSM_Col_effect_functions.R")
source("R/catchup_8/experiment_1/plots_functions.R")

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
# desc = paste('pi:',paste(cluster_pi, collapse = ", "))
# 
# cluster_probs <- generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
#                                         sample_size, total_sample_size, number_of_y )
# sample_df <- data_samping(sample_size, total_sample_size, cluster_pi, q, 
#                           cluster_probs, number_of_y)
# plt_image <- plot_sample(sample_df, desc=desc)
# print(plt_image)

# batch plots

# alpha
alpha_list <- list(
  c(-0.1, 0.1),
  #c(-0.5, 0.5), 
  c(-1,1), 
  #c(-2,2), 
  c(-3,3)
)

plots <- list()
# for (cur_alpha in alpha_list){
for (i in seq_along(alpha_list)) {
  cur_alpha <- alpha_list[[i]]
  desc = paste('alpha:',paste(cur_alpha, collapse = ", "))
  cluster_probs <- generate_cluster_probs(G, q, cur_alpha, beta, mu, phi, cluster_pi, 
                                          sample_size, total_sample_size, number_of_y )
  sample_df <- data_samping(sample_size, total_sample_size, cluster_pi, q, 
                            cluster_probs, number_of_y)
  plt_image <- plot_sample(sample_df, desc=desc)
  # print(plt_image)
  plots[[i]] <-plt_image
}

plots_title <- "Density Plots of Categories"
save_path <- paste0("/Users/felixzhao/Documents/workspace/STAT489/report/images/para_sim/","alpha.png")
show_plots(plots, plots_title, save_path)
