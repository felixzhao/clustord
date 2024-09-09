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

# beta
# reset_default_para_values()
# number_of_y = 10
# beta_list <- list(
#   c(-0.1, 0.1),
#   c(-1,1), 
#   c(-3,3)
# )
# cluster_probs_list <- lapply(beta_list, function(beta) {
#   generate_cluster_probs(G, q, alpha, beta, mu, phi, cluster_pi, 
#                          sample_size, total_sample_size, number_of_y)
# })
# batch_plots(beta_list, 'beta', cluster_probs_list)

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


