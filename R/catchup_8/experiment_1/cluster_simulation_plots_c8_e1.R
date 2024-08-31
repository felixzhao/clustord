library(rlang)
library(ggplot2)
library(gridExtra)
library(grid)
library(purrr)

set.seed(123)

source("R/catchup_8/experiment_1/OSM_Col_effect_functions.R")

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
