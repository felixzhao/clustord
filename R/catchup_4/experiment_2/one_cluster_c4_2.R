library(ggplot2)
library(gridExtra)

set.seed(123)  # Setting seed for reproducibility

###

q=3
p=5
alpha=c(-1) 
beta=c(-0.0001, -0.02, 0.02, 0.9, 1.2)
mu=c(0, 0.8, 0.6) 
phi=c(0, 0.7,1)

result_list <- list()
G=1
for ( c in 1:p){
  print(paste("c",c))
  for (g in 1:G) {
    probs <- numeric(q)
    for (k in 1:q) {
      linear <- mu[k] + phi[k] * (alpha[g] + beta[c])
      if (k > 1) {
        probs[k] <- exp(linear)
      } else {
        probs[k] <- 1
      }
    }
    
    prob <- probs/sum(probs)
    print(probs)
    print(prob)
    data_val <- sample(1:q, size = 1000, replace = TRUE, prob = prob)
    result_list[[paste0("Y", c)]] <- data_val
  }
}

result_df <- data.frame(result_list)
# print(result_df)

#####

# Create a list to store the plots
plot_list <- list()

for (c in 1:p){
  col_name <- paste0("Y", c)
  plt <- ggplot(result_df, aes(x = Y1)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = "title",
         x = col_name,
         y = "Density") +
    theme_minimal()
  plot_list[[c]] <- plt
}

grid.arrange(grobs = plot_list, ncol=3)

