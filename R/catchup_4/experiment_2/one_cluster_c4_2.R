library(ggplot2)
library(gridExtra)

set.seed(123)  # Setting seed for reproducibility

###

q=3
p=5
alpha=c(-1) 
beta=c(0.001, 0.02, 0.2, 0.5, 0.9)
mu=c(0, 0.8, 0.6) 
phi=c(0, 0.7,1)

result_list <- list()
G=1
for ( c in 1:p){
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
print(result_df)
# return(result_df)

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

#########


ord_cluster_simulate.plot <- function( q=3, p=5,
                                       alpha=c(-1), beta=c(0, 0.2, 0.2, 0.5, 0.1),
                                       mu=c(0, 0.8, 0.6), phi=c(0, 0.7,1), 
                                       title="Overall Density Plot of Sampled Data") {
  
  
  result_list <- list()
  G=1
  for ( c in 1:p){
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
  print(result_df)
  return(result_df)
  
  # due to G=1, thus can plot directly
  
  # Creating a data frame for ggplot
  samples <- data.frame(data_val = data_val)
  
  # Round probs to 2 decimal places and convert to string
  probs_str <- paste(sprintf("%.2f", probs), collapse = ", ")
  prob_str <- paste(sprintf("%.2f", prob), collapse = ", ")
  
  # Generating the density plot
  return( ggplot(samples, aes(x = data_val)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = title,
         x = paste("probs:", probs_str, "\nprob:", prob_str),
         y = "Density") +
    theme_minimal()
  )
}

# diff alpha
# no mean to have multi alpha, due to only has one cluster
# thus, alpha always take the first
q <- 3
alpha=c(0.08)
title_str <- paste("alpha:", alpha)
p1 <- ord_cluster_simulate.plot(q=q, alpha=alpha, title= title_str)
alpha=c(1)
title_str <- paste("alpha:", alpha)
p2 <- ord_cluster_simulate.plot(q=q, alpha=alpha, title= title_str)
alpha=c(3)
title_str <- paste("alpha:", alpha)
p3 <- ord_cluster_simulate.plot(q=q, alpha=alpha, title= title_str)

# grid.arrange(p1, p2, p3, ncol = 3)

# diff mu
mu <- c(0, 0.7, 0.5)
title_str <- paste("mu:", paste(sprintf("%.2f", mu), collapse = ", "))
p4 <- ord_cluster_simulate.plot(mu=mu, title= title_str)
mu <- c(0, 0.2, 0.9)
title_str <- paste("mu:", paste(sprintf("%.2f", mu), collapse = ", "))
p5 <- ord_cluster_simulate.plot(mu=mu, title= title_str)
mu <- c(0, 0.9, 0.2)
title_str <- paste("mu:", paste(sprintf("%.2f", mu), collapse = ", "))
p6 <- ord_cluster_simulate.plot(mu=mu, title= title_str)

# grid.arrange(p4, p5, p6, ncol = 3)

# diff phi

phi <- c(0, 0.2, 1)
title_str <- paste("phi:", paste(sprintf("%.2f", phi), collapse = ", "))
p7 <- ord_cluster_simulate.plot(phi = phi, title= title_str)
phi <- c(0, 0.5, 1)
title_str <- paste("phi:", paste(sprintf("%.2f", phi), collapse = ", "))
p8 <- ord_cluster_simulate.plot(phi = phi, title= title_str)
phi <- c(0, 0.9, 1)
title_str <- paste("phi:", paste(sprintf("%.2f", phi), collapse = ", "))
p9 <- ord_cluster_simulate.plot(phi = phi, title= title_str)

# plot
grid.arrange(p1, p2, p3,p4, p5, p6,p7, p8, p9, ncol = 3, nrow = 3)
