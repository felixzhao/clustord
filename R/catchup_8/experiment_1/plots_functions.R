# Plot
plot_sample <- function(df, desc, title="", y_idx=1) {
  sample_name <- paste0("Y", y_idx)
  # Plot
  data1 <- data.frame(Sample = df[[sample_name]], Cluster = as.factor(df$cluster))
  
  plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
    geom_density(alpha = 0.5) +
    # geom_bar(position = "dodge") +
    # geom_bar() +
    labs(title = title,
         x = "Sample Value",
         y = "Density",
         caption = desc) +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    theme_minimal()
  return(plot)
}

plot_sample_bar <- function(df, desc, title="", y_idx=1) {
  sample_name <- paste0("Y", y_idx)
  # Plot
  data1 <- data.frame(Sample = df[[sample_name]], Cluster = as.factor(df$cluster))
  
  plot <- ggplot(data1, aes(x = Sample, fill = Cluster)) +
    # geom_density(alpha = 0.5) +
    geom_bar(position = "dodge") +
    # geom_bar() +
    labs(title = title,
         x = "Sample Value",
         y = "Density",
         caption = desc) +
    scale_fill_brewer(palette = "Set1", name = "Cluster") +
    theme_minimal()
  return(plot)
}

show_plots <- function(plots, title, file_path){
  ncol <- 3  # Number of columns
  nrow <-  ceiling(length(plots) / ncol)  # Number of rows
  
  # Ensure the grid has enough cells
  stopifnot(nrow * ncol >= length(plots))
  
  # Create a grid layout with the title
  arranged_plots <- grid.arrange(
    grobs = plots,
    ncol = ncol,
    nrow = nrow,
    top = textGrob(title, gp = gpar(fontsize = 16, fontface = "bold"))
  )
  
  # Display the arranged plots on the screen
  print(arranged_plots)
  
  # Check if file_path is provided, then save the plot
  if (!is.null(file_path) && file_path != "") {
    ggsave(file_path, arranged_plots, width = ncol * 4, height = nrow * 4)
  } else {
    message("file_path is empty. Skipping the save step.")
  }
}