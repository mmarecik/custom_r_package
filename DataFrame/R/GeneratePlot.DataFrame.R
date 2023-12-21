GeneratePlot.DataFrame <-
function(data, column, plot_type = "distribution") {
  # Check if the specified column exists in the data frame
  if (!(column %in% names(data))) {
    stop("Specified column not found in the data frame.")
  }
  
  # Generate either a distribution plot or a boxplot based on the plot_type
  if (plot_type == "distribution") {
    # Create a distribution plot for the specified column
    plot <- plot(density(data[[column]]), main = paste("Distribution Plot for", column),
                 xlab = column, col = "skyblue", lwd = 2)
    polygon(density(data[[column]]), col = rgb(0.2, 0.5, 0.8, 0.5), border = NA)
  } else if (plot_type == "boxplot") {
    # Create a boxplot for the specified column
    plot <- boxplot(data[[column]], main = paste("Boxplot for", column),
                    xlab = column, col = "skyblue", border = "darkblue", plot = FALSE)
  } else {
    stop("Invalid plot_type. Supported types are 'distribution' and 'boxplot'.")
  }
  
  return(plot)
}
