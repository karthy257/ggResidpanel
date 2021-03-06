# Residual Plot.

# Creates a residual plot with the input residuals and predicted values
plot_auxresid <- function(resid, pred, smoother, theme, axis.text.size, title.text.size,
                          title.opt){

  ## Creation of Values to Plot -----------------------------------------------------

  # Create a data frame with the residuals
  model_values <- data.frame(Residual = resid, Prediction = pred)

  # Compute the values for the lowess curve
  model_values$Lowess.x <- lowess(x = model_values$Prediction, y = model_values$Residual)$x
  model_values$Lowess.y <- lowess(x = model_values$Prediction, y = model_values$Residual)$y

  ## Creation of Plot ---------------------------------------------------------------

  # Create the residual plot
  plot <- ggplot(data = model_values, aes_string(x = "Prediction", y = "Residual")) +
    geom_point() +
    geom_abline(slope = 0, intercept = 0, color = "blue") +
    labs(x = "Predicted Values", y = "Residuals")


  # If smoother is set to true, add it to the plot
  if (smoother == TRUE){
   plot <- plot +
     geom_line(aes_string(x = "Lowess.x", y = "Lowess.y"), colour = "red", size = 0.5)
  }

  # Add theme to plot
  if (theme == "bw"){
    plot <- plot + theme_bw()
  } else if (theme == "classic"){
    plot <- plot + theme_classic()
  } else if (theme == "gray" | theme == "grey"){
    plot <- plot + theme_grey()
  }

  # Set text size of title and axis lables, determine whether to include a title,
  # and return plot
  if(title.opt == TRUE){
    plot +
      labs(title = "Residual Plot") +
      theme(plot.title = element_text(size = title.text.size, face = "bold"),
            axis.title = element_text(size = axis.text.size))
  } else if (title.opt == FALSE){
    plot + theme(axis.title = element_text(size = axis.text.size))
  }

}
