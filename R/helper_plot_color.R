# Labels for Plotly Points.

# Creates a label for each data point in the residual plots
helper_plot_color <- function(model){

###############################################################
#Create Data to use as labels
#The only way to get plotly to plot the y, the x's, and the observation when
#it is not included in aes() is to create a vector where each value of that
#Vector contains all of the above information. The plotly option 'tooltips' can
#then be used to select which variables to print in plotly which would be the
#single variable 'Data' containing all the above information

#The methods for acquiring the data are different for mixed models so separated
#based on model type
if(class(model)[1]%in%c("lm", "glm")){
  #Get names of variables
  names_data <- names(model$model)
  #Get data used in model from model
  #color_data <- data.frame(as.matrix(model$model))

  color_data <- model$model

  #If binomial, the response variables are two columns
  if(class(model)[1]=="glm"){
    if(model$family[[1]]=="binomial"){
      color_data <- color_data[,-1]
      color_data_Y <- data.frame(as.matrix(model$model))[,1:2]

      names(color_data_Y)[1] <- "Success"
      names(color_data_Y)[2] <- "Total"

      color_data <- cbind(color_data_Y, color_data)
      color_data$Success <- as.numeric(as.character(color_data$Success))
      color_data$Total <- as.numeric(as.character(color_data$Total))

      color_data$Total <- color_data$Total+color_data$Success

      }
  }
  #Create a variable containing the observation number
  color_data$Obs <- 1:nrow(color_data)

  
  
} else if (class(model)[1]%in%c("lmerMod", "lmerModLmerTest", "glmerMod")) {
  names_data <- names(model@frame)

  # need to create data set this way to maintain types of input variables (factor, numeric, etc)
  color_data <- model@frame
  #If binomial, the response variables are two columsn
  if(class(model)[1]=="glmerMod"){
    if(model@resp$family[[1]]=="binomial"){
      # Remove first column
      color_data <- color_data[,-1]
      color_data_Y <- data.frame(as.matrix(model@frame))[,1:2]
      
      names(color_data_Y)[1] <- "Success"
      names(color_data_Y)[2] <- "Total"
      color_data <- cbind(color_data_Y, color_data)

      # Convert to numeric
      color_data$Success <- as.numeric(as.character(color_data$Success))
      color_data$Total <- as.numeric(as.character(color_data$Total))

      color_data$Total <- color_data$Total+color_data$Success

    }
  }

  color_data$Obs <- 1:nrow(color_data)


} else if (class(model)[1] == "lme") {
  names_data <- names(model$data)

  color_data <- model$data

  color_data$Obs <- 1:nrow(color_data)
  }
return(color_data)
#################################################################

}
