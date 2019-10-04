# Extended Residual Calculations.

# Calculates residuals beyond what 'resid' can do
helper_resid <- function(type = NA, model){

  # lm residuals
  if (class(model)[1] == "lm") {

    # Default: raw residuals
    if (is.na(type) | type == "response") {
      return(resid(model, type = "response"))
    } else if(type == "pearson") {
      return(resid(model, type = "response") / summary(model)$sigma)
    } else if(type == "standardized") {
      return(stdres(model))
    }

  # glm residuals
  } else if (class(model)[1] == "glm"){

    # Default: deviance residuals
    if (is.na(type) | type == "deviance") {
      return(resid(model, type = "deviance"))
    } else if (type == "response") {
      return(resid(model, type = "response"))
    } else if (type == "pearson") {
      return(resid(model, type = "pearson"))
    } else if (type == "stand.deviance") {
      return((resid(model, type = "deviance")) / (sqrt(summary(model)$dispersion*(1 - hatvalues(model)))))
    } else if (type == "stand.pearson") {
      return((resid(model, type = "pearson")) / (sqrt(summary(model)$dispersion*(1 - hatvalues(model)))))
    }

  # lme residuals
  } else if (class(model)[1] == "lme") {

    # Default: Pearson residuals (condtional on BLUPs)
    if (is.na(type) | type == "pearson") {
      return(resid(model, type = "response") / summary(model)$sigma)
    } else if (type == "response") {
      return(resid(model, type = "response"))
    }
    
  # lmer and lmerTest residuals
  } else if (class(model)[1] %in% c("lmerMod", "lmerModLmerTest")) {
    
    # Default: Standardized conditional (condtional on BLUPs)
    if (is.na(type) | type == "stand.cond") {
      return(redres::compute_redres(model, type = "std_cond"))
    } else if (type == "stand.mar") {
      return(redres::compute_redres(model, type = "std_mar"))
    } else if (type == "response.cond") {
      return(redres::compute_redres(model, type = "raw_cond"))
    } else if (type == "response.mar") {
      return(redres::compute_redres(model, type = "raw_mar"))
    } else if (type == "pearson.cond") {
      return(redres::compute_redres(model, type = "pearson_cond"))
    } else if (type == "pearson.mar") {
      return(redres::compute_redres(model, type = "pearson_mar"))
    } 

  # glmer residuals
  } else if (class(model)[1] == "glmerMod") {

    # Default: deviance residuals
    if (is.na(type) | type == "deviance") {
      return(resid(model, type = "deviance"))
    } else if (type == "response") {
      return(resid(model, type = "response"))
    } else if (type == "pearson") {
      return(resid(model, type = "pearson"))
    }
   
  }

}

