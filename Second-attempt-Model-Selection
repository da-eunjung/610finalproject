forward_stepwise_selection <- function(data, outcome, predictors) {
  best_models <- list() # Saving this as an empty list just to set up
  
  for (i in 1:length(predictors)) {  #function to come up with all the possible combinations of predictors
    vc <- combn(predictors,i)
    best_rss <- Inf # Setting the best_rss score to positive infinity, just as a base line
    
    for (j in 1:ncol(vc)){
      model_formula <- as.formula(paste0(outcome, "~", paste0(vc[,j], collapse = "+")))
      current_model <- lm(model_formula, data)
      
      
      rss <- sum(residuals(current_model)^2)	
      
      # Check if the current model has a lower RSS than the best model so far
      if (rss < best_rss) {
        best_rss <- rss
        best_model <- list(model = summary(current_model), rss = rss, predictors = vc[, j])
      }
    }
    # Save the best model for the current number of predictors
    best_models[[i]] <- best_model
    
    # Print the best model and its RSS for the current number of predictors
    cat("Best model with", i, "predictors:", paste(best_model$predictors, collapse = " + "), "\n")
    cat("RSS:", best_model$rss, "\n\n")
  }
}

