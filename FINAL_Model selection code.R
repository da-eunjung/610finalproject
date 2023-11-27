# Simulating data 
set.seed(1234)

# Generating the covariates and the error 
x1 <- rnorm(1000, 10, 2)
x2 <- rnorm(1000, 50, 3)
x3 <- rnorm(1000, 0, 10)
x4 <- rnorm(1000, 25, 5)
x5 <- rnorm(1000, 117, 12)
x6 <- rnorm(1000, 1200, 43)
x7 <- rnorm(1000, 82, 8)
x8 <- rnorm(1000, 45, 4.5)
error <- rnorm(1000, 0, 3)

# Generate the dependent variable 
y1 = 33+(4*x1)-(3*x2)+(1.5*x3)+(0*x4)+(8*x5)-(0.5*x6)+(0.1*x7)-(5*x8)+error

data <- data.frame(y1, x1, x2, x3, x4, x5, x6, x7, x8)
outcome = "y1"
predictors = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")

forward_stepwise_selection <- function(data, outcome, predictors) {
	best_models <- list() # Saving this as an empty list just to set up
	list_rss <- list()
	
	for (i in 1:length(predictors)) {
		vc <- combn(predictors, i)
		rss_values <- numeric(ncol(vc))
		
		for (j in 1:ncol(vc)){
			model_formula <- as.formula(paste0(outcome, "~", paste0(vc[,j], collapse = "+")))
			current_model <- lm(model_formula, data)
			
			rss <- sum(residuals(current_model)^2)
			rss_values[j] <- rss
		}
		
		list_rss[[i]] <- rss_values
		
		# Find the index of the minimum RSS for this set of predictors
		best_rss_index <- which.min(rss_values)
		
		# Save the best model for the current number of predictors
		best_model <- list(model = summary(lm(as.formula(paste0(outcome, "~", paste0(vc[, best_rss_index], collapse = "+"))), data)),
						   rss = rss_values[best_rss_index],
						   predictors = vc[, best_rss_index])
		
		best_models[[i]] <- best_model
		
		# Print the best model and its RSS for the current number of predictors
		cat("Best model with", i, "predictors:", paste(best_model$predictors, collapse = " + "), "\n")
		cat("RSS:", best_model$rss, "\n\n")
	}
	
	return(list(best_models = best_models, list_rss = list_rss))
}

result <- forward_stepwise_selection(data = data, outcome = outcome, predictors = predictors)

list_rss <- result$list_rss
min_values <- sapply(list_rss, min)

