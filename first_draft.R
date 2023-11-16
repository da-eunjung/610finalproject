library(dplyr)

data <- mtcars
outcome <- mpg

predictors <- c(cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)

backward_stepwise_selection <- function(data, outcome, predictors) {
  initial_formula <- as.formula(paste(outcome, ~, paste(predictors, collapse = +)))
  lm_model <- lm(initial_formula, data = data)
  
  while (length(predictors) > 1) {
    rss_list <- c()
    candidate_models <- list()
    rss_list <- numeric()  # Initialize inside the while loop
    candidate_models <- list()  # Initialize inside the while loop
    
    for (i in 1:length(predictors)) {
      subset_predictors <- setdiff(predictors, predictors[i])
      candidate_formula <- as.formula(paste(outcome, ~, paste(subset_predictors, collapse = +)))
      candidate_models[[i]] <- lm(candidate_formula, data = data)
      rss_list[i] <- sum(resid(candidate_models[[i]])^2)
    }
    
    best_rss <- which.min(rss_list)
    best_model <- candidate_models[[best_rss]]
    predictors <- setdiff(predictors, names(coef(best_model)))
  }
  return(best_model)
}




  
backward_stepwise_selection(data, outcome, predictors)

# Simulated data
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 2*x1 + 3*x2 + 4*x3 + rnorm(n)

data <- data.frame(x1, x2, x3, y)
outcome <- y
predictors <- c(x1, x2, x3)

# Call the function
result_model <- backward_stepwise_selection(data, outcome, predictors)
