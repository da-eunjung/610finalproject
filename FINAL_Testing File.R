library(testthat)

setwd("C:/Users/Roberto Ortiz/Desktop/Grad School Stuff/Year 2/Intro to Stats Computing/610 Project/")
source("FINAL_Model Selection Function and Simulation.R")

# Create a test case
test_that("best_rss is smaller than every other RSS", {
  # Call the function to obtain the best models
  result <- forward_stepwise_selection(data = data, outcome = outcome, predictors = predictors)
  best_models <- result$best_models
  all_models <- result$all_models
  
  # Check if best_rss is smaller than every other RSS
  for (i in seq_along(best_models)) {
    best_rss <- best_models[[i]]$rss
    other_rss <- sapply(all_models[[i]], function(model_info) model_info$rss)
    
    expect_true(all(best_rss < other_rss))
  }
})

# Plotting the residuals
df <- data.frame(
rss = min_values)
df$predictors <- seq_len(nrow(df))

library(ggplot2)
ggplot(df, aes(x=predictors, y= rss)) + 
  geom_line() + 
  geom_point() +
  labs(title = "Residual Sum of Squares by Number of Predictors")