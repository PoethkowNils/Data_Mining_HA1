nipoet_forward_selection <- function(data, response, threshold_p = 0.05) {
  
  # Step 1: Separate the response and predictors
  y <- as.matrix(data[[response]])  # Response vector
  X_all <- as.matrix(data[, setdiff(names(data), response)])  # All predictors
  n <- nrow(X_all)  # Number of observations
  k <- ncol(X_all)  # Number of potential predictors
  
  # Initialize variables for forward selection
  selected_vars <- c()  # No predictors selected initially
  remaining_vars <- 1:k  # Indices of variables available for selection
  X_selected <- matrix(1, nrow=n, ncol=1)  # Start with intercept-only model
  best_model <- NULL
  
  # Function to calculate coefficients manually
  calculate_coefficients <- function(X, y) {
    return(solve(t(X) %*% X) %*% t(X) %*% y)
  }
  
  # Function to calculate F-statistic
  calculate_f_statistic <- function(X, y, beta) {
    y_hat <- X %*% beta  # Predicted values
    residuals <- y - y_hat
    rss <- sum(residuals^2)  # Residual Sum of Squares
    tss <- sum((y - mean(y))^2)  # Total Sum of Squares
    r_squared <- 1 - (rss / tss)  # R-squared
    
    # Degrees of freedom
    p <- ncol(X) - 1  # Number of predictors (excluding intercept)
    df_model <- p
    df_error <- n - p - 1
    
    # F-statistic
    f_statistic <- (r_squared / df_model) / ((1 - r_squared) / df_error)
    return(f_statistic)
  }
  
  # Step 2: Forward selection process
  while (length(remaining_vars) > 0) {
    best_p_value <- Inf  # Initialize p-value for comparison
    best_var <- NULL  # Best variable to add
    
    # Try adding each variable and calculate the F-statistic
    for (var in remaining_vars) {
      X_temp <- cbind(X_selected, X_all[, var])  # Add the candidate variable to the model
      beta <- calculate_coefficients(X_temp, y)  # Calculate coefficients
      f_statistic <- calculate_f_statistic(X_temp, y, beta)  # Calculate F-statistic
      
      # Calculate the p-value from the F-statistic
      df_model <- ncol(X_temp) - 1
      df_error <- n - ncol(X_temp)
      p_value <- pf(f_statistic, df_model, df_error, lower.tail = FALSE)    # for the p value, a library is used
      
      # Check if this is the best model so far
      if (p_value < best_p_value && p_value < threshold_p) {
        best_p_value <- p_value
        best_var <- var
      }
    }
    
    # If a variable improves the model, add it
    if (!is.null(best_var)) {
      selected_vars <- c(selected_vars, best_var)
      remaining_vars <- setdiff(remaining_vars, best_var)
      X_selected <- cbind(X_selected, X_all[, best_var])
    } else {
      # If no variable improves the model, stop the selection process
      break
    }
  }
  
  # Final model coefficients and summary
  final_beta <- calculate_coefficients(X_selected, y)
  
  # Return the selected variables and final coefficients
  return(list(selected_vars = selected_vars, coefficients = final_beta))
}

#--------- TEST ---------#

# Example dataset
set.seed(123)  # For reproducibility
n <- 100
x <- seq(1, 100, length.out = n)
y <- 2 + 0.5 * x + rnorm(n, mean = 0, sd = 5)

data <- data.frame(x = x, y = y)

model_result <- nipoet_forward_selection(data, response = "y", threshold_p = 0.05)

selected_vars <- model_result$selected_vars
coefficients <- model_result$coefficients

x_selected <- data[, selected_vars[1]]
y_pred <- coefficients[1] + coefficients[2] * x_selected  # Intercept + Slope * Feature

plot_ly() %>%
  add_markers(x = ~x_selected, y = ~y, 
              marker = list(color = 'blue', size = 5), 
              name = "Data points") %>%
  
  add_lines(x = ~x_selected, y = ~y_pred, 
            line = list(color = 'red', width = 2), 
            name = "Fitted line") %>%
  
  layout(title = "Linear Model Fit (Forward Selection)", 
         xaxis = list(title = paste("Selected Feature:", selected_vars[1])), 
         yaxis = list(title = "Y values"))