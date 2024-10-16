library(plotly)

nipoet_forward_selection <- function(data, response, threshold_p = 0.05, tolerance = 1e-6) {
  
  # Separate the response and predictors
  y <- as.matrix(data[[response]])  # Response vector
  X_all <- as.matrix(data[, setdiff(names(data), response)])
  n <- nrow(X_all)  
  k <- ncol(X_all)
  
  # Initialize variables for forward selection
  selected_vars <- c()
  remaining_vars <- 1:k
  X_selected <- matrix(1, nrow=n, ncol=1)
  best_model <- NULL
  
  # Function to calculate coefficients manually
  calculate_coefficients <- function(X, y) {
    return(solve(t(X) %*% X + diag(ncol(X)) * tolerance) %*% t(X) %*% y)
  }
  
  # Function to calculate F-statistic
  calculate_f_statistic <- function(X, y, beta) {
    y_hat <- X %*% beta  # Predicted values
    residuals <- y - y_hat
    rss <- sum(residuals^2)
    tss <- sum((y - mean(y))^2)  
    r_squared <- 1 - (rss / tss)  # R-squared
    
    # Degrees of freedom
    p <- ncol(X) - 1  # Number of predictors (excluding intercept)
    df_model <- p
    df_error <- n - p - 1
    
    # F-statistic
    f_statistic <- (r_squared / df_model) / ((1 - r_squared) / df_error)
    return(f_statistic)
  }
  
  # Forward selection process
  while (length(remaining_vars) > 0) {
    best_p_value <- Inf  # Initialize p-value for comparison
    best_var <- NULL  # Best variable to add
    
    # Try adding each variable and calculate the F-statistic
    for (var in remaining_vars) {
      X_temp <- cbind(X_selected, X_all[, var])  # Add the candidate variable to the model
      beta <- calculate_coefficients(X_temp, y)  # Calculate coefficients
      
      # Check if the new coefficient is significantly different from zero
      if (abs(beta[length(beta)]) < tolerance) {
        next  # Skip this variable if its coefficient is too close to zero
      }
      
      f_statistic <- calculate_f_statistic(X_temp, y, beta)  # Calculate F-statistic
      
      # Calculate the p-value from the F-statistic
      df_model <- ncol(X_temp) - 1
      df_error <- n - ncol(X_temp)
      p_value <- pf(f_statistic, df_model, df_error, lower.tail = FALSE)
      
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
  
  final_beta <- calculate_coefficients(X_selected, y)
  
  # Create a named vector of coefficients
  coefficient_names <- c("Intercept", colnames(X_all)[selected_vars])
  named_coefficients <- setNames(as.vector(final_beta), coefficient_names)
  
  return(list(selected_vars = selected_vars, coefficients = named_coefficients))
}


#--------- TEST ---------#

# Load required libraries
library(plotly)

# Example dataset creation
set.seed(123)  # For reproducibility
n <- 100  # Number of observations

# Create 10 features
x1 <- rnorm(n, mean = 50, sd = 10)
x2 <- rnorm(n, mean = 30, sd = 5)
x3 <- rnorm(n, mean = 20, sd = 3)
x4 <- rnorm(n, mean = 10, sd = 2)
x5 <- rnorm(n, mean = 60, sd = 15)
x6 <- rnorm(n, mean = 40, sd = 8)
x7 <- rnorm(n, mean = 25, sd = 4)
x8 <- rnorm(n, mean = 55, sd = 12)
x9 <- rnorm(n, mean = 45, sd = 7)
x10 <- rnorm(n, mean = 35, sd = 5)

# Create a response variable with some noise
y <- 2 + 0.3 * x1 - 0.5 * x2 + 0.1 * x3

# Combine into a data frame
data <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y)

# Perform forward selection
model_result <- nipoet_forward_selection(data, response = "y", threshold_p = 0.05)

selected_vars <- model_result$selected_vars
coefficients <- model_result$coefficients

# Load required libraries
library(plotly)

# Assuming `model_result` contains the coefficients and selected variables
# Prepare data for bar plot
coefficients_df <- data.frame(
  Feature = c("(Intercept)", selected_vars),  # Include the intercept
  Coefficient = coefficients  # Coefficients from the model
)

# Create Plotly bar plot
plot_ly(coefficients_df, x = ~Feature, y = ~Coefficient, type = 'bar', 
        marker = list(color = 'lightred')) %>%
  layout(title = "Bar Plot of Coefficients",
         xaxis = list(title = "Features"),
         yaxis = list(title = "Coefficient Values"),
         showlegend = FALSE)

# train test split
