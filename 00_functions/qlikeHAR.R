qlikeHAR <- function(X, y, S) {
  # use aggregated data here
  # Loss function for optimization
  loss <- function(params, X, y, S) {
    haty <- params[1] + X %*% params[-1]
    error <- S(x = haty, y = y)
    return(sum(error, na.rm = TRUE)) # na.rm here is important !!!
  }
  
  X <- as.matrix(X)
  
  # Fit the model using OLS as the initial guess
  ols_model <- stats::lm(y ~ X)
  
  # Initial guess for parameters from OLS
  initial_params <- coefficients(ols_model) %>% as.numeric()
  
  # Define lower bounds for parameters (all parameters can be negative, so -Inf)
  lower_bounds <- rep(-Inf, length(initial_params))
  
  # Optimizing the loss function
  result <- stats::optim(par = initial_params, fn = loss, X = X, y = y, S = S)
  
  # Extract optimal parameters
  optimal_params <- result$par
  
  # Fitted values
  x_rc <- optimal_params[1] + X %*% optimal_params[-1]
  
  # Coefficients
  coef <- optimal_params
  
  return(list(x_rc = x_rc, coef = coef, ols_model = coefficients(ols_model)))
}

