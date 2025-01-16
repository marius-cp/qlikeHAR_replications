qlikeHAR <- function(X, y, S) {
  # use aggregated data here
  # Loss function for optimization
  loss <- function(params, X, y, S) {
    haty <- params[1] + X %*% params[-1]
    error <- S(x = haty, y = y)
    return(sum(error, na.rm = TRUE)) # na.rm here is important !!!
  }
  X <- as.matrix(X)
  
  # Start time
  t_start_ols <- Sys.time()
  
  # Fit the model using OLS as the initial guess
  ols_model <- stats::lm(y ~ X)
  
  # End time OLS
  t_end_ols <- Sys.time()
  
  
  t_start_optim <- Sys.time()
  # Initial guess for parameters from OLS
  initial_params <- coefficients(ols_model) %>% as.numeric()
  
  # Define lower bounds for parameters (all parameters can be negative, so -Inf)
  lower_bounds <- rep(-Inf, length(initial_params))
  
  # Optimizing the loss function
  result <- stats::optim(
    par = initial_params, 
    fn = loss,
    X = X, y = y, 
    S = S
    )
  
  # Extract optimal parameters
  optimal_params <- result$par
  
  # Fitted values
  x_rc <- optimal_params[1] + X %*% optimal_params[-1]
  
  # Coefficients
  coef <- optimal_params
  
  # End time
  t_end <- Sys.time()
  
  # Calculate computation time in seconds
  time_taken_ols <- as.numeric(difftime(t_end_ols, t_start_ols, units = "secs"))
  time_taken <- as.numeric(difftime(t_end, t_start_optim, units = "secs"))
  
  return(
    list(
      x_rc = x_rc, 
      coef = coef, 
      ols_model = coefficients(ols_model), 
      time_taken_ols = time_taken_ols,
      time_taken = time_taken
      )
    )
}

