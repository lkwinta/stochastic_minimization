library(smoof)
library(parallel)

multi_start <- function(smoof_function, optim_runs) {
  D <- getNumberOfParameters(smoof_function)
  lower_bound <- getLowerBoxConstraints(smoof_function)
  upper_bound <- getUpperBoxConstraints(smoof_function)
  
  function_calls <- 0
  min_result <- Inf
  
  for(i in 1:optim_runs) {
    input_vector <- runif(D, lower_bound, upper_bound)
    
    optim_result <- optim(
      par=input_vector, 
      fn = smoof_function, 
      method = "L-BFGS-B", 
      lower = lower_bound, 
      upper = upper_bound
    )
    
    if(optim_result$convergence == 0) {
      function_calls <- function_calls + optim_result$counts[["function"]]
      min_result <- min(min_result, optim_result$value)
    }
  }
  
  return (c(min_result, function_calls))
}

get_average_result_ms <- function(smoof_function, optim_runs, N) {
  cl <- makeCluster(detectCores() - 1)
  clusterEvalQ(cl, library(smoof))
  clusterExport(cl, "multi_start")
  clusterSetRNGStream(cl)
  
  result <- parSapply(cl, 1:N, function(i) {multi_start(smoof_function, optim_runs)})
  
  stopCluster(cl)
  
  return (list( "mean" = mean(result[1, ]), 
                "total_iterations" = mean(result[2, ]),
                "data"=(result[1,])))
}

