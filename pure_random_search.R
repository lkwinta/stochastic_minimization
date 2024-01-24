library(smoof)
library(parallel)

pure_random_search <- function(smoof_funcion, budget) {
  D <- getNumberOfParameters(smoof_funcion)
  lower_bound <- getLowerBoxConstraints(smoof_funcion)
  upper_bound <- getUpperBoxConstraints(smoof_funcion)
  
  min_value = Inf
  
  for(i in 1:budget) {
    input_vector <- runif(D, lower_bound, upper_bound)
    min_value = min(min_value, smoof_funcion(input_vector))
  }
  
  return (min_value)
}

get_average_result_prs <- function(smoof_function, budget, N) {
  cl <- makeCluster(detectCores() - 1)
  clusterEvalQ(cl, library(smoof))
  clusterExport(cl, "pure_random_search")
  clusterSetRNGStream(cl)
  
  mins <- parSapply(cl, 1:N, function(i) {pure_random_search(smoof_function, budget)})
  stopCluster(cl)
  
  return (list("mean"=mean(mins), "data"=mins))
}

