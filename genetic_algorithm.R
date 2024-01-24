library(smoof)
library(ecr)
library(parallel)

population_size = 100
population_growth = 5

genetic_algorithm <- function(smoof_function, budget) {
  D <- getNumberOfParameters(smoof_function)
  lower_bound <- getLowerBoxConstraints(smoof_function)
  upper_bound <- getUpperBoxConstraints(smoof_function)

  minimization <- ecr(
    fitness.fun = smoof_function, representation = "float",
    n.dim = D, survival.strategy = "plus",
    lower = lower_bound, upper = upper_bound,
    mu = population_size, lambda = population_growth,
    mutator = setup(mutGauss, sdev = 1.5, lower = lower_bound, upper = upper_bound),
    terminators = list(stopOnIters(budget)),
    log.pop = FALSE)
    
  return (minimization$best.y)
}

get_average_result_ga <- function(smoof_function, budget, N) {
  cl <- makeCluster(detectCores() - 1)
  clusterEvalQ(cl, library(smoof))
  clusterEvalQ(cl, library(ecr))
  clusterExport(cl, "genetic_algorithm")
  clusterExport(cl, "population_size")
  clusterExport(cl, "population_growth")
  clusterSetRNGStream(cl)
  
  mins <- parSapply(cl, 1:N, function(i) {genetic_algorithm(smoof_function, budget)})
  
  stopCluster(cl)
  
  return (list("mean"=mean(mins), "data"=mins))
}