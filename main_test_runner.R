source("pure_random_search.R")
source("multi_start.R")
source("genetic_algorithm.R")

library(vioplot)

N <- 50

test_function <- function(smoof_function) {
  function_name <- getName(smoof_function)
  
  ms_results <- get_average_result_ms(smoof_function, 100, N)
  budget <- as.integer(ms_results$total_iterations)
  prs_results <- get_average_result_prs(smoof_function, budget, N)
  ga_results <- get_average_result_ga(smoof_function, budget, N)
  
  boxplot(ms_results$data, prs_results$data, ga_results$data, 
          names=c("Multi-start", "Pure random search", "Genetic algorithm"),
          main=sprintf("Comparison of algorithms: %s", function_name),
          ylab="Minimum function value",
          col=c("red", "green", "blue"))
  
  vioplot(ms_results$data, prs_results$data, ga_results$data, 
          names=c("Multi-start", "Pure random search", "Genetic algorithm"),
          main=sprintf("Comparison of algorithms: %s", function_name),
          ylab="Minimum function value",
          col=c("red", "green", "blue"))
  
  hist(ms_results$data, breaks=20, col="red", 
       main=sprintf("Multi-start: %s", function_name), xlab="Minimum function value")
  
  hist(prs_results$data, breaks=20, col="green", 
       main=sprintf("Pure random search: %s", function_name), xlab="Minimum function value")
  
  hist(ga_results$data, breaks=20, col="blue",
       main=sprintf("Genetic algorithm: %s", function_name), xlab="Minimum function value")
  
  return (c("ms"=ms_results$mean, "prs"=prs_results$mean, "ga"=ga_results$mean))
}

main <- function() {
  ackley_function2D = makeAckleyFunction(dimensions = 2)
  ackley_function10D = makeAckleyFunction(dimensions = 10)
  ackley_function20D = makeAckleyFunction(dimensions = 20)
  
  ackley2D_results <- test_function(ackley_function2D)
  ackley10D_results <- test_function(ackley_function10D)
  ackley20D_results <- test_function(ackley_function20D)
  
  michalewicz_function2D = makeMichalewiczFunction(dimensions = 2)
  michalewicz_function10D = makeMichalewiczFunction(dimensions = 10)
  michalewicz_function20D = makeMichalewiczFunction(dimensions = 20)
  
  michalewicz2D_results <- test_function(michalewicz_function2D)
  michalewicz10D_results <- test_function(michalewicz_function10D)
  michalewicz20D_results <- test_function(michalewicz_function20D)

  print("Ackley function 2D")
  print(ackley2D_results)
  print("Ackley function 10D")
  print(ackley10D_results)
  print("Ackley function 20D")
  print(ackley20D_results)
  
  print("Michalewicz function 2D")
  print(michalewicz2D_results)
  print("Michalewicz function 10D")
  print(michalewicz10D_results)
  print("Michalewicz function 20D")
  print(michalewicz20D_results)
}

main()