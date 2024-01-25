source("pure_random_search.R")
source("multi_start.R")
source("genetic_algorithm.R")

library(vioplot)

N <- 50
dimensions <- list(2, 10, 20)

functions <- list(
  makeAckleyFunction,
  makeMichalewiczFunction
)

test_function <- function(smoof_function) {
  function_name <- getName(smoof_function)
  
  ms_results <- get_average_result_ms(smoof_function, 100, N)
  budget <- as.integer(ms_results$total_iterations)
  prs_results <- get_average_result_prs(smoof_function, budget, N)
  ga_results <- get_average_result_ga(smoof_function, budget, N)
  
  # -------------- BOX PLOTS --------------
  png(file=sprintf("plots/boxplot/%s.png", function_name), width=800, height=600)
  boxplot(ms_results$data, prs_results$data, ga_results$data, 
          names=c("Multi-start", "Pure random search", "Genetic algorithm"),
          main=function_name,
          ylab="Minimum function value",
          col=c("red", "green", "blue"))
  dev.off()
  
  png(file=sprintf("plots/boxplot/multi_start/%s.png", function_name), width=450, height=600)
  boxplot(ms_results$data, names="Multi-start",
          main=sprintf("Multi-start: %s", function_name),
          ylab="Minimum function value",
          col="red")
  dev.off()
  
  png(file=sprintf("plots/boxplot/pure_random_search/%s.png", function_name), width=450, height=600)
  boxplot(prs_results$data, names="Pure random search",
          main=sprintf("Pure random search: %s", function_name),
          ylab="Minimum function value",
          col="green")
  dev.off()
  
  png(file=sprintf("plots/boxplot/genetic_algorithm/%s.png", function_name), width=450, height=600)
  boxplot(ga_results$data, names="Genetic algorithm",
          main=sprintf("Pure random search: %s", function_name),
          ylab="Minimum function value",
          col="blue")
  dev.off()
  
  # -------------- VIOLIN PLOTS --------------
  png(file=sprintf("plots/violin/%s.png", function_name), width=800, height=600)
  vioplot(ms_results$data, prs_results$data, ga_results$data, 
          names=c("Multi-start", "Pure random search", "Genetic algorithm"),
          main=function_name,
          ylab="Minimum function value",
          col=c("red", "green", "blue"))
  dev.off()
  
  png(file=sprintf("plots/violin/multi_start/%s.png", function_name), width=450, height=600)
  vioplot(ms_results$data, names="Multi-start",
          main=sprintf("Multi-start: %s", function_name),
          ylab="Minimum function value",
          col="red")
  dev.off()
  
  png(file=sprintf("plots/violin/pure_random_search/%s.png", function_name), width=450, height=600)
  vioplot(prs_results$data, names="Pure random search",
          main=sprintf("Pure random search: %s", function_name),
          ylab="Minimum function value",
          col="green")
  dev.off()
  
  png(file=sprintf("plots/violin/genetic_algorithm/%s.png", function_name), width=450, height=600)
  vioplot(ga_results$data, names="Genetic algorithm",
          main=sprintf("Pure random search: %s", function_name),
          ylab="Minimum function value",
          col="blue")
  dev.off()
  
  # -------------- HISTOGRAMS --------------
  png(file=sprintf("plots/multi_start/%s.png", function_name), width=800, height=600)
  hist(ms_results$data, breaks=20, col="red", 
       main=sprintf("Multi-start: %s", function_name), xlab="Minimum function value")
  dev.off()
  
  png(file=sprintf("plots/pure_random_search/%s.png", function_name), width=800, height=600)
  hist(prs_results$data, breaks=20, col="green", 
       main=sprintf("Pure random search: %s", function_name), xlab="Minimum function value")
  dev.off()
  
  png(file=sprintf("plots/genetic_algorithm/%s.png", function_name), width=800, height=600)
  hist(ga_results$data, breaks=20, col="blue",
       main=sprintf("Genetic algorithm: %s", function_name), xlab="Minimum function value")
  dev.off()
  
  return (list(
    "optimization_results" = c("ms"=ms_results$mean, "prs"=prs_results$mean, "ga"=ga_results$mean),
    "budget" = budget,
    "data" = list(
      "ms" = ms_results$data,
      "prs" = prs_results$data,
      "ga" = ga_results$data
    )))
}

main <- function() {
  
  for(func in functions) {
    for(dim in dimensions) {
      smoof_function <- func(dimensions = dim)
      results <- test_function(smoof_function)
      
      print(sprintf("-------- %s --------", getName(smoof_function)))
      print(results$optimization_results)
      print(results$budget)
      
      print("comparison MS-PRS")
      print(t.test(results$data$ms, results$data$prs))
      print("comparison MS-GA")
      print(t.test(results$data$ms, results$data$ga))
      print("comparison PRS-GA")
      print(t.test(results$data$prs, results$data$ga))
    }
  }
}

main()