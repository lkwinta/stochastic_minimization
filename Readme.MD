# Stochastic minimization

Simple R script to find function minimum using stochastic minimization
algorithm for statistic course on AGH UST.

We have implemented three of the following algorithms: - Pure Random
Search - Multi-Start (using L-BSGS-B from R `optim` package) - Genetic
Algorithm (using R `ecr` package)

Script generates multiple plots with data from minimum finding and text
output with results.

# Usage

Script requires R installed on your machine tohether with following
dependencies: - `ecr` package - `smoof` package - `parallel` package -
`vioplot` package

```         
RScript main_test_runner.R
```

In `main_test_runner.R` you can change following parameters: - `N` -
number of repeats for each algorithm - `dimensions` - list of dimensions
to test for each function - `functions` - list of functions to create
tested function from smoof package

# Results

You can find plots in `plots` directory. Results are printed on screen
after running algorithm. You can find our elaboration in `wyniki.Rmd`
(in Polish).
