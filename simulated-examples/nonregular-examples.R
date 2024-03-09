library(tidyverse)
library(mvtnorm)
library(parallel)

max_mean_param <- 1
param_dim <- 5
n_observations <- 400
#dif_means <- 1/sqrt(n_observations)
#dif_means <- 1/(n_observations)
dif_means <- 1/sqrt(sqrt(n_observations))
n_reps <- 10000
set.seed(42)

###
# Functions
#

DefineMeanVector <- function(p_params, max_mean_param, dif_mean_param) {
    param_vec <- c(
        max_mean_param,
        rep((max_mean_param - dif_mean_param), times = p_params - 1)
    )

    return(param_vec)
}

SimulateMVNDraws <- function(param_vec, n_obs){
    observations <- mvtnorm::rmvnorm(n = n_obs, mean = param_vec)

    return(observations)
}

SimulateMaxOfMeansInstance <- function(param_vec, n_obs) {
  replicate_data <- SimulateMVNDraws(param_vec, n_obs)

  estimated_means <- colMeans(replicate_data)

  sim_data <- list(
    est_means = estimated_means,
    est_max_mean = max(estimated_means),
    est_max_pos = which.max(estimated_means)
  )

  return(sim_data)
}

SimulateSpec <- function(param_dim,
                         max_mean_param,
                         dif_means,
                         n_observations,
                         n_reps){
    param_vec <- DefineMeanVector(
        p_params = param_dim,
        max_mean_param = max_mean_param,
        dif_mean_param = dif_means
    )


results <- mapply(process_scenario, n_observations = c(100, 400, 800, 1200, 2000),
                   dif_means_formula = "sqrt",
                   param_dim = param_dim, max_mean_param = max_mean_param, n_reps = n_reps)

results_list_of_tibs <- apply(results, 2, as_tibble)

results_tibble <- bind_rows(results_list_of_tibs) %>%
  mutate(scenario = rep(c(100, 400, 800, 1200, 2000), each = n_reps),
         dif_means_formula = "sqrt") %>%
  select(-ID) # assuming we don't need the original ID column for this analysis

# Output results in desired format
cat("Simulation with n_observation =", results$scenario[1], "and dif_means =", results$NormalizedMaxEst[1], "\n", sep="")
cat("Simulation with n_observation =", results$scenario[2], "and dif_means =", results$NormalizedMaxEst[2], "\n", sep="")
cat("Simulation with n_observation =", results$scenario[3], "and dif_means =", results$NormalizedMaxEst[3], "\n", sep="")
cat("Simulation with n_observation =", results$scenario[4], "and dif_means =", results$NormalizedMaxEst[4], "\n", sep="")
cat("Simulation with n_observation =", results$scenario[5], "and dif_means =", results$NormalizedMaxEst[5], "\n", sep="")
    example_res <- replicate(
        SimulateMaxOfMeansInstance(param_vec = param_vec,
                                   n_obs = n_observations),
        n = n_reps)

    max_ests <- unlist(example_res[2, ])

    sim_df <- tibble(ID = 1:n_reps,
                     MaxEst = max_ests) %>%
                     mutate(NormalizedMaxEst = sqrt(n_observations) * (MaxEst - max_mean_param),
                     RnormDraw = rnorm(n = n(), mean = 0, sd = 1))

  return(sim_df)
}

# Adjust for parallel processing
n_observations_values <- c(100, 400, 2000)
scenarios <- expand.grid(n_observations = n_observations_values,
                         dif_means_formula = c('sqrt', 'linear', 'fourth_root'))

# Function to process each scenario
process_scenario <- function(n_observations,
                             dif_means_formula,
                             ...) {

  n_observatio<- n_observations
  formula <- dif_means_formula

  dif_means <- switch(as.character(formula),
                      sqrt = 1/sqrt(n_observations),
                      linear = 1/n_observations,
                      fourth_root = 1/sqrt(sqrt(n_observations)))

  scenario_mean_vec <- DefineMeanVector(param_dim, max_mean_param, dif_means)

  sim_runs_single_scenario <- replicate(
      SimulateMaxOfMeansInstance(param_vec = scenario_mean_vec,
                                 n_obs = n_observations),
      n = n_reps)

  max_ests <- unlist(sim_runs_single_scenario[2, ])

  scenario_results_df <- tibble(ID = 1:n_reps, MaxEst = max_ests) %>%
    mutate(NormalizedMaxEst = sqrt(n_observations) * (MaxEst - max_mean_param),
           RnormDraw = rnorm(n = n(), mean = 0, sd = 1))

  # Placeholder for simulation execution code
  sprintf("n_observations: %d, dif_means: %f, formula: %s", n_observations, dif_means, formula)
  # Include actual simulation and analysis logic here

  return(scenario_results_df)
}


results <- mapply(FUN = process_scenario,
                  n_observations = c(scenarios$n_observations),
                  dif_means_formula = scenarios$dif_means_formula,
                  MoreArgs = list(param_dim = param_dim,
                                    max_mean_param = max_mean_param,
                                    n_reps = n_reps))


# Flatten the array into a single vector
results_list_of_tibs <- apply(results, 2, as_tibble)

# Convert the vector to a tibble
results_tibble <- bind_rows(results_list_of_tibs) %>%
  mutate(scenario = rep(scenarios$n_observations, each = n_reps),
         dif_means_formula = rep(scenarios$dif_means_formula, each = n_reps))

SimulateFixedDifVaryN <- function(param_dim = 5,
                                  max_mean_param = 1,
                                  dif_means = .01,
                                  n_observations = c(50, 100, 400, 800, 1200, 2000),
                                  n_reps = 1000) {

    scenario_mean_vec <- DefineMeanVector(param_dim, max_mean_param, dif_means)

    sim_list <- map(n_observations, ~ replicate(n = n_reps, SimulateMaxOfMeansInstance(param_vec = scenario_mean_vec, n_obs = .x), simplify = "array"))

    est_max_means <- map(sim_list, ~ unlist(t(.x)[, 2])) %>%
        unlist(.)
    # Convert the vector to a tibble

    scenario_results_df <- tibble(
        MaxEst = est_max_means,
        N = rep(n_observations, each = n_reps)
    ) %>%
        mutate(
            NormalizedMaxEst = sqrt(N) * (MaxEst - max_mean_param),
            RnormDraw = rnorm(n = n(), mean = 0, sd = 1)
        )

    return(scenario_results_df)
}

# Call the simulation with varying n_observation and fixed dif_means
sim_fixed_dif_vary_n_df <- SimulateFixedDifVaryN(n_reps = 1000)

# Output the results in a format suitable for further analysis
write.csv(sim_df, "simulation_results.csv", row.names = FALSE)
