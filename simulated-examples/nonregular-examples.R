library(mvtnorm)

max_mean_param <- 1
param_dim <- 5
dif_means <- .3
n_observations <- 100
n_reps <- 1000
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

###
# Simulation
#

param_vec <- DefineMeanVector(
    p_params = param_dim,
    max_mean_param = max_mean_param,
    dif_mean_param = dif_means
)

example_res <- replicate(SimulateMaxOfMeansInstance(param_vec = param_vec, n_obs = n_observations), n = n_reps)
max_ests <- unlist(example_res[2, ])
est_probs <- dnorm(max_ests, mean = 1)
