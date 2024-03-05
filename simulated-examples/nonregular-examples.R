library(tidyverse)
library(mvtnorm)

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

    example_res <- replicate(
        SimulateMaxOfMeansInstance(param_vec = param_vec,
                                   n_obs = n_observations),
        n = n_reps)

    max_ests <- unlist(example_res[2, ])

    sim_df <- tibble(ID = 1:length(est_dens),
                     MaxEst = max_ests) %>%
        mutate(NormalizedMaxEst = sqrt(n_observations) * (MaxEst - max_mean_param),
               RnormDraw = rnorm(n = n(), mean = 0, sd = 1))

  return(sim_df)
}

###
# Simulation
#

param_vec <- DefineMeanVector(
    p_params = param_dim,
    max_mean_param = max_mean_param,
    dif_mean_param = dif_means
)

example_res <- replicate(
    SimulateMaxOfMeansInstance(param_vec = param_vec,
                               n_obs = n_observations),
    n = n_reps)

max_ests <- unlist(example_res[2, ])

# Asymptotically the distribution of the transformed estimates should be N(0,1)
transformed_ests <- sqrt(n_observations) * (max_ests - max_mean_param)
# est_dens<- dnorm(transformed_ests)

sim_df <- tibble(ID = 1:length(est_dens),
                 MaxEst = max_ests,
                 NormalizedMaxEst = transformed_ests,
                 Dens = est_dens) %>%
    mutate(RnormDraw = rnorm(n = n(), mean = 0, sd = 1))



ggplot(data = sim_df, aes(x = NormalizedMaxEst)) +
    geom_density(color = "orange", adjust = 0.75) +
    geom_vline(xintercept = mean(sim_df$NormalizedMaxEst), linetype = "dashed", color = "orange") +
    geom_density(aes(x=RnormDraw), color = "grey", adjust = 0.75) +
        geom_vline(xintercept = mean(sim_df$RnormDraw), linetype = "dashed", color = "grey") +
    xlim(-4, 4)
