
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



# Calculate the mean for each facet
mean_data <- results_tibble %>%
  group_by(scenario, dif_means_formula) %>%
  summarise(mean_NormalizedMaxEst = mean(NormalizedMaxEst),
            mean_RnormDraw = mean(RnormDraw))

# Plot the data
ggplot(data = results_tibble, aes(x = NormalizedMaxEst)) +
  geom_density(color = "orange", adjust = 0.9) +
  geom_vline(data = mean_data, aes(xintercept = mean_NormalizedMaxEst),
             linetype = "dashed", color = "orange") +
  geom_density(aes(x=RnormDraw), color = "grey", adjust = 0.9) +
  geom_vline(data = mean_data, aes(xintercept = mean_RnormDraw),
             linetype = "dashed", color = "grey") +
  xlim(-2.5, 2.5) +
  theme_minimal() +
  labs(title = "Distribution of Normalized Max Estimates",
       x = "Standardized Estimate of Maximum Mean",
       y = "Density") +
  facet_grid(scenario ~ dif_means_formula)

# Change the order and labels of the facets
results_tibble$dif_means_formula <- factor(results_tibble$dif_means_formula,
                                           levels = c("linear", "sqrt", "fourth_root"),
                                           labels = c(expression(delta == n^-1),
                                                      expression(delta == n^-1/2),
                                                      expression(delta == n^-1/4)))
# Calculate the mean for each facet
mean_data <- results_tibble %>%
  group_by(scenario, dif_means_formula) %>%
  summarise(mean_NormalizedMaxEst = mean(NormalizedMaxEst),
            mean_RnormDraw = mean(RnormDraw))

# Plot the data
ggplot(data = results_tibble, aes(x = NormalizedMaxEst)) +
  geom_density(color = "orange", adjust = 0.9) +
  geom_vline(data = mean_data, aes(xintercept = mean_NormalizedMaxEst),
             linetype = "dashed", color = "orange") +
  geom_density(aes(x=RnormDraw), color = "grey", adjust = 0.9) +
  geom_vline(data = mean_data, aes(xintercept = mean_RnormDraw),
             linetype = "dashed", color = "grey") +
  xlim(-2.5, 2.5) +
  theme_minimal() +
  labs(title = "Distribution of Normalized Max Estimates",
       x = "Standardized Estimate of Maximum Mean",
       y = "Density") +
  facet_grid(scenario ~ dif_means_formula)

library(latex2exp)

# Change the order of the facets
results_tibble$dif_means_formula <- factor(results_tibble$dif_means_formula,
                                           levels = c("linear", "sqrt", "fourth_root"))

# Calculate the mean for each facet
mean_data <- results_tibble %>%
  group_by(scenario, dif_means_formula) %>%
  summarise(mean_NormalizedMaxEst = mean(NormalizedMaxEst),
            mean_RnormDraw = mean(RnormDraw))
"fourth_root" = "$\\delta = n^{-1/4}$")

# Create a mapping from the original labels to the LaTeX expressions
latex_labels <- c("linear" = "$\\delta = n^{-1}$",
                  "sqrt" = "$\\delta = n^{-1/2}$",
                  "fourth_root" = "$\\delta = n^{-1/4}$")
texified_labels <-  TeX(latex_labels, output = "character")
# Plot the data
p <- ggplot(data = results_tibble, aes(x = NormalizedMaxEst)) +
  geom_density(color = "orange", adjust = 0.9) +
  geom_vline(data = mean_data, aes(xintercept = mean_NormalizedMaxEst),
             linetype = "dashed", color = "orange") +
  geom_density(aes(x=RnormDraw), color = "grey", adjust = 0.9) +
  geom_vline(data = mean_data, aes(xintercept = mean_RnormDraw),
             linetype = "dashed", color = "grey") +
  xlim(-2.5, 2.5) +
  theme_minimal() +
  labs(title = "Distribution of Normalized Max Estimates",
       x = "Standardized Estimate of Maximum Mean",
       y = "Density") +
  facet_grid(scenario ~ dif_means_formula,
             labeller = labeller(dif_means_formula = texified_labels, labeller = label_parsed))

print(p)




# Create a mapping from the original labels to the LaTeX expressions
latex_labels <- c("linear" = TeX("$\\delta = n^{-1}$"),
"sqrt" = TeX("$\\delta = n^{-1/2}$"),
"fourth_root" = TeX("$\\delta = n^{-1/4}$"))

# Use as_labeller function with label_parsed for mathematical expressions
texified_labels <- as_labeller(latex_labels, label_parsed)

# Plot the data
p <- ggplot(data = results_tibble, aes(x = NormalizedMaxEst)) +
geom_density(color = "orange", adjust = 0.9) +
geom_vline(data = mean_data, aes(xintercept = mean_NormalizedMaxEst),
linetype = "dashed", color = "orange") +
geom_density(aes(x = RnormDraw), color = "grey", adjust = 0.9) +
geom_vline(data = mean_data, aes(xintercept = mean_RnormDraw),
linetype = "dashed", color = "grey") +
xlim(-2.5, 2.5) +
theme_minimal() +
labs(title = "Distribution of Normalized Max Estimates",
x = "Standardized Estimate of Maximum Mean",
y = "Density") +
facet_grid(scenario ~ dif_means_formula, labeller = labeller(dif_means_formula = texified_labels))

print(p)


# Create a mapping from the original labels to the expressions
latex_labels <- c("linear" = "delta == n^{-1}",
"sqrt" = "delta == n^{-1/2}",
"fourth_root" = "delta == n^{-1/4}")

# Use as_labeller function with label_parsed for mathematical expressions
texified_labels <- as_labeller(latex_labels, default = label_parsed)

# Plot the data
p <- ggplot(data = results_tibble, aes(x = NormalizedMaxEst)) +
geom_density(color = "orange", adjust = 0.9) +
geom_vline(data = mean_data, aes(xintercept = mean_NormalizedMaxEst),
linetype = "dashed", color = "orange") +
geom_density(aes(x = RnormDraw), color = "grey", adjust = 0.9) +
geom_vline(data = mean_data, aes(xintercept = mean_RnormDraw),
linetype = "dashed", color = "grey") +
xlim(-2.5, 2.5) +
theme_minimal() +
labs(title = "Distribution of Normalized Max Estimates",
x = "Standardized Estimate of Maximum Mean",
y = "Density") +
facet_grid(scenario ~ dif_means_formula, labeller = labeller(dif_means_formula = texified_labels))

print(p)

# Assuming the plot p has already been created

# Save the plot as an SVG file
ggsave(filename = "/home/jsperger/work/presentations/2024-03-08-smart-inference/figures/max_means_sim_plot.svg", plot = p, width = 8, height = 6)

# Save the plot as a PNG file
ggsave(filename = "/home/jsperger/work/presentations/2024-03-08-smart-inference/figures/max_means_sim_plot.png", plot = p, width = 8, height = 6)
