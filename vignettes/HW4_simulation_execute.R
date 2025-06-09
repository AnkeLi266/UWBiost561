# HW4_simulation_execute.R
# 1. load
library(UWBiost561)


sim_results <- simulate_partial_clique_study(
  n_values = c(10, 25, 50),
  alpha_values = c(0.7, 0.9),
  clique_fraction = 0.5,
  clique_edge_density = 0.9,
  n_trials = 10,         
  time_limit = 30        
)


save(sim_results,file = "HW4_simulation.RData")

