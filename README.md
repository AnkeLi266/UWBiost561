# UWBiost561

UWBiost561 is an R package for simulating random graphs with partial cliques, and finding maximal partial cliques using several algorithms. It is designed for both educational and research purposes, especially for those interested in graph theory, simulation, and algorithm benchmarking.

## Installation

To install the latest version from GitHub, use:

```r
# If you have not installed devtools, run:
# install.packages("devtools")
devtools::install_github("AnkeLi266/UWBiost561")
```


library(UWBiost561)

# Generate a random adjacency matrix with a planted partial clique
set.seed(123)
sim <- generate_partial_clique(n = 10, clique_fraction = 0.6, clique_edge_density = 0.9)

# Find the maximal partial clique using method 6 (greedy search)
result <- compute_maximal_partial_clique6(sim$adj_mat, alpha = 0.8)
print(result)

# Run a simple simulation study
sim_results <- simulate_partial_clique_study(
  n_values = 10,
  alpha_values = 0.8,
  n_trials = 2,
  time_limit = 5
)
head(sim_results)
