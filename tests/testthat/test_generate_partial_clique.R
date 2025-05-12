test_that("generate_partial_clique returns correct structure", {
  set.seed(0)
  sim <- generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.9)
  expect_type(sim, "list")
  expect_true("adj_mat" %in% names(sim))
  expect_true(is.matrix(sim$adj_mat))
})

test_that("partial clique has at least required edge density", {
  set.seed(0)
  sim <- generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.8)
  adj <- sim$adj_mat
  n <- nrow(adj)
  
  m <- round(0.5 * n)
  all_combs <- combn(n, m)
  passed <- FALSE
  
  for (i in 1:ncol(all_combs)) {
    idx <- all_combs[, i]
    sub_adj <- adj[idx, idx]
    edges <- (sum(sub_adj) - m) / 2
    density <- edges / (m * (m - 1) / 2)
    if (density >= 0.8) {
      passed <- TRUE
      break
    }
  }
  
  expect_true(passed)
})

