test_that("output list contains clique_idx and edge_density", {
  set.seed(1)
  adj <- generate_partial_clique(10, 0.5, 0.9)$adj_mat
  res <- compute_maximal_partial_clique(adj, alpha = 0.8)
  expect_named(res, c("clique_idx", "edge_density"))
})

test_that("clique_idx values are valid node indices", {
  set.seed(2)
  adj <- generate_partial_clique(12, 0.5, 0.9)$adj_mat
  res <- compute_maximal_partial_clique(adj, alpha = 0.8)
  expect_true(all(res$clique_idx %% 1 == 0))
  expect_true(all(res$clique_idx >= 1 & res$clique_idx <= nrow(adj)))
})

test_that("edge density is >= alpha", {
  set.seed(3)
  adj <- generate_partial_clique(10, 0.5, 0.9)$adj_mat
  res <- compute_maximal_partial_clique(adj, alpha = 0.85)
  expect_true(res$edge_density >= 0.85)
})

test_that("function errors with bad adj_mat input", {
  bad_mat <- matrix(runif(25), nrow = 5)
  diag(bad_mat) <- 1
  expect_error(compute_maximal_partial_clique(bad_mat, 0.9))
})

test_that("function errors if alpha is out of bounds", {
  adj <- generate_partial_clique(10, 0.5, 0.9)$adj_mat
  expect_error(compute_maximal_partial_clique(adj, alpha = 1.5))
})
