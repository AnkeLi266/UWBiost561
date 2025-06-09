test_that("simulation function returns a data frame", {
  res <- simulate_partial_clique_study(
    n_values = 10,  # very small for fast testing
    alpha_values = 0.6,
    n_trials = 1,
    time_limit = 1
  )
  expect_true(is.data.frame(res))
  expect_gt(nrow(res), 0)
})

test_that("all methods are run for each trial", {
  res <- simulate_partial_clique_study(
    n_values = 10, 
    alpha_values = 0.6, 
    n_trials = 1,
    time_limit = 1
  )
  expect_equal(length(unique(res$method)), 15)
})

test_that("simulation catches errors", {
  bad_res <- simulate_partial_clique_study(
    n_values = 10,  # will probably error
    alpha_values = 2, # invalid alpha
    n_trials = 1,
    time_limit = 1
  )
  expect_true(any(bad_res$error))
})
