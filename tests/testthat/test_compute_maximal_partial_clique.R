context("Testing compute_maximal_partial_clique")

test_that("compute_maximal_partial_clique works", {
  # mock data for testing
  set.seed(10)
  sim <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)
  adj_mat <- sim$adj_mat
  alpha <- 0.9
  res <- compute_maximal_partial_clique(
    adj_mat = adj_mat,
    alpha = alpha
  )

  # test that output is the correct type
  expect_true(is.list(res))
  expect_true(is.vector(res$clique_idx))
  expect_true(is.numeric(res$edge_density))
})
