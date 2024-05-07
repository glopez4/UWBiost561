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

  # test that output is within the correct range
  expect_true(length(res$clique_idx) <= nrow(adj_mat))
  expect_true(all(res$clique_idx <= nrow(adj_mat)) && all(res$clique_idx > 0))
  expect_true(res$edge_density >= alpha && res$edge_density <= 1)

  # test with a small adjacency matrix
  adj_mat <- matrix(c(1, 1, 0, 0, 0,
                      1, 1, 1, 0, 0,
                      0, 1, 1, 1, 0,
                      0, 0, 1, 1, 1,
                      0, 0, 0, 1, 1), nrow = 5, byrow = TRUE)
  alpha <- 0.6
  result <- compute_maximal_partial_clique(adj_mat, alpha)
  expect_equal(result$clique_idx, c(1, 2, 3, 4))
  expect_equal(result$edge_density, 0.6)

  # test with a larger adjacency matrix
  adj_mat <- matrix(0, nrow = 50, ncol = 50)
  diag(adj_mat) <- 1
  alpha <- 0.7
  result <- compute_maximal_partial_clique(adj_mat, alpha)
  expect_equal(length(result$clique_idx), 1)
  expect_equal(result$edge_density, 1)

  # test with adjacency matrix containing only 1s
  adj_mat <- matrix(1, nrow = 10, ncol = 10)
  diag(adj_mat) <- 1
  alpha <- 0.5
  result <- compute_maximal_partial_clique(adj_mat, alpha)
  expect_equal(result$clique_idx, 1:10)
  expect_equal(result$edge_density, 1)

  # testing with an empty adjacency matrix
  adj_mat <- matrix(0, nrow = 10, ncol = 10)
  alpha <- 0.7
  result <- compute_maximal_partial_clique(adj_mat, alpha)
  expect_equal(result, list(clique_idx = numeric(0), edge_density = NA))

})
