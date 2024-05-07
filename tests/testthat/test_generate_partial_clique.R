context("Testing generate_partial_clique")

test_that("generate_partial_clique works", {
  # mock data for testing
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)
  # test that output is a list
  expect_true(is.list(res))
  # test that output components have the correct dimension and types
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))

  # test that adjacency matrix is symmetric, consists of 0s and 1s, and has 1s along the diagonal
  expect_true(all(res$adj_mat == t(res$adj_mat)))
  expect_true(all(res$adj_mat %in% c(0, 1)))
  #expect_true(all(diag(res$adj_mat) == 1))
})
