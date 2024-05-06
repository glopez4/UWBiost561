#' Generate a random graph with a partial clique
#'
#'Generate a random adjacency matrix with a large partial clique.
#' @param n number of nodes in the graph
#' @param clique_fraction the fraction of nodes (of the n nodes) that are part of the partial clique
#' @param clique_edge_density the edge density among the nodes in the clique
#'
#' @return a random adjacency matrix with a partial clique - it is a symmetric matrix with only values 0 or 1 and has all 1's along its diagonal
#' @export
generate_partial_clique <- function(n, clique_fraction, clique_edge_density)
{
  stopifnot(n%%1 == 0, n > 0, clique_fraction >= 0, clique_fraction <= 1,
            clique_edge_density >= 0, clique_edge_density <= 1)

  # calculate the size of the partial clique
  m <- round(n * clique_fraction)
  stopifnot(m > 0, m <= n)

  # calculate the number of edges the partial clique should have
  min_clique_edges <- round(clique_edge_density * m * (m-1) / 2)

  # create an empty adjacency matrix
  adj_mat <- matrix(0, nrow = n, ncol = n)

  # fill the adjacency matrix with edges within the partial clique
  clique_edges <- round(clique_edge_density * m * (m-1) / 2)
  if (clique_edges > 0) {
    clique_nodes <- sample(1:m, clique_edges, replace = TRUE)
    for (i in 1:clique_edges) {
      for (j in 1:clique_edges) {
        adj_mat[clique_nodes[i], clique_nodes[j]] <- 1
        adj_mat[clique_nodes[j], clique_nodes[i]] <- 1
      }
    }
  }

  # fill the diagonal with ones
  diag(adj_mat) <- 1

  # fill the rest of the adjacency matrix with random edges
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > m || j > m) {
        if (adj_mat[i, j] == 0 && runif(1) < 0.5) {
          adj_mat[i, j] <- 1
          adj_mat[j, i] <- 1
        }
      }
    }
  }

  # shuffle matrix
  sample_idx <- sample(1:n)
  adj_mat <- adj_mat[sample_idx, sample_idx]

  # reverse ordering
  rev_order <- sapply(1:n, function(i) {
    which(sample_idx == i)
  })

  list(adj_mat = adj_mat, rev_order = rev_order)
}
