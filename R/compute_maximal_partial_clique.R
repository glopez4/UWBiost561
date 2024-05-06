#' Compute the maximal partial clique
#'
#' Computes the maximal partial clique of a given adjacency matrix
#'
#' @param adj_mat a symmetric adjacency matrix consisting of only 0 and 1 with 1's along the diagonal and between 5 and 50 rows/columns
#' @param alpha edge density
#'
#' @return
#' @export
compute_maximal_partial_clique <- function(adj_mat, alpha) {
  # check if adj_mat is a symmetric matrix with only values 0 or 1
  stopifnot(is.matrix(adj_mat), all(adj_mat == t(adj_mat)), all(adj_mat %in% c(0, 1)))

  # check if adj_mat has 1's along the diagonal
  stopifnot(all(diag(adj_mat) == 1))

  # check if adj_mat has no row or column names
  stopifnot(is.null(rownames(adj_mat)) && is.null(colnames(adj_mat)))

  # check if adj_mat has between 5 and 50 rows/columns
  stopifnot(nrow(adj_mat) >= 5 && nrow(adj_mat) <= 50 && ncol(adj_mat) >= 5 && ncol(adj_mat) <= 50)

  # check if alpha is a single numeric value between 0.5 and 1
  stopifnot(is.numeric(alpha), length(alpha) == 1, alpha >= 0.5, alpha <= 1)

  # initialize vector to store selected nodes
  clique_idx <- numeric(0)

  # compute edge density for each potential clique
  for (i in 1:nrow(adj_mat)) {
    if(all(adj_mat[i, clique_idx] >= alpha)) {
      clique_idx <- c(clique_idx, i)
    }
  }

  # if no nodes satisfy the condition, return an empty clique
  if(length(clique_idx) == 0) {
    clique_idx <- numeric(0)
  }

  # calculate the number of edges within the identified clique
  clique_edges <- sum(adj_mat[clique_idx,clique_idx]) - length(clique_idx)

  # calculate the number of edges in a full clique of the same size
  full_clique_edges <- choose(length(clique_idx), 2)

  # check if the number of edges in the identified clique meets the condition
  if(clique_edges >= alpha * full_clique_edges) {
    # calculate edge density
    edge_density <- clique_edges / full_clique_edges
  } else {
    # if the condition is not met, reset clique_idx to an empty vector
    clique_idx <- numeric(0)
    edge_density(0)
  }

  # if adj_mat is all 0s except for the diagonal, the maximal partial clique has size 1
  if(all(adj_mat == diag(nrow(adj_mat))) && all(diag(adj_mat) == 1)) {
    clique_idx <- 1
  }

  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
