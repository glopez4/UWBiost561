#' Compute maximal partial clique
#'
#'Computes the maximal partial clique of a given adjacency matrix
#'
#' @param adj_mat adjacency matrix
#' @param alpha edge density
#'
#' @return index numbers of nodes along with the edge density
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

  # calculate the number of nodes
  n <- nrow(adj_mat)

  # initialize clique_idx to an empty vector
  clique_idx <- c()

  #iterate over each node to find the maximal partial clique
  for (i in 1:n) {
    if(length(clique_idx) == 0 ||
              (sum(adj_mat[clique_idx, i]) >= alpha * length(clique_idx) &&
               sum(adj_mat[i, clique_idx]) >= alpha * length(clique_idx))) {
      # add node i to the clique
      clique_idx <- c(clique_idx, i)
    }
  }

  # calculate edge density
  m <- length(clique_idx)
  edge_density_numerator <- sum(adj_mat[clique_idx, clique_idx]) - m
  edge_density_denominator <- m * (m-1) / 2
  edge_density <- edge_density_numerator / edge_density_denominator

  return(list(clique_idx = clique_idx, edge_density = edge_density))
}
