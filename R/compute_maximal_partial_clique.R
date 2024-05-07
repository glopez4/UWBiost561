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

  n <- nrow(adj_mat)
  # Function to compute edge density
  compute_edge_density <- function(clique_idx) {
    m <- length(clique_idx)
    if (m <= 1) return(0) # If clique size is 1, edge density is 0
    edge_density_numerator <- (sum(adj_mat[clique_idx, clique_idx])-m) / 2
    max_possible_edges <- m * (m - 1) / 2
    return(edge_density_numerator / max_possible_edges)
  }

  # Initialize variables
  best_clique_idx <- numeric(0)
  best_edge_density <- 0

  # Find all nodes with degree greater than or equal to alpha * (n - 1)
  candidate_nodes <- which(rowSums(adj_mat) >= alpha * (n - 1))

  # Iterate through each candidate node and expand clique
  for (node in candidate_nodes) {
    clique <- node
    neighbors <- which(adj_mat[node, ] == 1)

    # Greedily expand the clique
    while (length(neighbors) > 0) {
      new_node <- neighbors[1]
      if (compute_edge_density(c(clique, new_node)) >= alpha && !new_node %in% clique) {
        clique <- c(clique, new_node)
        neighbors <- setdiff(intersect(which(adj_mat[new_node, ] == 1), neighbors), clique)
      } else {
        break
      }
    }

    # Update the best clique if a larger one is found
    if (length(clique) > length(best_clique_idx)) {
      best_clique_idx <- clique
      best_edge_density <- compute_edge_density(clique)
    }
  }

  # Output the best maximal partial clique and its edge density
  return(list(clique_idx = best_clique_idx, edge_density = best_edge_density))
}

library(UWBiost561)
set.seed(0)
simulation <- UWBiost561::generate_partial_clique(
  n = 10,
  clique_fraction = 0.5,
  clique_edge_density = 0.2
)
adj_mat <- simulation$adj_mat
res <- compute_maximal_partial_clique(
  adj_mat = adj_mat,
  alpha = 0.6
)
res
adj_mat[res$clique_idx, res$clique_idx]



adj_mat <- matrix(c(
  1, 1, 0, 0, 1,
  1, 1, 1, 1, 1,
  0, 1, 1, 1, 0,
  0, 1, 1, 1, 1,
  1, 1, 0, 1, 1
), nrow = 5, byrow = TRUE)

# Example alpha value
alpha <- 0.6

# Find the maximal partial clique
result <- compute_maximal_partial_clique(adj_mat, alpha)

# Print the result
print(result)
