#' Generate a random graph with a partial clique
#'
#' This function generates a symmetric adjacency matrix with a partial clique.
#'
#' @param n Positive integer. Number of nodes in the graph.
#' @param clique_fraction Numeric 0, 1. Fraction of nodes in the partial clique.
#' @param clique_edge_density A number between 0 and 1 (inclusive).
#' @return A list with element \code{adj_mat}, the symmetric adjacency matrix.
#' @export
generate_partial_clique <- function(n, clique_fraction, clique_edge_density) {
  
  # Input checks
  stopifnot(length(n) == 1, n %% 1 == 0, n > 0)
  stopifnot(length(clique_fraction) == 1, clique_fraction >= 0, clique_fraction <= 1)
  stopifnot(length(clique_edge_density) == 1, clique_edge_density >= 0, clique_edge_density <= 1)
  
  # Initialize adjacency matrix
  adj_mat <- matrix(0, nrow = n, ncol = n)
  diag(adj_mat) <- 1
  
  # Determine size of partial clique
  m <- round(n * clique_fraction)
  if (m < 2) return(list(adj_mat = adj_mat))  # Too small to form edges
  
  # Choose which nodes form the partial clique
  clique_nodes <- sample(1:n, m)
  
  # Number of edges to include in the partial clique
  possible_edges <- combn(clique_nodes, 2)
  num_edges_to_add <- round(ncol(possible_edges) * clique_edge_density)
  
  # Sample which edges to include in partial clique
  if (num_edges_to_add > 0) {
    selected_edges <- possible_edges[, sample(ncol(possible_edges), num_edges_to_add)]
    for (i in seq_len(ncol(selected_edges))) {
      u <- selected_edges[1, i]
      v <- selected_edges[2, i]
      adj_mat[u, v] <- 1
      adj_mat[v, u] <- 1
    }
  }
  
  return(list(adj_mat = adj_mat))
}
