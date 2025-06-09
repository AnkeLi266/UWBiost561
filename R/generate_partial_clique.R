#' Generate a random graph with a partial clique
#'
#' This function generates a symmetric adjacency matrix with a partial clique.
#'
#' @param n Positive integer. Number of nodes in the graph.
#' @param clique_fraction Numeric in [0, 1]. Fraction of nodes in the partial clique.
#' @param clique_edge_density A number between 0 and 1 (inclusive).
#' @return A list with element \code{adj_mat}, the symmetric adjacency matrix.
#' @export
generate_partial_clique <- function(n, clique_fraction, clique_edge_density) {
  # Defensive input checks
  if (length(n) != 1 || n %% 1 != 0 || n < 2) stop("n must be a single integer >= 2.")
  if (length(clique_fraction) != 1 || clique_fraction < 0 || clique_fraction > 1)
    stop("clique_fraction must be in [0, 1].")
  if (length(clique_edge_density) != 1 || clique_edge_density < 0 || clique_edge_density > 1)
    stop("clique_edge_density must be in [0, 1].")
  
  m <- round(n * clique_fraction)
  if (m < 2) {
    # Not enough nodes for a clique; return diagonal matrix
    adj_mat <- diag(1, n)
    return(list(adj_mat = adj_mat))
  }
  
  max_clique_edges <- choose(m, 2)
  if (max_clique_edges <= 0) stop("Invalid clique size (choose(m, 2) <= 0).")
  num_clique_edges <- round(clique_edge_density * max_clique_edges)
  if (num_clique_edges > max_clique_edges)
    stop("Requested more clique edges than possible.")
  if (num_clique_edges < 0)
    stop("Requested a negative number of edges.")
  
  # Initialize adjacency matrix
  adj_mat <- matrix(0, nrow = n, ncol = n)
  diag(adj_mat) <- 1
  
  # Choose which nodes form the partial clique
  clique_nodes <- sample(seq_len(n), m)
  
  # All possible undirected edges among clique nodes
  possible_edges <- combn(clique_nodes, 2)
  num_possible_edges <- ncol(possible_edges)
  
  # Number of edges to include in the partial clique
  num_edges_to_add <- min(num_clique_edges, num_possible_edges)
  
  if (num_edges_to_add > 0 && num_possible_edges > 0) {
    # Sample which edges to include in the partial clique
    idx <- sample(seq_len(num_possible_edges), num_edges_to_add)
    selected_edges <- possible_edges[, idx, drop = FALSE]
    for (i in seq_len(ncol(selected_edges))) {
      u <- selected_edges[1, i]
      v <- selected_edges[2, i]
      adj_mat[u, v] <- 1
      adj_mat[v, u] <- 1
    }
  }
  
  return(list(adj_mat = adj_mat))
}
