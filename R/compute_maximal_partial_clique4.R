#' Compute Maximal Partial Clique
#'
#' This function computes an approximate maximal partial clique from a given adjacency matrix,
#' such that the edge density among the selected nodes is at least `alpha`.
#'
#' @param adj_mat A symmetric adjacency matrix (0/1) with diagonal 1s.
#' @param alpha A numeric value between 0.5 and 1, representing the minimum edge density required.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{clique_idx}{A numeric vector of node indices that form the partial clique.}
#'   \item{edge_density}{The edge density among the selected nodes (between 0 and 1).}
#' }
#' @export
compute_maximal_partial_clique4 <- function(adj_mat, alpha) {
  # Input checks
  stopifnot(is.matrix(adj_mat),
            all(adj_mat %in% c(0, 1)),
            isSymmetric(adj_mat),
            all(diag(adj_mat) == 1),
            is.numeric(alpha), length(alpha) == 1,
            alpha >= 0.5, alpha <= 1)
  
  n <- nrow(adj_mat)
  candidate_nodes <- 1:n
  best_clique <- c()
  best_density <- 0
  
  # Heuristic: Start with highest-degree nodes
  degrees <- rowSums(adj_mat) - 1  # exclude diagonal
  node_order <- order(degrees, decreasing = TRUE)
  
  for (i in seq_along(node_order)) {
    candidate <- node_order[i]
    new_clique <- c(best_clique, candidate)
    m <- length(new_clique)
    
    if (m == 1) {
      edge_density <- 1  # single node is trivially 100%
    } else {
      edge_count <- (sum(adj_mat[new_clique, new_clique]) - m) / 2
      max_edges <- m * (m - 1) / 2
      edge_density <- edge_count / max_edges
    }
    
    if (edge_density >= alpha) {
      best_clique <- new_clique
      best_density <- edge_density
    } else {
      break  # adding this node would drop density below alpha
    }
  }
  
  return(list(clique_idx = best_clique,
              edge_density = best_density))
}