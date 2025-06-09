#' Compute maximal partial clique
#'
#' This function finds a reasonably large set of nodes in a graph (represented by
#' an adjacency matrix) that forms a partial clique with edge density at least alpha.
#' It uses a greedy approach to expand from highly connected nodes.
#'
#' @param adj_mat A symmetric binary adjacency matrix with diagonal 1s and no row/column names.
#' @param alpha A numeric value between 0.5 and 1. The minimum density required in the clique.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{clique_idx}{Indices of nodes in the maximal partial clique.}
#'   \item{edge_density}{The edge density among the selected nodes.}
#' }
#'
#' @export
compute_maximal_partial_clique6 <- function(adj_mat, alpha) {
  # Input checks
  stopifnot(is.matrix(adj_mat),
            all(adj_mat %in% c(0, 1)),
            isSymmetric(adj_mat),
            all(diag(adj_mat) == 1),
            is.null(rownames(adj_mat)),
            is.null(colnames(adj_mat)),
            nrow(adj_mat) >= 5,
            nrow(adj_mat) <= 50)
  
  stopifnot(length(alpha) == 1,
            is.numeric(alpha),
            alpha >= 0.5,
            alpha <= 1)
  
  n <- nrow(adj_mat)
  best_clique <- c()
  best_density <- 0
  
  for (i in 1:n) {
    current <- i
    candidates <- setdiff(1:n, current)
    improved <- TRUE
    
    while (improved) {
      improved <- FALSE
      for (j in candidates) {
        new_clique <- c(current, j)
        m <- length(new_clique)
        num_edges <- (sum(adj_mat[new_clique, new_clique]) - m) / 2
        possible_edges <- m * (m - 1) / 2
        density <- ifelse(possible_edges == 0, 1, num_edges / possible_edges)
        
        if (density >= alpha) {
          current <- new_clique
          candidates <- setdiff(candidates, j)
          improved <- TRUE
        }
      }
    }
    
    # Keep best
    m <- length(current)
    num_edges <- (sum(adj_mat[current, current]) - m) / 2
    possible_edges <- m * (m - 1) / 2
    density <- ifelse(possible_edges == 0, 1, num_edges / possible_edges)
    
    if (m > length(best_clique)) {
      best_clique <- current
      best_density <- density
    }
  }
  
  return(list(clique_idx = sort(best_clique),
              edge_density = best_density))
}