#' Compute Maximal Partial Clique
#'
#' @param adj_mat A symmetric 0/1 adjacency matrix with 1s on the diagonal.
#' @param alpha A numeric threshold between 0.5 and 1 indicating required edge density.
#' @param verbose Logical; whether to print debug output. Default is FALSE.
#'
#' @return A list with:
#' \describe{
#'   \item{clique_idx}{Vector of node indices in the maximal partial clique.}
#'   \item{edge_density}{Density of the subgraph induced by clique_idx.}
#' }
#' @export
compute_maximal_partial_clique1 <- function(adj_mat, alpha, verbose = FALSE) {
  # --- Input Validation ---
  if (!is.matrix(adj_mat)) stop("adj_mat must be a matrix.")
  if (!all(adj_mat %in% c(0, 1))) stop("adj_mat must only contain 0s and 1s.")
  if (!isSymmetric(adj_mat)) stop("adj_mat must be symmetric.")
  if (any(diag(adj_mat) != 1)) stop("Diagonal of adj_mat must be all 1s.")
  if (!is.null(rownames(adj_mat)) || !is.null(colnames(adj_mat)))
    stop("adj_mat must not have row or column names.")
  n <- nrow(adj_mat)
  if (n < 5 || n > 50) stop("adj_mat must be between 5 and 50 rows/columns.")
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1)
    stop("alpha must be a single numeric between 0.5 and 1.")
  
  # --- Greedy heuristic to find a large partial clique ---
  best_clique <- c()
  best_density <- 0
  
  # Start from every node to find a good seed
  for (i in 1:n) {
    current <- c(i)
    candidates <- setdiff(1:n, i)
    repeat {
      added <- FALSE
      for (j in candidates) {
        test_clique <- c(current, j)
        density <- get_density_1(test_clique, adj_mat)
        if (density >= alpha) {
          current <- test_clique
          candidates <- setdiff(candidates, j)
          added <- TRUE
          break
        }
      }
      if (!added) break
    }
    density <- get_density_1(current, adj_mat)
    if (length(current) > length(best_clique) ||
        (length(current) == length(best_clique) && density > best_density)) {
      best_clique <- current
      best_density <- density
    }
  }
  
  if (verbose) {
    message("Max clique size: ", length(best_clique))
    message("Edge density: ", round(best_density, 4))
  }
  
  return(list(
    clique_idx = sort(best_clique),
    edge_density = best_density
  ))
}

# --- Helper: Compute edge density for a set of nodes ---
get_density_1 <- function(subgraph, adj_mat) {
  m <- length(subgraph)
  if (m < 2) return(1)
  total_possible_edges <- m * (m - 1) / 2
  total_edges <- sum(adj_mat[subgraph, subgraph]) - m  # exclude diagonals
  total_edges <- total_edges / 2  # undirected
  return(total_edges / total_possible_edges)
}