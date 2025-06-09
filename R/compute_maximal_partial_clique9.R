#' Compute the maximal partial clique
#'
#' This function finds a subset of nodes forming a partial clique in the given adjacency matrix
#' such that the edge density among the nodes is at least \code{alpha}.
#'
#' The function uses a simple greedy algorithm to build candidate cliques and returns
#' the largest one it finds that satisfies the density threshold.
#'
#' @param adj_mat A symmetric 0/1 adjacency matrix with diagonal values of 1, no row/column names, and size between 5 and 50.
#' @param alpha A single numeric value between 0.5 and 1, specifying the required minimum edge density.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{clique_idx}{A numeric vector of node indices forming the identified partial clique.}
#'   \item{edge_density}{The actual edge density among the selected nodes.}
#' }
#'
#' @export
compute_maximal_partial_clique9 <- function(adj_mat, alpha) {
  # Input checks
  if (!is.matrix(adj_mat)) stop("adj_mat must be a matrix")
  if (!all(adj_mat == t(adj_mat))) stop("adj_mat must be symmetric")
  if (!all(adj_mat %in% c(0,1))) stop("adj_mat must contain only 0s and 1s")
  if (!all(diag(adj_mat) == 1)) stop("Diagonal of adj_mat must be 1")
  if (!is.null(rownames(adj_mat)) || !is.null(colnames(adj_mat))) stop("adj_mat must have no row/column names")
  n <- nrow(adj_mat)
  if (n < 5 || n > 50) stop("adj_mat must be between 5 and 50 rows/columns")

  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1) {
    stop("alpha must be a numeric value between 0.5 and 1")
  }

  best_clique <- NULL
  best_density <- 0
  best_size <- 0

  # Greedy algorithm: Start with each node, grow clique greedily
  for (i in 1:n) {
    clique <- c(i)
    candidates <- setdiff(1:n, clique)

    repeat {
      added <- FALSE
      for (j in candidates) {
        test_clique <- c(clique, j)
        m <- length(test_clique)
        num_edges <- (sum(adj_mat[test_clique, test_clique]) - m) / 2
        density <- num_edges / (m * (m - 1) / 2)
        if (density >= alpha) {
          clique <- test_clique
          candidates <- setdiff(candidates, j)
          added <- TRUE
          break
        }
      }
      if (!added) break
    }

    m <- length(clique)
    if (m > best_size) {
      best_clique <- clique
      best_size <- m
      best_density <- (sum(adj_mat[clique, clique]) - m) / 2 / (m * (m - 1) / 2)
    }
  }

  return(list(
    clique_idx = best_clique,
    edge_density = best_density
  ))
}
