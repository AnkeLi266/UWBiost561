#' Compute a Maximal Partial Clique
#'
#' This function attempts to find the largest subset of nodes from a binary adjacency matrix
#' such that the induced subgraph has an edge density of at least \code{alpha}. The approach
#' uses a greedy heuristic: for each node in the graph, it iteratively adds neighboring nodes
#' that most increase the edge density until no further additions satisfy the required threshold.
#' This method does not guarantee the global optimum, but returns a reasonably large and dense
#' partial clique within a practical time frame.
#'
#' The final result includes the indices of the selected nodes and the achieved edge density.
#' 
#' @param adj_mat A symmetric binary adjacency matrix with 1s on the diagonal and no row/col names.
#' @param alpha A numeric scalar between 0.5 and 1. Minimum required edge density.
#' @param verbose Logical; if TRUE, prints progress. Default is FALSE.
#'
#' @return A list with:
#' \describe{
#'   \item{clique_idx}{Numeric vector of node indices forming the partial clique.}
#'   \item{edge_density}{The actual edge density among these nodes.}
#' }
#'
#' @export
compute_maximal_partial_clique5 <- function(adj_mat, alpha, verbose = FALSE) {
  # -------------------
  # Input Checks
  # -------------------
  if (!is.matrix(adj_mat)) stop("adj_mat must be a matrix.")
  if (!all(adj_mat %in% c(0, 1))) stop("adj_mat must contain only 0s and 1s.")
  if (!all.equal(adj_mat, t(adj_mat))) stop("adj_mat must be symmetric.")
  if (!all(diag(adj_mat) == 1)) stop("Diagonal of adj_mat must be all 1s.")
  if (!is.null(rownames(adj_mat)) || !is.null(colnames(adj_mat))) stop("adj_mat must not have row or column names.")
  if (!(nrow(adj_mat) >= 5 && nrow(adj_mat) <= 50)) stop("adj_mat must be 5 to 50 rows/columns.")
  
  if (!(is.numeric(alpha) && length(alpha) == 1 && alpha >= 0.5 && alpha <= 1)) {
    stop("alpha must be a numeric scalar between 0.5 and 1.")
  }
  
  n <- nrow(adj_mat)
  
  # -------------------
  # Greedy heuristic
  # -------------------
  best_clique <- c()
  best_density <- 0
  
  for (start_node in 1:n) {
    candidate <- c(start_node)
    others <- setdiff(1:n, candidate)
    
    repeat {
      densities <- sapply(others, function(j) {
        temp_clique <- c(candidate, j)
        dens <- edge_density_5(adj_mat[temp_clique, temp_clique, drop = FALSE])
        return(dens)
      })
      
      if (all(densities < alpha)) break  # No more valid additions
      
      max_density <- max(densities)
      best_j <- others[which.max(densities)]
      
      candidate <- c(candidate, best_j)
      others <- setdiff(others, best_j)
    }
    
    dens_final <- edge_density_5(adj_mat[candidate, candidate, drop = FALSE])
    
    if (length(candidate) > length(best_clique) || 
        (length(candidate) == length(best_clique) && dens_final > best_density)) {
      best_clique <- candidate
      best_density <- dens_final
    }
    
    if (verbose) cat("Start node", start_node, "- clique size:", 
                     length(candidate), "- density:", 
                     round(dens_final, 3), "\n")
  }
  
  return(list(
    clique_idx = sort(best_clique),
    edge_density = best_density
  ))
}

# -------------------
# Helper: Edge density function
# -------------------
edge_density_5 <- function(submat) {
  m <- nrow(submat)
  if (m <= 1) return(1)
  num_edges <- (sum(submat) - m) / 2  # remove self-loops
  max_possible <- m * (m - 1) / 2
  return(num_edges / max_possible)
}