#' Compute Maximal Partial Clique
#'
#' Given an adjacency matrix and an edge density threshold \code{alpha},
#' this function computes a maximal partial clique — a subset of nodes whose
#' induced subgraph has edge density at least \code{alpha}.
#'
#' @param adj_mat A symmetric binary (0/1) matrix with 1s on the diagonal, and no row/column names.
#' Must have between 5 and 50 rows/columns.
#' @param alpha A single numeric value between 0.5 and 1 (inclusive) specifying the minimum required edge density.
#'
#' @return A list with the following elements:
#' clique_idx, A numeric vector of node indices forming the maximal partial clique.
#' edge_density, The edge density (between 0 and 1) of the returned clique.
#'
#' @export
#'
#' @examples
#' adj <- matrix(1, nrow = 6, ncol = 6)
#' adj[1, 2] <- adj[2, 1] <- 0
#' diag(adj) <- 1
#' result <- compute_maximal_partial_clique10(adj, 0.9)
#' print(result)
compute_maximal_partial_clique10 <- function(adj_mat, alpha) {
  
  # Checking inputs
  stopifnot(
    is.matrix(adj_mat),
    is.numeric(adj_mat),
    all(adj_mat == t(adj_mat)),
    all(adj_mat %in% c(0, 1)),
    all(diag(adj_mat) == 1),
    is.null(rownames(adj_mat)),
    is.null(colnames(adj_mat)),
    nrow(adj_mat) >= 5, nrow(adj_mat) <= 50,
    is.numeric(alpha),
    length(alpha) == 1,
    alpha >= 0.5, alpha <= 1
  )
  
  # Main Logic
  n <- nrow(adj_mat)
  best_clique <- c(1)
  for (start in 1:n) {
    clique <- find_partial_clique_from_node10(
      adj_mat = adj_mat,
      alpha = alpha, 
      n = n, 
      start_node = start
    )
    if (length(clique) > length(best_clique)) {
      best_clique <- clique
    }
  }
  
  # Final density
  final_density <- edge_density_10(adj_mat[best_clique,best_clique])
  
  return(list(
    clique_idx = sort(best_clique),
    edge_density = final_density
  ))
  
}

# Greedy Heuristic Search Helper function (GPT Assisted)
find_partial_clique_from_node10 <- function(adj_mat,
                                            alpha, 
                                            n, 
                                            start_node) {
  candidate <- start_node
  others <- setdiff(1:n, candidate)
  
  repeat {
    added <- FALSE
    best_node <- NULL
    best_density <- 0
    
    for (node in others) {
      test_clique <- c(candidate, node)
      dens <- edge_density_10(adj_mat[test_clique,test_clique])
      
      if (dens >= alpha && length(test_clique) > length(candidate)) {
        if (dens > best_density) {
          best_node <- node
          best_density <- dens
          added <- TRUE
        }
      }
    }
    
    if (added) {
      candidate <- c(candidate, best_node)
      others <- setdiff(others, best_node)
    } else {
      break
    }
  }
  
  return(candidate)
}


# Density Helper function
edge_density_10 <- function(subgraph) {
  m <- nrow(subgraph)
  if (m <= 1) return(1)
  total_edges <- (sum(subgraph) - m) / 2
  max_edges <- m * (m - 1) / 2
  total_edges / max_edges
}
