#' Find the maximal partial clique in an adjacency matrix
#'
#' @param adj_mat a symmetric matrix with 5 to 50 rows
#' @param alpha a numeric value between 0.5 and 1
#'
#' @return a list containing clique_idx and edge density
#' @export
compute_maximal_partial_clique7 <- function(adj_mat, alpha){
  # check input
  stopifnot(
    is.matrix(adj_mat),
    all(adj_mat %in% c(0, 1)),
    isSymmetric(adj_mat),
    all(diag(adj_mat) == 1),
    is.null(rownames(adj_mat)),
    is.null(colnames(adj_mat)),
    nrow(adj_mat) >= 5,
    nrow(adj_mat) <= 50,
    is.numeric(alpha),
    length(alpha) == 1,
    alpha >= 0.5,
    alpha <= 1
  )
  
  # start the function
  if (all(adj_mat == diag(nrow(adj_mat)))) {
    return(list(
      clique_idx = 1,  # Or any single node (e.g., sample(1:nrow(adj_mat), 1))
      edge_density = 1  # A single node has edge density 1 by definition
    ))
  }
  else{
    ones <- apply(adj_mat, 1, max_consec_ones_7)
    max_idx <- which.max(ones)
    clique_idx <- max_idx:(max_idx+max(ones)-1)
    # calculate edge density
    sum_of_ones <- sum(ones[max_idx:(max_idx+max(ones)-1)])
    edge_density <- (sum_of_ones-max(ones))/(max(ones)*(max(ones)-1))
    return(list(clique_idx = clique_idx, edge_density = edge_density))
  }
}

# helper function
# Function to calculate max consecutive 1s in a vector
max_consec_ones_7 <- function(x) {
  runs <- rle(x)
  if (any(runs$values == 1)) {
    return(max(runs$lengths[runs$values == 1]))
  } else {
    return(0)
  }
}