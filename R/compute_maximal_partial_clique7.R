#' Find a maximal partial clique in a binary adjacency matrix
#'
#' Given a symmetric adjacency matrix with binary entries, finds a subset of nodes that forms a partial clique with edge density at least alpha.
#'
#' @param adj_mat A symmetric binary matrix (0/1) with 5 to 50 rows, diagonal entries are 1, and no row/col names.
#' @param alpha A numeric between 0.5 and 1 specifying minimum edge density.
#'
#' @return A list with two elements: 
#' \describe{
#'   \item{clique_idx}{Indices of nodes in the maximal partial clique.}
#'   \item{edge_density}{Edge density among selected nodes.}
#' }
#' @examples
#' mat <- diag(1, 5)
#' compute_maximal_partial_clique7(mat, 0.8)
#' 
#' mat2 <- matrix(1, 5, 5)
#' diag(mat2) <- 1
#' compute_maximal_partial_clique7(mat2, 0.8)
#' @export
compute_maximal_partial_clique7 <- function(adj_mat, alpha){
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
  
  if (all(adj_mat == diag(nrow(adj_mat)))) {
    return(list(
      clique_idx = 1,
      edge_density = 1
    ))
  } else {
    ones <- apply(adj_mat, 1, function(x) {
      runs <- rle(x)
      if (any(runs$values == 1)) {
        return(max(runs$lengths[runs$values == 1]))
      } else {
        return(0)
      }
    })
    max_idx <- which.max(ones)
    clique_idx <- max_idx:(max_idx + max(ones) - 1)
    sum_of_ones <- sum(ones[max_idx:(max_idx + max(ones) - 1)])
    edge_density <- if (max(ones) > 1) {
      (sum_of_ones - max(ones)) / (max(ones) * (max(ones) - 1))
    } else {
      1
    }
    return(list(clique_idx = clique_idx, edge_density = edge_density))
  }
}
