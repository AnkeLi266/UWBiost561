#' Compute maximum partial clique.
#'
#' @param adj_mat Matrix. Adjacency matrix to search.
#' @param alpha Numeric. Minimum edge density of any potential partial clique.
#' @param verbose boolean
#'
#' @return List.
#'   \item{clique_idx}{Nodes in the found maximum partial clique.}
#'   \item{edge_density}{Edge density of found clique. Should be above alpha.}
#'
#'@export
compute_maximal_partial_clique3 = function(adj_mat, alpha, verbose = 0){
  stopifnot(all(diag(adj_mat) == 1), all(adj_mat %in% c(0, 1), all(adj_mat == t(adj_mat)), nrow(adj_mat) == ncol(adj_mat)))
  stopifnot(is.null(rownames(adj_mat)) && is.null(colnames(adj_mat)))
  stopifnot(nrow(adj_mat) >= 5, nrow(adj_mat) <= 50)
  stopifnot(is.numeric(alpha), alpha >= .5, alpha <= 1)
  
  n =  nrow(adj_mat)
  clique_idx =  integer(0)
  final_edge_density =  0
  clique_size = 0
  
  if (n > 10 & verbose > 0) {
    print("Number of nodes is > 10. For reasonable efficiency, this function can only find cliques of maximum size 10.")
  }
  
  for (m in min(10, n):2) {
    node_combinations =  combn(1:n, m)
    
    for (i in 1:ncol(node_combinations)) {
      candidate =  node_combinations[, i]
      subgraph =  adj_mat[candidate, candidate]
      
      num_edges  = sum(subgraph[upper.tri(subgraph)])
      max_edges =  m * (m - 1) / 2
      edge_density =  num_edges / max_edges
      
      if (edge_density >= alpha) {
        clique_idx =  candidate
        final_edge_density = edge_density
        break
      }
      
    }
    if (length(clique_idx) > 0) break
    
  }
  
  return(list(clique_idx = clique_idx, edge_density = final_edge_density))
}