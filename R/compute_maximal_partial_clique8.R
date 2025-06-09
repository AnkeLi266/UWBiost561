#' Compute maximal partial clique
#' This function computes the maximal partial clique by recursively removing the node with the least number of edges until the desired density (alpha) is obtained
#'
#' @param adj_mat An adjacency matrix with edges between a node and itself. Should contain at least 5 nodes and no more than 50.
#' @param alpha The density required to be a partial clique, excluding self-edges.
#'
#' @returns a list containing the index of the clique \code{clique_idx} and density of the clique
#' @export
compute_maximal_partial_clique8 <- function(adj_mat,
                                           alpha){
  if(length(unique(as.vector(adj_mat))) == 1){
    one_or_zero <- unique(as.vector(adj_mat)) == 1
  }else{
    one_or_zero <- all(unique(as.vector(adj_mat)) == 1:0)
  } # * The 1 should come first
  # because the 1, 1 entry is 1
  stopifnot(length(alpha) == 1,
            is.numeric(alpha),
            alpha >= .5,
            alpha <= 1)
  stopifnot(unique(diag(adj_mat)) == 1,
            one_or_zero, # *
            all(t(adj_mat) == adj_mat),
            dim(adj_mat)[1] >= 5,
            dim(adj_mat)[1] <= 50)
  
  
  n <- dim(adj_mat)[1]
  adj_mat_c <- adj_mat
  removed_nodes <- vector(mode="numeric",
                          length=n)
  clique_idx <- 1:n
  i <- 1
  while(.obtain_density_8(adj_mat_c) < alpha && i < n){
    peeled <- .peel_node_8(adj_mat_c)
    # removes node with the fewest connections
    adj_mat_c <- peeled$adj_mat
    clique_idx <- clique_idx[-peeled$node_removed]
    i <- i + 1
  }
  edge_density <- .obtain_density_8(adj_mat[clique_idx,
                                          clique_idx])
  
  return(list(clique_idx = clique_idx,
              edge_density = edge_density))
}

.peel_node_8 <- function(adj_mat){
  node_remove <- which.min(apply(adj_mat,
                                 FUN=sum,
                                 MARGIN = 1))
  adj_mat <- adj_mat[-node_remove, -node_remove]
  return(list(adj_mat = adj_mat,
              node_removed = node_remove))
}

.obtain_density_8 <- function(mat){
  m <- dim(mat)[1]
  numerator <- (sum(mat) - m)/2
  denominator <- m*(m-1)/2
  edge_density <- numerator/denominator
  return(edge_density)
}
