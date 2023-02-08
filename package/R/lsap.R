
#' Run LSAP (conditional on number of links) from a weights matrix.
#' 
#' @export
lsap <- function(weightsMatrix, k=NULL, C=1e6) {
  n1 = dim(weightsMatrix)[[1]]
  n2 = dim(weightsMatrix)[[2]]

  assert::assert(k >= 0, k <= min(n1, n2))
  
  if (is.null(k)) {
    k = min(n1, n2)
  }
  
  if (n1 >= n2) {
    nAddRows = n2 - k
    n1.tilde = n1+n2-k
    extraRows = Matrix::Matrix(data=C, nrow=nAddRows, ncol=n2)
    weights = rbind(weightsMatrix, extraRows)
    
    graph = weightedBigraph_from_weightsMatrix(weights, n1.tilde, n2)
    
    matching = igraph::max_bipartite_match(graph)
    Z = matching$matching[(n1.tilde+1):(n1.tilde+n2)]
    Z[Z>n1] = n1 + which(Z>n1)
    
    return(Z)
  } else {
    stop("n1 should be greater than n2")
  }
}
