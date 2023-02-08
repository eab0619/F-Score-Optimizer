#' @export
BRLVector_from_linkageBigraph <- function(graph, n1, n2) {
  assert::assert(isLinkageBigraph(graph, n1, n2))
  edges = igraph::as_edgelist(graph)
  Z = n1 + (1:n2)
  Z[edges[, 2] - n1] = edges[, 1]
  
  return(Z)
}

#' @export
linkageBigraph_from_BRLVector <- function(Z, n1, n2) {
  assert::assert(isBRLVector(Z, n1, n2))
  I = Z <= n1
  edges = matrix(c(n1 + (1:n2)[I], Z[I]), ncol=2)
  
  graph = igraph::make_bipartite_graph(types=c(rep(TRUE, n1), rep(FALSE, n2)), edges=c(t(edges)))
  
  return(graph)
}

#' @export
weightsMatrix_from_BRLVector <- function(Z, n1, n2) {
  assert::assert(isBRLVector(Z, n1, n2))
  
  graph = linkageBigraph_from_BRLVector(Z, n1, n2)
  types = igraph::get.vertex.attribute(graph)$type
  
  return(graph[types, !types])
}

#' @export
adjMatrix_from_weightsMatrix <- function(matrix, n1, n2) {
  assert::assert(isWeightsMatrix(matrix, n1, n2))
  
  n = n1+n2
  adjMatrix = Matrix::Matrix(data=0, nrow=n, ncol=n)
  adjMatrix[1:n1, n1 + (1:n2)] = matrix
  adjMatrix[n1 + (1:n2), 1:n1] = Matrix::t(matrix)
  
  return(adjMatrix)
}

#' @export
weightedBigraph_from_weightsMatrix <- function(matrix, n1, n2) {
  assert::assert(isWeightsMatrix(matrix, n1, n2))

  adjMatrix = adjMatrix_from_weightsMatrix(matrix, n1, n2)
  graph = igraph::graph_from_adjacency_matrix(adjMatrix, mode="undirected", weighted=TRUE)
  igraph::V(graph)$type = c(rep(TRUE, n1), rep(FALSE, n2))
    
  return(graph)
}

#' @export
weightsMatrix_from_weightedBigraph <- function(graph, n1, n2) {
  assert::assert(isWeightedBigraph(graph, n1, n2))
  
  types = igraph::get.vertex.attribute(graph)$type
  
  return(graph[types, !types])
}