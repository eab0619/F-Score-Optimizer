#' @export
bipartiteLinkageVector_from_bipartiteLinkageGraph <- function(graph, n1, n2) {
  edges = igraph::as_edgelist(graph)
  Z = n1 + (1:n2)
  Z[edges[, 2] - n1] = edges[, 1]
  
  return(Z)
}

#' @export
bipartiteLinkageGraph_from_bipartiteLinkageVector <- function(Z, n1, n2) {
  I = Z <= n1
  edges = matrix(c(n1 + (1:n2)[I], Z[I]), ncol=2)
  
  graph = igraph::make_bipartite_graph(types=c(rep(TRUE, n1), rep(FALSE, n2)), edges=c(t(edges)))
  
  return(graph)
}

#' @export
coreferenceMatrix_from_bipartiteLinkageVector <- function(Z, n1, n2) {
  assert::assert(isBipartiteLinkageVector(Z, n1, n2))
  
  graph = bipartiteLinkageGraph_from_bipartiteLinkageVector(Z, n1, n2)
  types = igraph::get.vertex.attribute(graph)$type
  
  return(graph[types, !types])
}

#' @export
adjacencyMatrix_from_bipartiteWeightsMatrix <- function(matrix, n1, n2) {
  assert::assert(isBipartiteWeightsMatrix(matrix, n1, n2))
  
  n = n1+n2
  adjMatrix = Matrix::Matrix(data=0, nrow=n, ncol=n)
  adjMatrix[1:n1, n1 + (1:n2)] = matrix
  adjMatrix[n1 + (1:n2), 1:n1] = Matrix::t(matrix)
  
  return(adjMatrix)
}

#' @export
bipartiteWeightedGraph_from_bipartiteWeightsMatrix <- function(matrix, n1, n2) {
  assert::assert(isBipartiteWeightsMatrix(matrix, n1, n2))

  adjMatrix = adjacencyMatrix_from_bipartiteWeightsMatrix(matrix, n1, n2)
  
  return(igraph::graph_from_adjacency_matrix(adjMatrix, mode="undirected", weighted=TRUE))
}

#' @export
bipartiteWeightsMatrix_from_bipartiteWeightedGraph <- function(graph, n1, n2) {
  assert::assert(isBipartiteWeightedGraph(graph, n1, n2))
  
  types = igraph::get.vertex.attribute(graph)$type
  
  return(graph[types, !types])
}