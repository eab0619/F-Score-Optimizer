#' Check that object is a bipartite linkage vector.
#' 
#' A bipartite linkage vector `Z` represents a bipartite linkage between two lists of records of sizes `n1` and `n2`. It is a vector of length `n2` defined as follows. For a given index `j`, where `1 <= j <= n2`, `Z[j] = i` if record `j` is linked to record `i`. If record `j` is not linked to any other record, then `Z_j = n_1 + j`.
#' 
#' Therefore, a valid bipartite linkage vector for lists of sizes `n1` and `n2` should satisfy the following requirements:
#' 1. It is an integer vector of length `n2`.
#' 2. Values `Z[j]` satisfy `1 <= Z[j] <= 2` or `Z[j] == n2 + j`.
#' 3. Since `Z` should represent a bipartite linkage, `Z` should not have any duplicate values.
#' 
#' @param Z object check.
#' @param n1 size of the first list of records.
#' @param n2 size of the second list of records.
#' 
#' @returns TRUE if `Z` is a bipartite linkage vector, FALSE otherwise.
#' 
#' @examples
#' Z <- c(1, 2, 4)
#' n1 <- 4
#' n2 <- 3
#' isBipartiteLinkageVector(Z, n1, n2)
#' # Returns: TRUE
#' 
#' @export
isBipartiteLinkageVector <- function(Z, n1, n2) {
  if (!is.numeric(Z)) {
    return(FALSE)
  }
  if (length(Z) != n2) {
    return(FALSE)
  }
  if (any(duplicated(Z))) {
    return(FALSE)
  }
  I = (1 <= Z) & (Z <= n1)
  if (any(Z[!I] != n1 + which(!I))) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Check that graph is a bipartite linkage graph.
#' 
#' A bipartite linkage graph is a bipartite graph with degrees at most 1.
#' 
#' @param graph object check.
#' @param n1 size of the first list of records.
#' @param n2 size of the second list of records.
#' 
#' @returns TRUE if `graph` is a bipartite linkage graph, FALSE otherwise.
#' 
#' @examples 
#' graph <- igraph::graph.bipartite(types=c(rep(TRUE, 3), rep(FALSE, 2)), edges = c(1, 4), directed = FALSE)
#' n1 <- 3
#' n2 <- 2
#' isBipartiteLinkageGraph(graph, n1, n2)
#' # Returns: TRUE
#' 
#' graph[]
#' 
#' # 5 x 5 sparse Matrix of class "dgCMatrix"
#' # 
#' # [1,] . . . 1 .
#' # [2,] . . . . .
#' # [3,] . . . . .
#' # [4,] 1 . . . .
#' # [5,] . . . . .
#' 
#' @export
isBipartiteLinkageGraph <- function(graph, n1, n2) {
  if (!igraph::is.bipartite(graph)) {
    return(FALSE)
  }
  
  # Check that each vertex is linked to at most one other.
  if (any(igraph::degree(graph) > 1)) {
    return(FALSE)
  }
  
  # Check that there are n1 vertices in the left-hand set and n2 vertices in the right-hand set.
  left_indicator = igraph::get.vertex.attribute(graph)$type
  left_vertices = igraph::V(graph)[left_indicator]
  right_vertices = igraph::V(graph)[!left_indicator]
  if (length(left_vertices) != n1 | length(right_vertices) != n2) {
    return(FALSE)
  }
  
  return(TRUE)
}


#' Check that graph is a bipartite weighted graph.
#' 
#' A bipartite weighted graph is a bipartite graph with edge weights.
#' 
#' @param graph object check.
#' @param n1 size of the first list of records.
#' @param n2 size of the second list of records.
#' 
#' @returns TRUE if `graph` is a bipartite weighted graph, FALSE otherwise.
#' 
#' @examples 
#' graph <- igraph::graph.bipartite(types=c(rep(TRUE, 3), rep(FALSE, 2)), edges = c(1, 4, 3, 5), directed = FALSE)
#' igraph::E(graph)$weight = c(1.5, 0.5)
#' n1 <- 3
#' n2 <- 2
#' isBipartiteWeightedGraph(graph, n1, n2)
#' # Returns: TRUE
#' 
#' graph[]
#' # 5 x 5 sparse Matrix of class "dgCMatrix"
#' # 
#' # [1,] .   . .   1.5 .  
#' # [2,] .   . .   .   .  
#' # [3,] .   . .   .   0.5
#' # [4,] 1.5 . .   .   .  
#' # [5,] .   . 0.5 .   .  
#' 
#' @export
isBipartiteWeightedGraph <- function(graph, n1, n2) {
  if (!igraph::is.bipartite(graph)) {
    return(FALSE)
  }
  if (!igraph::is.weighted(graph)) {
    return(FALSE)
  }
  
  # Check that there are n1 vertices in the left-hand set and n2 vertices in the right-hand set.
  left_indicator = igraph::get.vertex.attribute(graph)$type
  left_vertices = igraph::V(graph)[left_indicator]
  right_vertices = igraph::V(graph)[!left_indicator]
  if (length(left_vertices) != n1 | length(right_vertices) != n2) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Check that matrix represents weights between two record lists.
#' 
#' A bipartite weights matrix is a matrix with rows corresponding to the `n1` elements of a first list, and columns corresponding to the `n2` elements to the second list.
#' 
#' @param matrix object check. Can be a sparse matrix.
#' @param n1 size of the first list of records.
#' @param n2 size of the second list of records.
#' 
#' @returns TRUE if `matrix` is a bipartite weights matrix, FALSE otherwise.
#' 
#' @examples
#' matrix <- Matrix::Matrix(c(0,1,0,0,1,0), nrow=3, byrow=TRUE)
#' matrix
#' # 3 x 2 sparse Matrix of class "dgCMatrix"
#' # 
#' # [1,] . 1
#' # [2,] . .
#' # [3,] 1 .
#' 
#' n1 <- 3
#' n2 <- 2
#' isBipartiteWeightsMatrix(matrix, n1, n2)
#' # Returns: TRUE
#' 
#' @export
isBipartiteWeightsMatrix <- function(matrix, n1, n2) {
  if (!dim(matrix)[[1]] == n1 | !dim(matrix)[[2]] == n2) {
    return(FALSE)
  }
  
  return(TRUE)
}


