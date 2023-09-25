#' Run LSAP (conditional on number of links) from a weights matrix.
#'
#' @export
lsap2 <- function(weightsMatrix, k = NULL, C = 1e+06) {
  n1 <- dim(weightsMatrix)[[1]]
  n2 <- dim(weightsMatrix)[[2]]

  if (is.null(k)) {
    k <- min(n1, n2)
  } else {
    assert::assert(k >= 0, k <= min(n1, n2))
    assert::assert(n1 > n2)
  }

  # Data augmentation for k links constraint.
  nAddRows <- n2 - k
  n1.tilde <- n1 + n2 - k
  extraRows <- Matrix::Matrix(data = C, nrow = nAddRows, ncol = n2)
  weights <- rbind(weightsMatrix, extraRows)

  graph <- weightedBigraph_from_weightsMatrix(weights, n1.tilde, n2)

  matching <- igraph::max_bipartite_match(graph)
  Z <- matching$matching[(n1.tilde + 1):(n1.tilde + n2)]
  Z[Z > n1] <- n1 + which(Z > n1)

  return(Z)
}

# lsap general function
#' @export
lsap <- function(delta, k = NULL) {
  if (is.null(k)) {
    # default k to ncol(delta)
    res <- clue::solve_LSAP(t(delta))
    return(as.vector(res))
  } else {
    n1 <- nrow(delta)
    n2 <- ncol(delta)
    lsap_aug <- create_Augmented_Weight_Matrix(k, delta) # create augmented weight matrix for LSAP
    result <- as.vector(clue::solve_LSAP(t(lsap_aug), maximum = TRUE))

    return(result)
  }
}




# delta is a n1 x n2 matrix and is dependent on k, that is for each k there is a delta Augmented Weight matrix function for lsap
#' @export
create_Augmented_Weight_Matrix <- function(k, delta) {
  n1 <- nrow(delta)
  n2 <- ncol(delta)

  if (k == n2) {
    return(delta)
  } else {
    aug <- matrix(n2, nrow = n2 - k, ncol = n2)

    lsap_aug <- rbind(delta, aug)

    return(lsap_aug)
  }
}
