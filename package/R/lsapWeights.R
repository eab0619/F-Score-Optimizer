## Create BRL Delta Matrix posterior is n1+1 x n2
#' @export
sadinle_weights_matrix <- function(Zchain, n1, n2, l_10, l_01, l_11) {
  n1 <- nrow(Zchain) - 1
  n2 <- ncol(Zchain)

  lsap <- matrix(NA, nrow = (n1 + n2), ncol = n2)


  for (i in 1:nrow(lsap)) {
    if (i <= n1) {
      lsap[i, ] <- l_01 * Zchain[n1 + 1, ] + l_11 * (1 - Zchain[i, ] - Zchain[n1 + 1, ])
    } else {
      lsap[i, ] <- 10^10
      lsap[i, i - n1] <- l_10 * (1 - Zchain[n1 + 1, i - n1])
    }
  }
  return(lsap)
}

#' Compute delta matrix from BRL chain
#'
#' @import MatrixExtra assert
#' @export
deltaMatrix_from_BRLchain <- function(Zchain, n1, n2, k, beta = 1) {
  sums <- colSums(Zchain <= n1)
  n_samples <- dim(Zchain)[[2]]
  vals <- (1 + beta^2) / (beta^2 * sums + k)
  j <- rep(1:n2, n_samples)
  x <- rep(vals / n_samples, each = n2)
  I_sub <- Zchain <= n1

  return(Matrix::sparseMatrix(i = Zchain[I_sub], j = j[I_sub], x = x[I_sub], dims = c(n1, n2), repr = "T"))
}

#' Compute delta matrix from list of coreference matrices
#'
#'
#' @note List of coreference matrices can be computed from a chain of BRL vectors as:
#' \code{
#' matrices = lapply(1:dim(chains)[[2]], function(i){
#'   Z = chains[, i]
#'   return(weightsMatrix_from_BRLVector(Z, n1, n2))
#' })
#' }
#'
#'
#' @import MatrixExtra assert
#' @export
deltaMatrix_from_list <- function(matrixChain, n1, n2, k, beta = 1) {
  n_samples <- length(matrixChain)

  deltaMatrices <- lapply(1:n_samples, function(i) {
    return((1 + beta^2) * matrixChain[[i]] / (beta^2 * sum(matrixChain[[i]]) + k))
  })

  return(Reduce(`+`, deltaMatrices) / n_samples)
}

#' @export
deltaMatrix_from_probas <- function(probas, n1, n2, k, beta = 1) {
  return((1 + beta^2) * probas / (beta^2 * sum(probas) + k))
}
