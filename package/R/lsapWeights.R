
#' @export
deltaMatrix_from_BRLchain <- function(Zchain, n1, n2, k, beta=1.0) {
  sums = colSums(chains <= n1)
  n_samples = dim(chains)[[2]]
  vals = (1+beta**2) / (beta**2 * sums + k)
  j = rep(1:n2, n_samples)
  i = c(chains)
  x = rep(vals, each=n2) / n_samples
  I_sub = i <= n1
  
  return(Matrix::sparseMatrix(i=i[I_sub], j=j[I_sub], x=x[I_sub], dims=c(n1, n2), repr="T"))
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
deltaMatrix_from_list <- function(matrixChain, n1, n2, k, beta=1.0) {
  n_samples = length(matrixChain)

  deltaMatrices = lapply(1:n_samples, function(i){
    return((1+beta**2)*matrixChain[[i]] / (beta**2 * sum(matrixChain[[i]]) + k))
  })
  
  return(Reduce(`+`, deltaMatrices) / n_samples)
}

#' @export
deltaMatrix_from_probas <- function(probas, n1, n2, k, beta=1.0) {
  return((1+beta**2)*probas / (beta**2 * sum(probas) + k))
}