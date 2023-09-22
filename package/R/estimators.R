#' @export
linkage_estimate <- function(type = "BRL", chain, n1, n2, l_10 = 1, l_01 = 1, l_11 = 2, b = 1) {
  assert::assert(n2 <= n1, msg="dataset 2 must be smaller than dataset 1")
  
  # create posterior from chain
  chain[chain > n1 + 1] <- n1 + 1 # redundant but defensive
  tableLabels <- apply(chain, 1, tabulate, nbins = max(chain))
  posterior <- tableLabels / ncol(chain) # last row of the posterior has the non-link

  # BRL linkage Bayes estimate
  if (type == "BRL") {
    # create delta matrix for BRL
    delta <- sadinle_weights_matrix(posterior, n1, n2, l_10, l_01, l_11)
    result <- lsap(delta, k = NULL) # k=NULL defaults to traditional lsap
    result[result > n1] <- n1 + 1 # code all non matches to have n1 +1 index
    return(result)
  }

  # F score bayes estimate
  if (type == "F") {
    # outer list to keep track of top k linkage
    outer <- list()
    # outer f score vector for each top k linkage
    score <- c()
    # since n2=min(n1,n2), the top k selection only goes up to n2 starting inner maximization
    for (k in 1:n2) {
      # create delta matrix delta <- create_F_Delta_K(chain=chain,posterior=posterior,tableLabels=tableLabels,k=k,b=b)
      delta <- as.matrix(deltaMatrix_from_BRLchain(chain, n1, n2, k = k))
      result <- lsap(delta, k = k) # top k result
      result[result > n1] <- n1 + 1 # code all non matches to have n1 +1 index

      valid_indices <- which(result <= n1)
      res_score <- sum(delta[result[valid_indices], valid_indices])
      
      score <- c(score, res_score) # append result score to score vector
      outer <- c(outer, list(result)) # append result to outer list
    }

    # outer maximization
    return(outer[[which.max(score)]])
  } else {
    stop("you must provide a valid model type")
  }
}
