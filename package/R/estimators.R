#' @export
linkage_estimate <- function(type = "BRL", chain, n1, n2, l_10 = 1, l_01 = 1, l_11 = 2, b = 1, step=NULL, mc.cores=1) {
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
    if (is.null(step)) {
      step = floor(sqrt(n2))
    }
    kLinksLinkage <- function(k) {
      delta <- as.matrix(deltaMatrix_from_BRLchain(chain, n1, n2, k = k))
      result <- lsap(delta, k = k) # top k result
      result[result > n1] <- n1 + 1 # code all non matches to have n1 +1 index
      valid_indices <- which(result <= n1)
      res_score <- sum(delta[result[valid_indices], valid_indices])
      return(list(
        score = res_score,
        linkage = result
      ))
    }
    
    grid0 = seq(0, n2, by=step)
    results = parallel::mclapply(grid0, kLinksLinkage, mc.cores=mc.cores)
    if (step > 1) {
      k0 = which.max(map(results, "score"))
      grid1 = c(
        seq(max(0, k0 - step+1), k0-1), 
        seq(k0+1, min(k0 + step-1, n2))
      )
      results1 = parallel::mclapply(grid1, kLinksLinkage, mc.cores=mc.cores)
      results = c(results, results1)
    }
    scores = map(results, "score")
    linkages = map(results, "linkage")
    
    return(linkages[[which.max(scores)]])
  } else {
    stop("you must provide a valid model type")
  }
}
