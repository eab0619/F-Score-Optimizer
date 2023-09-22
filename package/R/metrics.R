#' @export
metrics <- function(predicted, actual, chain, n1, n2) {
  tp <- sum((actual == predicted) & (predicted <= n1))
  pred_links_count <- sum(predicted <= n1)
  true_links_count <- sum(actual <= n1)
  
  precision <- ifelse(pred_links_count > 0, tp / pred_links_count, 0)
  recall <- ifelse(true_links_count > 0, tp / true_links_count, 0)
  f_score <- 2 * (precision * recall) / (precision + recall)
  
  p_r_ratio <- ifelse(recall == 0, Inf, precision / recall)
  
  overlap <- apply(chain, 2, function(x) {
    sum(x <= n1)
  })
  interval <- quantile(overlap, c(0.025, 0.975)) # population size interval from Gibbs
  induced_overlap <- sum(predicted <= n1)
  
  true_pop_size = n1 + n2 - true_links_count
  estimated_pop_size = n1 + n2 - pred_links_count
  pop_size_rel_bias = (estimated_pop_size - true_pop_size) / true_pop_size
  
  delta = as.matrix(FScoreBRL::deltaMatrix_from_BRLchain(chain, n1, n2, k=pred_links_count))
  valid_indices <- which(predicted <= n1)
  est_f_score <- sum(delta[predicted[valid_indices], valid_indices])
  
  return(list(
    precision = precision,
    recall = recall,
    p_r_ratio = p_r_ratio,
    f_score = f_score,
    est_f_score = est_f_score,
    lower = interval[1],
    upper = interval[2],
    induced_overlap = induced_overlap,
    true_overlap = sum(actual <= n1),
    true_pop_size = true_pop_size,
    estimated_pop_size = estimated_pop_size,
    pop_size_rel_bias = pop_size_rel_bias
  ))
}