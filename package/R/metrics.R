#' @export
metrics <- function(predicted, actual, chain, n1, n2) {
  tp <- sum((actual == predicted) & (predicted <= n1))
  pred_pos <- sum(predicted <= n1)
  true_pos <- sum(actual <= n1)

  precision <- ifelse(pred_pos > 0, tp / pred_pos, 0)
  recall <- ifelse(true_pos > 0, tp / true_pos, 0)
  f_score <- 2 * (precision * recall) / (precision + recall)

  p_r_ratio <- ifelse(recall == 0, Inf, precision / recall)

  overlap <- apply(chain, 2, function(x) {
    sum(x <= n1)
  })
  interval <- quantile(overlap, c(0.025, 0.975)) # population size interval from Gibbs
  induced_overlap <- sum(predicted <= n1)

  return(list(
    precision = precision,
    recall = recall,
    p_r_ratio = p_r_ratio,
    f_score = f_score,
    lower = interval[1],
    upper = interval[2],
    induced_overlap = induced_overlap,
    actual_pop = sum(actual <= n1)
  ))
}
