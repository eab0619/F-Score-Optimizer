metrics <- function(predicted, actual,chain){
  n1 = max(chain)-1
  tp = sum((actual==predicted) & 
             (predicted<=n1))
  
  pred_pos = sum(predicted<=n1)
  
  
  recall = tp/sum(actual<=n1)
  ## setting recall =0 when denominator is zero 
  recall = ifelse(is.nan(recall), 0, recall)
  
  precision = tp/sum(predicted<=n1)
  
  ##override precision if predict all non matches
  if(pred_pos==0){
    precision = 1
  }
  ## setting precision =0 when denominator is zero 
  precision = ifelse(is.nan(precision), 0, precision)
  
  
  
  ## creating f score, B=1
  f_score =  2*(precision*recall)/(precision+recall)
  f_score = ifelse(is.nan(f_score), 0, f_score)
  
  ## creating ratio of precision and recall
  ratio = ifelse(recall==0,Inf ,precision/recall)
  
  
  overlap <- apply(chain, 2, function(x){sum(x<=n1)})
  interval <- quantile(overlap, c(0.025, 0.975)) #population size interval from Gibbs
  
  induced_overlap = sum(predicted<=n1)
  
  return(list(precision=precision, recall=recall, p_r_ratio = ratio, f_score = f_score,
              lower = interval[1], upper = interval[2], 
         induced_pop=induced_overlap, actual_pop=sum(actual<=n1)))
}