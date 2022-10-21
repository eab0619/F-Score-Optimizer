library(BRL)

######Algo returns non match is coded as n1+1 instead of n1+j

#true positive for ground truth
truth_tp <- function(z, true_index,n1){
  sum((z==true_index) & (z<=n1))
}

truth_fn <- function(z, true_index, n1){
  
}

truth_fp <- function(z, true_index, n1){
  sum(z!=true_index & z<=n1)
}

truth_tn <- function(z, true_index, n1){
  
}


## helper functions
calculate_tp  <- function(posterior, z){
  tp <- 0
  n1 <- nrow(posterior)-1
  n2 <- ncol(posterior)
  for(j in 1:n2){
    if(z[j]!=n1+1){
      tp <- tp + posterior[z[j],j]
    }
    
  }
  tp
} 

calculate_fn  <- function(posterior, z){
  fn <- 0
  n1 <- nrow(posterior)-1
  n2 <- ncol(posterior)
  for(j in 1:n2){
    if(z[j]!=n1+1){
      mid = 1- posterior[z[j],j] - posterior[n1+1,j]
    }
    else {
      mid = 1-posterior[n1+1,j]
    }
    #
    #print(mid)
    fn<- fn+mid
    #fn <- fn + (z[j]==n1+1)*(1-posterior[n1+1,j])
  }
  fn
}



calculate_fp  <- function(posterior, z){
  fp <- 0
  n1 <- nrow(posterior)-1
  n2 <- ncol(posterior)
  for(j in 1:n2){
    if(z[j] != n1+1){
      fp <- fp + 1-posterior[z[j],j]
    }
  }
  fp
}




#### Iterative algorithm to optimize F score
optim_F <- function(posterior, iteration=1, seed=0){
  ### posterior is (n1+1) by (n2) posterior probabilities
  ### initialize values all to non matches
  
  n1 <- nrow(posterior)-1
  n2 <- ncol(posterior)
  
  z <- rep(n1+1,ncol(posterior))
  tp <- 0
  fn <- 0
  fp <- 0
  


    
  #calculate true positives
  tp <- calculate_tp(posterior,z)
  #calculate false negatives; offset since r index starts at 1
  fn <- calculate_fn(posterior,z)
  #calculate false positives;  offset since r index starts at 1
  fp <- calculate_fp(posterior,z)

  f_score <- 2*tp/(2*tp + fp + fn)
  
  #print(paste0("initial f: ", f_score))
  #print(paste0("initial fn: ", fn))
  
  for(k in 1:iteration){
    changed = FALSE
    f_score_max_change <- 0
    for(j in 1:n2){
      #remove the jth individual
      #print("here")
      tp <- calculate_tp(posterior[,-j], z[-j])
      fn <- calculate_fn(posterior[,-j],z[-j])
      fp <- calculate_fp(posterior[,-j],z[-j])
      #f_score_old <- 2*tp/(2*tp + fp + fn)
      
      
      
      #iterate over matches
      for(i in 1:n1){
        f_new_match = 2*(tp +posterior[i,j])/( 2*(tp +posterior[i,j]) + fn+(1 - posterior[i,j] -posterior[n1+1,j]) +fp+(1-posterior[i,j]))
        if(f_new_match > f_score){
          changed = TRUE
          if(f_score -f_new_match >f_score_max_change){
            f_score_max_change <- f_score -f_new_match
          }
          f_score = f_new_match
          
          z[j] = i
        }
      }
      #after this step i now is the match for j that maximizes the f score
      # check non match
      f_new_non_match = 2*tp/(2*tp + fn+(1 - posterior[n1+1,j]) + fp)
      if(f_new_non_match > f_score){
        changed = TRUE
        if(f_score -f_new_match >f_score_max_change){
          f_score_max_change <- f_score -f_new_match
        }
        f_score = f_new_non_match
        z[j] = n1+1
      }
      
    }
    if (!changed) {
      break
    }
    #if(f_score_max_change < 10^(-5)){
    #break
    #}
    
  }
  list(z=z, f_score =f_score)
}

