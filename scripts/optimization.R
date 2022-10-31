library(BRL)

######Algo returns non match is coded as n1+1 instead of n1+j


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
optim_F <- function(posterior, iteration=1, seed=0, B=1){
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

  f_score <- (1+B^2)*tp/((1+B^2)*tp + fp + (B^2)*fn)
  
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
        # calculate f score with z[j] = i
        f_new_match = (1+B^2)*(tp +posterior[i,j])/( (1+B^2)*(tp +posterior[i,j]) +(B^2)*( fn+(1 - posterior[i,j] -posterior[n1+1,j])) +fp+(1-posterior[i,j]))
        if(f_new_match > f_score){
          
          # bipartite restriction
          if((i %in% z[-j])){
            j_prime = which(i==z)
            # only compare to records before j
            if(j_prime < j){
              #compare
              z1 =z
              z1[j] = z[j_prime]
              z1[j_prime] = z[j]
              
              tp <- calculate_tp(posterior,z1)
              #calculate false negatives
              fn <- calculate_fn(posterior,z1)
              #calculate false positives
              fp <- calculate_fp(posterior,z1)
              
              #f score for the swapped case
              f_score1 <- (1+B^2)*tp/((1+B^2)*tp + (B^2)*fn + fp)
              
              
              tp <- calculate_tp(posterior,z)
              #calculate false negatives
              fn <- calculate_fn(posterior,z)
              #calculate false positives
              fp <- calculate_fp(posterior,z)
              
              #f score for the non-swapped case
              f_score2 <- (1+B^2)*tp/((1+B^2)*tp + (B^2)*fn + fp)
              
              if(f_score1 > f_score2){
                temp = z[j]
                z[j] = z[j_prime]
                z[j_prime] = temp
                changed= TRUE
              }
              else{
                changed = FALSE
              }

            }
          }
          # no biparite restriction
          else{
          changed = TRUE
            if(f_score -f_new_match >f_score_max_change){
              f_score_max_change <- f_score -f_new_match
            }
            f_score = f_new_match
            
            z[j] = i
          }
        }
      }
      #after this step i now is the match for j that maximizes the f score
      # check non match
      f_new_non_match = (1+B^2)*tp/((1+B^2)*tp + (B^2) *(fn+(1 - posterior[n1+1,j])) + fp)
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

