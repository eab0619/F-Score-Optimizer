library(BRL)
data(twoFiles)

myCompData <- compareRecords(df1, df2, flds=c("gname", "fname", "age", "occup"), 
                             types=c("lv","lv","bi","bi"))

chain <- bipartiteGibbs(myCompData)

chain_final <- chain$Z[, -c(1:100)]

n1 <- nrow(df1)
n2 <- nrow(df2)

chain_final[chain_final > n1+1] <- n1+1
tableLabels <- apply(chain_final, 1, tabulate, nbins=max(chain_final))
tableLabels <- tableLabels/ncol(chain_final)
#probNoLink <- tableLabels[n1+1,]



# 
zhat2 <- linkRecords(chain_final, n1=nrow(df1))
zhat2[zhat2 > n1+1] <- n1+1
# 
# 
# nLinks <- sum(zhat2<=nrow(df1))
# 
# 
# post_nLinks <- apply(chain$Z, 2, function(x) {sum(x<=nrow(df1))})

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
    fn <- fn + (z[j]==n1+1)*(1-posterior[n1+1,j])
  }
  fn
}



calculate_fp  <- function(posterior, z){
  fp <- 0
  n1 <- nrow(posterior)-1
  n2 <- ncol(posterior)
  for(j in 1:n2){
    if(z[j] < n1+1)
    fp <- fp + posterior[n1+1,j]
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


  for(j in 1:n2){
    
    #calculate true positives
    tp <- calculate_tp(posterior,z)
    #calculate false negatives; offset since r index starts at 1
    fn <- calculate_fn(posterior,z)
    #calculate false positives;  offset since r index starts at 1
    fp <- calculate_fp(posterior,z)
  }
  f_score <- 2*tp/(2*tp + fp + fn)
 
  
  for(k in 1:iteration){
    changed = FALSE
    for(j in 1:n2){
      #remove the jth individual
      
      tp <- calculate_tp(posterior[,-j], z[-j])
      fn <- calculate_fn(posterior[,-j],z[-j])
      fp <- calculate_fp(posterior[,-j],z[-j])
      #f_score_old <- 2*tp/(2*tp + fp + fn)
      
      
      #iterate over matches
      for(i in 1:n1){
        f_new_match = 2*(tp +posterior[i,j])/( 2*(tp +posterior[i,j]) + fn +fp+posterior[n1+1,j])
        if(f_new_match > f_score){
          changed = TRUE
          f_score = f_new_match
          z[j] = i
        }
      }
      #after this step i now is the match for j that maximizes the f score
      # check non match
      f_new_non_match = 2*tp/(2*tp + fn+(1-posterior[n1+1,j]) + fp)
      if(f_new_non_match > f_score){
        changed = TRUE
        f_score = f_new_non_match
        z[j] = n1+1
      }
 
    }
    if (!changed) {
      break
    }
    
  }
  list(z=z, f_score =f_score)
}

check <- optim_F(tableLabels, iteration = 10000)
