library(clue)
library(BRL)




#LSAP function

create_LSAP_Matrix <- function(posterior,l_10, l_01, l_11){
  n1 = nrow(posterior)-1
  n2 = ncol(posterior)
  
  lsap = matrix(NA, nrow=(n1+n2), ncol=n2)
  
  
  for(i in 1:nrow(lsap)){
    
    if(i <= n1){
      lsap[i,] =  l_01*posterior[n1+1, ] + l_11*(1-posterior[i,] - posterior[n1+1,])
    }
    else{
      lsap[i,] = 10^10
      lsap[i, i-n1] = l_10*(1-posterior[n1+1,i-n1])
      
    }
    
  }
  return(lsap)
}






