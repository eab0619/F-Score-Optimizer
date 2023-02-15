library(clue)






#lsap general function
lsap <- function(delta, k=NULL){
  if(is.null(k)){
    #default k to ncol(delta)
    res <- clue::solve_LSAP(t(delta))
    return(as.vector(res))
  }
  else{
    n1 = nrow(delta)
    n2 = ncol(delta)
    lsap_aug = create_Augmented_Weight_Matrix(k, delta) #create augmented weight matrix for LSAP
    result <- as.vector(clue::solve_LSAP(t(lsap_aug),maximum=TRUE))
    
    return(result)
  }
}




#delta is a n1 x n2 matrix and is dependent on k, that is for each k there is a delta
#Augmented Weight matrix function for lsap
create_Augmented_Weight_Matrix <- function(k,delta){
  n1 = nrow(delta)
  n2 = ncol(delta)
  
  if(k==n2){
    return(delta)
  }
  else{
    aug = matrix(10^10, nrow=n2-k, ncol=n2)
    
    lsap_aug = rbind(delta, aug)
    
    return(lsap_aug)
    
  }
}


#################################### Testing code
#read in testing object
testing <- readRDS("data-raw/testing.rds")

#### testing lsap function
test1 <- lsap(k=5,testing$post)
test1[test1 <= nrow(testing$post)]



#### testing create_Augmented_Weight_Matrix
delta = testing$post[-nrow(testing$post),]
test2 <- create_Augmented_Weight_Matrix(k=5,delta)


#################################### Testing code





