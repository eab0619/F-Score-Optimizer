


## Create BRL Delta Matrix
#posterior is n1+1 x n2
create_BRL_Delta_Matrix <- function(posterior,l_10, l_01, l_11){
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




#create F optimizer delta matrix
#delta matrix should be n1 x n2, indexed by k
#posterior is n1+1 x n2
# create_F_Delta_K <- function(chain,posterior,tableLabels, k, b){
#   n1 = max(chain)-1 #the max number of chain is the index assigned to a non-link, 
#   #which is one extra than the number of records in dataset 1
#   n2 = nrow(chain) # the column size is the number of records in dataset 2
#   
#   
#   
#   delta_matrix <- matrix(NA, nrow=n1, ncol=n2)
#   sums<- apply(chain, 2, function(x){sum(x<=n1)})
#   
#   for(i in 1:n1){
#     for(j in 1:n2){
#       #threshold condition
#       if(tableLabels[i,j]==0) {
#         delta_matrix[i,j] = 0 
#       }
#       else{
#         chain_matched <- chain[j,]==i
#         aggregate_sums <- sums[chain_matched]
#         component3 <- sum((1+b^2)/((b^2)*aggregate_sums + k))
#         component2 <- 1/tableLabels[i,j]
#         component1 <- posterior[i,j]
#         delta_ij <- component1*component2* component3
#         delta_matrix[i,j] = delta_ij
#       }
#       
#     }
#   }
#   return(delta_matrix)
#   
#   
# }

#################################### Testing code

#read in testing object
testing <- readRDS("data-raw/testing.rds")

## testing create_BRL_Delta_Matrix
test1 <- create_BRL_Delta_Matrix(testing$post, 1, 1, 2)


## testing create_F_Delta_K
#test2 <- create_F_Delta_K(testing$chain,posterior =  testing$post,5, 1)


#################################### Testing code


