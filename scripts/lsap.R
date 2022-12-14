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




  
  
test <- create_LSAP_Matrix(als_benchmark1$post, 1,1,0)

n1 <- nrow(als_benchmark1$post) -1

test2 <- clue::solve_LSAP(t(test))
test3 <- as.vector(test2)

test3[test3>n1] = n1+1

sum(als_benchmark1$data$true_index != as.vector(test3))
sum(test3<=n1)
sum(als_benchmark1$data$true_index <=n1)



test <- create_LSAP_Matrix(als_benchmark1$post, 1,1,1)

n1 <- nrow(als_benchmark1$post) -1

test2 <- clue::solve_LSAP(t(test))
test3 <- as.vector(test2)

test3[test3>n1] = n1+1

sum(als_benchmark1$data$true_index != as.vector(test3))
sum(test3<=n1)
sum(als_benchmark1$data$true_index <=n1)





test <- create_LSAP_Matrix(testing$post, 1,1,1)

n1 <- nrow(testing$post) -1

test2 <- clue::solve_LSAP(t(test))
test3 <- as.vector(test2)

test3[test3>n1] = n1+1

sum(testing$data$true_index != as.vector(test3))
sum(test3<=n1)
sum(testing$data$true_index <=n1)