library(clue)
library(BRL)

#sourcing helper functions
source("scripts/lsap_helper.R")
source("scripts/create_delta_helper.R")
source("package/R/lsapWeights.R")





#l_10, l_01, l_11 are cost parameters to BRL
# chain is n2 x I, where I is the number of iterations, each element of the chain is the index i from dataset 1
linkage_estimate <- function(type='BRL',chain,l_10 = 1, l_01 =1, l_11 =2, b=1) {
  
  #create posterior from chain
  n1 = max(chain)-1 #the max number of chain is the index assigned to a non-link,  which is one extra than the number of records in dataset 1
  n2 = nrow(chain) # the row count is the number of records in dataset 2
  if(n2>n1){
    stop("dataset 2 must be smaller than dataset 1")
  }
  chain[chain > n1+1] <- n1+1 #redundant but defensive
  tableLabels <- apply(chain, 1, tabulate, nbins=max(chain))
  posterior <- tableLabels/ncol(chain) #last row of the posterior has the non-link
  
  #BRL linkage Bayes estimate
  if(type=='BRL'){
    #create delta matrix for BRL
    delta <- create_BRL_Delta_Matrix(posterior,l_10, l_01, l_11)
    result <- lsap(delta, k=NULL) #k=NULL defaults to traditional lsap
    result[result>n1] = n1+1 #code all non matches to have n1 +1 index
    return(result)
  }
  
  #F score bayes estimate
  if(type=='F'){
    #outer list to keep track of top k linkage
    outer<- list()
    #outer f score vector for each top k linkage 
    score <- c()
    #since n2=min(n1,n2), the top k selection only goes up to n2
    #starting inner maximization
    for(k in 1:n2){
      #create delta matrix
      #delta <- create_F_Delta_K(chain=chain,posterior=posterior,tableLabels=tableLabels,k=k,b=b) 
      delta <- as.matrix(deltaMatrix_from_BRLchain(chain, n1, n2, k=k))
      result <- lsap(delta, k=k) #top k result
      result[result>n1] = n1+1 #code all non matches to have n1 +1 index
      
      res_score <- 0     #calculate score for result
      for(j in 1:length(result)){
        if(result[j] <=n1){ #if it is a match in dataset1
          res_score <- res_score + delta[result[j],j] #delta matrix contains scores
        }
      }
      
      score <- c(score, res_score)  #append result score to score vector
      outer <- c(outer, list(result)) #append result to outer list
      
 
    }
    
    #outer maximization
    return(outer[[which.max(score)]])
    ###below is for testing purposes
    #return(list(score=score, links=outer))

  }
  else{
    stop("you must provide a valid model type")
  }
  
}

#################################### Testing code
#read in testing object
testing <- readRDS("data-raw/testing.rds")

#### testing BRL estimate
test1 <- linkage_estimate(chain=testing$chain)




#### testing F estimate
test2 <- linkage_estimate(type="F", chain=testing$chain)
#test2_optim <- test2$links[[which.max(test2$score)]]

#sum(test2_optim!=testing$data$true_index) #misclassification
#sum(test2_optim<=max(testing$chain)-1) #induced population size


#################################### Testing code









