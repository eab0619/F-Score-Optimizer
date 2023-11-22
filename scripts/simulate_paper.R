#simulate.R  returns chain

simulate <- function(n1=1000, n2=50,
                     pi=0.25,
                     num_field=3,
                     field_lvl=c(2,2,2),
                     field_type=c("bi", "bi", "bi"),
                     field_name = c("fname",
                                    "lname",
                                    "city"),
                     m = list(c(0.93, 0.07),
                              c(0.93, 0.07),
                              c(0.98,0.02)
                     ),
                     u = list(c(0.06,0.94),
                              c(0.06,0.94),
                              c(0.02,0.98)
                     ),
                     num_chain=10000, burnin=1000)

{
  if(n2>n1){
    stop("n2 must be smaller than n1") #check for file size restriction
  }
  #set.seed(seed)
  
  #create index for data1 and data2
  data1_index <- seq(1, n1)
  data2_index <- seq(1, n2)
  

  #get which records in file 2 link to records in file 1
  overlap <- rbinom(n2, 1, pi)
  #overlap <- rep(0,n2)
  #overlap[1:round(pi*n2)] <- 1
  overlap_size= sum(overlap)
  
  #randomly assign bipartite matching after
  #obtaining which records in file 2 link
  match_index = rep(n1+1, length(data2_index))
  match_index[overlap==1] = sample(data1_index, sum(overlap), replace = F)
  
  #get final match dataframe
  match_df = data.frame(data2_index = data2_index,
                        true_index = match_index)
  
  #initialize comparision vector
  comp_vec <- match_df %>% 
    slice(rep(1:n(), each=n1)) %>% 
    mutate(data1_index = rep(1:n1,n2)) %>%
    mutate(match_ind = data1_index==true_index, id=row_number()) 
  
  
  #id and comparison vectors for the matches
  id = comp_vec %>% filter(match_ind==T) %>% pull(id)
  combined_m = cbind(id)
  
  
  
  for(mu_draw in m){

    #draw the comparison vectors given a match
    col = rmultinom(overlap_size, 1, c(mu_draw)) %>% t()
    combined_m = cbind(combined_m, col)
  }
  
  #id and comparison vectors for the non-matches
  id = comp_vec %>% filter(match_ind==F) %>% pull(id)
  combined_u = cbind(id)
  
  
  for(u_draw in u){
    #draw u vector
    # sample the comparison vectors given its not a match
    col = rmultinom(nrow(comp_vec) - overlap_size, 1, c(u_draw)) %>% t()
    combined_u = cbind(combined_u, col)
  }
  
  combined_total <- data.frame(rbind(combined_m, combined_u)) %>% arrange(id)
  comparison_vector <- cbind(combined_total,comp_vec %>% dplyr::select(data1_index, data2_index, true_index))
  
  #set up Mauricio's Model
  comp_final <- combined_total[,-1]==1
  
  file1 <- field_name
  file2 <- field_name
  types <- field_type
  
  nDisagLevs <- field_lvl
  compFields <- data.frame(file1=file1, file2=file2, types= types)
  
  res <- list(comparisons = comp_final, n1 = n1, n2 = n2, nDisagLevs = nDisagLevs, 
              compFields=compFields)
  
  
  
  chain <- BRL::bipartiteGibbs(cd = res, nIter = num_chain) #gibbs sampler BRL
  chain_final <- chain$Z[, -c(1:burnin)] #toss out burn in
  chain_final[chain_final > n1+1] <- n1+1 #standardize non links to have n1+1 index
  
  
  
  return(list(chain= chain_final, comparison=comparison_vector, ground_truth =match_df$true_index))
}


#test<- simulate()



########################################## Testing Code
# 
# test <- simulate(m = list(c(5000, 1000),
#                           c(5000, 1000),
#                           c(5000,1000)),
#                  u = list(c(1000,5000),
#                           c(1000,5000),
#                           c(400,5000))
# )
# 
# test2 <- linkage_estimate(type="F",chain=test$chain)
# 
# #check individual noise rates for non matches
# test$comparison %>% 
#   filter(data1_index != true_index) %>% 
#   summarise(across(starts_with("V"),mean))
# 
# 
# #check individual noise rates for matches
# test$comparison %>% 
#   filter(data1_index == true_index) %>% 
#   summarise(across(starts_with("V"),mean))
# 
# 
# 


