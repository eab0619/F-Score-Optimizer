#' @import RecordLinkage BRL caret clue MCMCpack dplyr
#' @export
simulate <- function(n1=1000, n2=50, 
                     aBM=1000, bBM=200,
                     num_field=3,
                     field_lvl=c(2,2,2),
                     field_type=c("bi", "bi", "bi"),
                     field_name = c("fname",
                                    "lname",
                                    "city"),
                     m = list(c(1000, 1),
                              c(1000, 1),
                              c(1000,30)
                     ),
                     u = list(c(1000,5000),
                              c(1000,5000),
                              c(400,5000)
                     ),
                     num_chain=10000, burnin=1000,
                     seed=5)

{
  if(n2>n1){
    stop("n2 must be smaller than n1") #check for file size restriction
  }
  set.seed(seed)
  
  #create index for data1 and data2
  data1_index <- seq(1, n1)
  data2_index <- seq(1, n2)
  
  #draw overlap from beta binomial
  pi <- rbeta(1, aBM, bBM)
  #get which records in file 2 link to records in file 1
  overlap <- rbinom(n2, 1, pi)
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
  
  
  
  for(mu in m){
    #draw m vector
    mu_draw = MCMCpack::rdirichlet(1, mu)
    #draw the comparison vectors given a match
    col = rmultinom(overlap_size, 1, c(mu_draw)) %>% t()
    combined_m = cbind(combined_m, col)
  }
  
  #id and comparison vectors for the non-matches
  id = comp_vec %>% filter(match_ind==F) %>% pull(id)
  combined_u = cbind(id)
  
  
  for(uu in u){
    #draw u vector
    u_draw = MCMCpack::rdirichlet(1, uu)
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