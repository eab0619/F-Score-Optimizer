source("scripts/linkage_estimate.R")
source("scripts/simulate_paper.R")
source("scripts/metrics.R")

scenarios <- list(low_25_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                               c(0.93, 0.07),
                                                                               c(0.98,0.02)),
                                                                      u = list(c(0.06,0.94),
                                                                               c(0.06,0.94),
                                                                               c(0.02,0.98)), pi=0.25)},
                  low_50_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                               c(0.93, 0.07),
                                                                               c(0.98,0.02)),
                                                                      u = list(c(0.06,0.94),
                                                                               c(0.06,0.94),
                                                                               c(0.02,0.98)), pi=0.5)},
                  low_75_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                               c(0.93, 0.07),
                                                                               c(0.98,0.02)),
                                                                      u = list(c(0.06,0.94),
                                                                               c(0.06,0.94),
                                                                               c(0.02,0.98)), pi=0.75)},
                  low_100_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                                c(0.93, 0.07),
                                                                                c(0.98,0.02)),
                                                                       u = list(c(0.06,0.94),
                                                                                c(0.06,0.94),
                                                                                c(0.02,0.98)), pi=1)},
                  
                  moderate_25_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                           c(0.83, 0.17),
                                                                           c(0.98,0.02)),
                                                                  u = list(c(0.16,0.84),
                                                                           c(0.16,0.84),
                                                                           c(0.02,0.98)), pi=0.25)},
                  moderate_50_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                           c(0.83, 0.17),
                                                                           c(0.98,0.02)),
                                                                  u = list(c(0.16,0.84),
                                                                           c(0.16,0.84),
                                                                           c(0.02,0.98)), pi=0.50)},
                  moderate_75_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                           c(0.83, 0.17),
                                                                           c(0.98,0.02)),
                                                                  u = list(c(0.16,0.84),
                                                                           c(0.16,0.84),
                                                                           c(0.02,0.98)), pi=0.75)},
                  moderate_100_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                            c(0.83, 0.17),
                                                                            c(0.98,0.02)),
                                                                   u = list(c(0.16,0.84),
                                                                            c(0.16,0.84),
                                                                            c(0.02,0.98)), pi=1)},
                  moderate_high_25_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                           c(0.83, 0.17),
                                                                           c(0.88,0.12)),
                                                                  u = list(c(0.16,0.84),
                                                                           c(0.16,0.84),
                                                                           c(0.02,0.98)), pi=0.25)},
                  moderate_high_50_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                           c(0.83, 0.17),
                                                                           c(0.88,0.12)),
                                                                  u = list(c(0.16,0.84),
                                                                           c(0.16,0.84),
                                                                           c(0.02,0.98)), pi=0.50)},
                  moderate_high_75_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                           c(0.83, 0.17),
                                                                           c(0.88,0.12)),
                                                                  u = list(c(0.16,0.84),
                                                                           c(0.16,0.84),
                                                                           c(0.02,0.98)), pi=0.75)},
                  moderate_high_100_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                            c(0.83, 0.17),
                                                                            c(0.88,0.12)),
                                                                   u = list(c(0.16,0.84),
                                                                            c(0.16,0.84),
                                                                            c(0.02,0.98)), pi=1)}
)

#build list of eistimator functions
estimators <- list(BRL=function(chain) {linkage_estimate(type="BRL", chain)},
                   F_Score=function(chain) {linkage_estimate(type="F",chain)}
)

#first expand grid
params = expand.grid(scenario=scenarios, estimator = estimators, stringsAsFactors = FALSE)

#apply to get link estimates per scenario per algorithm
estimates = apply(params, 1, function(x){list(x$estimator(x$scenario()$chain))})

#cleaning step
un_estimates <- unlist(estimates, recursive = F)
params$estimates <- un_estimates

# apply again to get metrics
metrics <- apply(params, 1, function(x){metrics(x$estimates,x$scenario()$ground_truth,x$scenario()$chain)})

#get final df
param_names = expand.grid(scenario_name=names(scenarios), estimator_name=names(estimators), stringsAsFactors=FALSE)
final_df <- data.frame(params,param_names, metrics = I(metrics)) %>% unnest_wider(metrics)

metrics_df <- final_df

metrics_df %>% pivot_wider(id_cols = c(estimator_name), names_from = scenario_name, values_from= c(f_score, p_r_ratio)) %>% View()

order = c("low_25_perc_overlap","low_50_perc_overlap", "low_75_perc_overlap", "low_100_perc_overlap",
          "moderate_25_perc_overlap","moderate_50_perc_overlap","moderate_75_perc_overlap", 
          "moderate_100_perc_overlap", "moderate_high_25_perc_overlap","moderate_high_50_perc_overlap", 
          "moderate_high_75_perc_overlap","moderate_high_100_perc_overlap")

test <- metrics_df %>% arrange(match(scenario_name, order)) %>% 
  dplyr::select(scenario_name, estimator_name, f_score,lower,upper,induced_pop) %>%
  dplyr::select(-c(scenario_name,estimator_name)) %>% as.matrix()







####################
mc <- function(){
  source("scripts/metrics.R")
  
  scenarios <- list(low_25_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                        c(0.93, 0.07),
                                                                        c(0.98,0.02)),
                                                               u = list(c(0.06,0.94),
                                                                        c(0.06,0.94),
                                                                        c(0.02,0.98)), pi=0.25)},
                    low_50_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                        c(0.93, 0.07),
                                                                        c(0.98,0.02)),
                                                               u = list(c(0.06,0.94),
                                                                        c(0.06,0.94),
                                                                        c(0.02,0.98)), pi=0.5)},
                    low_75_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                        c(0.93, 0.07),
                                                                        c(0.98,0.02)),
                                                               u = list(c(0.06,0.94),
                                                                        c(0.06,0.94),
                                                                        c(0.02,0.98)), pi=0.75)},
                    low_100_perc_overlap = function() {simulate(m = list(c(0.93, 0.07),
                                                                         c(0.93, 0.07),
                                                                         c(0.98,0.02)),
                                                                u = list(c(0.06,0.94),
                                                                         c(0.06,0.94),
                                                                         c(0.02,0.98)), pi=1)},
                    
                    moderate_25_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                             c(0.83, 0.17),
                                                                             c(0.98,0.02)),
                                                                    u = list(c(0.16,0.84),
                                                                             c(0.16,0.84),
                                                                             c(0.02,0.98)), pi=0.25)},
                    moderate_50_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                             c(0.83, 0.17),
                                                                             c(0.98,0.02)),
                                                                    u = list(c(0.16,0.84),
                                                                             c(0.16,0.84),
                                                                             c(0.02,0.98)), pi=0.50)},
                    moderate_75_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                             c(0.83, 0.17),
                                                                             c(0.98,0.02)),
                                                                    u = list(c(0.16,0.84),
                                                                             c(0.16,0.84),
                                                                             c(0.02,0.98)), pi=0.75)},
                    moderate_100_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                              c(0.83, 0.17),
                                                                              c(0.98,0.02)),
                                                                     u = list(c(0.16,0.84),
                                                                              c(0.16,0.84),
                                                                              c(0.02,0.98)), pi=1)},
                    moderate_high_25_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                                  c(0.83, 0.17),
                                                                                  c(0.88,0.12)),
                                                                         u = list(c(0.16,0.84),
                                                                                  c(0.16,0.84),
                                                                                  c(0.02,0.98)), pi=0.25)},
                    moderate_high_50_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                                  c(0.83, 0.17),
                                                                                  c(0.88,0.12)),
                                                                         u = list(c(0.16,0.84),
                                                                                  c(0.16,0.84),
                                                                                  c(0.02,0.98)), pi=0.50)},
                    moderate_high_75_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                                  c(0.83, 0.17),
                                                                                  c(0.88,0.12)),
                                                                         u = list(c(0.16,0.84),
                                                                                  c(0.16,0.84),
                                                                                  c(0.02,0.98)), pi=0.75)},
                    moderate_high_100_perc_overlap = function() {simulate(m = list(c(0.83, 0.17),
                                                                                   c(0.83, 0.17),
                                                                                   c(0.88,0.12)),
                                                                          u = list(c(0.16,0.84),
                                                                                   c(0.16,0.84),
                                                                                   c(0.02,0.98)), pi=1)}
  )
  
  #build list of eistimator functions
  estimators <- list(BRL=function(chain) {linkage_estimate(type="BRL", chain)},
                     F_Score=function(chain) {linkage_estimate(type="F",chain)}
  )
  
  #first expand grid
  params = expand.grid(scenario=scenarios, estimator = estimators, stringsAsFactors = FALSE)
  
  #apply to get link estimates per scenario per algorithm
  estimates = apply(params, 1, function(x){list(x$estimator(x$scenario()$chain))})
  
  #cleaning step
  un_estimates <- unlist(estimates, recursive = F)
  params$estimates <- un_estimates
  
  # apply again to get metrics
  metrics <- apply(params, 1, function(x){metrics(x$estimates,x$scenario()$ground_truth,x$scenario()$chain)})
  
  #get final df
  param_names = expand.grid(scenario_name=names(scenarios), estimator_name=names(estimators), stringsAsFactors=FALSE)
  final_df <- data.frame(params,param_names, metrics = I(metrics)) %>% unnest_wider(metrics)
  
  metrics_df <- final_df
  
  #metrics_df %>% pivot_wider(id_cols = c(estimator_name), names_from = scenario_name, values_from= c(f_score, p_r_ratio)) %>% View()
  
  order = c("low_25_perc_overlap","low_50_perc_overlap", "low_75_perc_overlap", "low_100_perc_overlap",
            "moderate_25_perc_overlap","moderate_50_perc_overlap","moderate_75_perc_overlap", 
            "moderate_100_perc_overlap", "moderate_high_25_perc_overlap","moderate_high_50_perc_overlap", 
            "moderate_high_75_perc_overlap","moderate_high_100_perc_overlap")
  
  test <- metrics_df %>% arrange(match(scenario_name, order)) %>% 
    dplyr::select(scenario_name, estimator_name, f_score,lower,upper,induced_pop) %>%
    dplyr::select(-c(scenario_name,estimator_name)) %>% as.matrix()
  return(test)
  
  
}


####Simulation section


list_ans = list()
for(i in 1:1000){
  replicate <- mc()
  #names(replicate) <- paste0("replicate",i)
  list_ans[[i]] <- replicate
}
#save(list_ans,file = "list_ans.RData")


Reduce(`+`,list_ans)/1000



mc_list <- list()
for(i in 1:1000){
  replicate2<- diff(list_ans[[i]])[c(T,F),]
  mc_list[[i]] = replicate2
}
mean_matrix <- Reduce(`+`, mc_list)/1000
variance_matrix <- matrix(0, nrow=nrow(mc_list[[1]]), ncol = ncol(mc_list[[1]]))
for(mat in mc_list){
  variance_matrix <- variance_matrix + (mat-mean_matrix)^2
}
variance_matrix <- variance_matrix/1000

se_matrix <- sqrt(variance_matrix2)/sqrt(1000)



#metrics_df %>% select(scenario_name, estimator_name, f_score) %>% arrange(desc(scenario_name)) %>% 
#  pivot_wider(names_from=c(scenario_name), values_from = c(f_score))


