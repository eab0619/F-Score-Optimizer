library(tidyverse)

source("scripts/linkage_estimate.R")
source("scripts/simulate.R")
source("scripts/metrics.R")






### build the dataframe of scenarios/estimates/metrics


scenarios <- list(low_moderate_25_perc_overlap = function() {simulate(m = list(c(70000, 5000),
                                                                               c(70000, 5000),
                                                                               c(50000,1000)),
                                                                      u = list(c(5000,70000),
                                                                               c(10000,70000),
                                                                               c(5000,60000)), aBM=10000, bBM=30000)},
                  low_moderate_50_perc_overlap = function() {simulate(m = list(c(70000, 5000),
                                                                               c(70000, 5000),
                                                                               c(50000,1000)),
                                                                      u = list(c(5000,70000),
                                                                               c(10000,70000),
                                                                               c(5000,60000)), aBM=10000, bBM=10000)},
                  low_moderate_75_perc_overlap = function() {simulate(m = list(c(70000, 5000),
                                                                               c(70000, 5000),
                                                                               c(50000,1000)),
                                                                      u = list(c(5000,70000),
                                                                               c(10000,70000),
                                                                               c(5000,60000)), aBM=30000, bBM=10000)},
                  low_moderate_100_perc_overlap = function() {simulate(m = list(c(70000, 5000),
                                                                                c(70000, 5000),
                                                                                c(50000,1000)),
                                                                       u = list(c(5000,70000),
                                                                                c(10000,70000),
                                                                                c(5000,60000)), aBM=30000, bBM=1)},
                  
                  moderate_25_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                           c(50000, 10000),
                                                                           c(50000,1000)),
                                                                  u = list(c(10000,50000),
                                                                           c(10000,50000),
                                                                           c(1000,50000)), aBM=10000, bBM=30000)},
                  moderate_50_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                           c(50000, 10000),
                                                                           c(50000,1000)),
                                                                  u = list(c(10000,50000),
                                                                           c(10000,50000),
                                                                           c(1000,50000)), aBM=10000, bBM=10000)},
                  moderate_75_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                           c(50000, 10000),
                                                                           c(50000,1000)),
                                                                  u = list(c(10000,50000),
                                                                           c(10000,50000),
                                                                           c(1000,50000)), aBM=30000, bBM=10000)},
                  moderate_100_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                            c(50000, 10000),
                                                                            c(50000,1000)),
                                                                   u = list(c(10000,50000),
                                                                            c(10000,50000),
                                                                            c(1000,50000)), aBM=30000, bBM=1)},
                  
                  high_25_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                       c(50000, 10000),
                                                                       c(70000,30000)),
                                                              u = list(c(10000,50000),
                                                                       c(10000,50000),
                                                                       c(1000,50000)), aBM=10000, bBM=30000)},
                  high_50_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                       c(50000, 10000),
                                                                       c(70000,30000)),
                                                              u = list(c(10000,50000),
                                                                       c(10000,50000),
                                                                       c(1000,50000)), aBM=30000, bBM=30000)},
                  high_75_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                       c(50000, 10000),
                                                                       c(70000,30000)),
                                                              u = list(c(10000,50000),
                                                                       c(10000,50000),
                                                                       c(1000,50000)), aBM=30000, bBM=10000)},
                  high_100_perc_overlap = function() {simulate(m = list(c(50000, 10000),
                                                                        c(50000, 10000),
                                                                        c(70000,30000)),
                                                               u = list(c(10000,50000),
                                                                        c(10000,50000),
                                                                        c(1000,50000)), aBM=30000, bBM=1)}
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


#metrics_df %>% select(scenario_name, estimator_name, f_score) %>% arrange(desc(scenario_name)) %>% 
#  pivot_wider(names_from=c(scenario_name), values_from = c(f_score))
