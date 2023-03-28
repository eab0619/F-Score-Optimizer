source("scripts/linkage_estimate.R")
source("scripts/simulate.R")
source("scripts/metrics.R")




## list of scenarios
## three types of noisy scenarios: Low, medium, high. Increasing noise level by field

### Low noise

#### 1) low noise in one field

#### 2) low noise in two fields

#### 3) low noise in three fields

low <- simulate(m = list(c(50000, 2000),
                                   c(50000, 2000),
                                   c(50000,2000)),
                          u = list(c(2000,50000),
                                   c(2000,50000),
                                   c(2000,50000)))




### Moderate noise

#### 1) Moderate noise in one field

mod_one <- simulate(m = list(c(50000, 10000),
                                 c(50000, 1000),
                                 c(50000,1000)),
                        u = list(c(10000,50000),
                                 c(1000,50000),
                                 c(1000,50000)))

#### 2) Moderate noise in two fields
mod_two <- simulate(m = list(c(50000, 10000),
                                 c(50000, 10000),
                                 c(50000,1000)),
                        u = list(c(10000,50000),
                                 c(10000,50000),
                                 c(1000,50000)))

#### 3) Moderate noise in three fields
mod_three <- simulate(m = list(c(50000, 10000),
                                   c(50000, 10000),
                                   c(50000,10000)),
                          u = list(c(10000,50000),
                                   c(10000,50000),
                                   c(10000,50000)))

### High noise

#### 1) High noise in one field

high_one <- simulate(m = list(c(50000, 30000),
                      c(50000, 1000),
                      c(50000,1000)),
             u = list(c(30000,50000),
                      c(1000,50000),
                      c(1000,50000)))
#### 2) High noise in two fields
high_two <- simulate(m = list(c(50000, 30000),
                                  c(50000,30000),
                                  c(50000,1000)),
                         u = list(c(30000,50000),
                                  c(30000,50000),
                                  c(1000,50000)))
#### 3) High noise in three fields
high_three <- simulate(m = list(c(50000, 30000),
                              c(50000,30000),
                              c(50000,30000)),
                     u = list(c(30000,50000),
                              c(30000,50000),
                              c(30000,50000)))






### build the dataframe of scenarios/estimates/metrics


scenarios <- list(low_noise = function() {simulate(m = list(c(50000, 1000),
                                                            c(50000, 1000),
                                                            c(50000,1000)),
                                                   u = list(c(2000,50000),
                                                            c(2000,50000),
                                                            c(2000,50000)))},
                  moderate_noise_one_field = function() {simulate(m = list(c(50000, 10000),
                                                                           c(50000, 1000),
                                                                           c(50000,1000)),
                                                                  u = list(c(10000,50000),
                                                                           c(1000,50000),
                                                                           c(1000,50000)))},
                  moderate_noise_two_field = function() {simulate(m = list(c(50000, 10000),
                                                                           c(50000, 10000),
                                                                           c(50000,1000)),
                                                                  u = list(c(10000,50000),
                                                                           c(10000,50000),
                                                                           c(1000,50000)))},
                  moderate_noise_three_field = function() {simulate(m = list(c(50000, 10000),
                                                                             c(50000, 10000),
                                                                             c(50000,10000)),
                                                                    u = list(c(10000,50000),
                                                                             c(10000,50000),
                                                                             c(10000,50000)))},
                  
                  high_noise_one_field = function() {simulate(m = list(c(50000, 30000),
                                                                       c(50000, 1000),
                                                                       c(50000,1000)),
                                                              u = list(c(30000,50000),
                                                                       c(1000,50000),
                                                                       c(1000,50000)))},
                  high_noise_two_field = function() {simulate(m = list(c(50000, 30000),
                                                                       c(50000,30000),
                                                                       c(50000,1000)),
                                                              u = list(c(30000,50000),
                                                                       c(30000,50000),
                                                                       c(1000,50000)))},
                  high_noise_three_field = function() {simulate(m = list(c(50000, 30000),
                                                                         c(50000,30000),
                                                                         c(50000,30000)),
                                                                u = list(c(30000,50000),
                                                                         c(30000,50000),
                                                                         c(30000,50000)))}
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

