
run_sim <- function(qbdata, alpha_kick, beta_kick, nsim=10000){
  library(rstan)
  
  #Model 3: Yards given completion
  subcompleted <- qbdata %>%
    filter(complete_pass == 1) %>% 
    mutate(passing_yards = ifelse(passing_yards <= 0, 0.5, passing_yards))
  
  #Create a list of the data 
  if (sum(subcompleted$touchdown == 1) > 0){
    stan_data <- list(
      y_obs = subcompleted$passing_yards[subcompleted$touchdown == 0],
      N_obs = sum(subcompleted$touchdown == 0),
      c = subcompleted$passing_yards[subcompleted$touchdown == 1],
      N_cens = sum(subcompleted$touchdown == 1)
    )
    
    fit_rstan <- stan(
      file = "./R/yardsmodel.stan",
      data = stan_data,
      cores = 4
    )
    
    mu <- extract(fit_rstan)$mu
    sigma <- extract(fit_rstan)$sigma
  } else {
    stan_data <- list(
      y_obs = subcompleted$passing_yards,
      N_obs = nrow(subcompleted)
    )
    
    fit_rstan <- stan(
      file = "./R/yardsmodelnotd.stan",
      data = stan_data, 
      cores = 4
    )
    
    mu <- extract(fit_rstan)$mu
    sigma <- extract(fit_rstan)$sigma
  }
  
  # id <- sample(1:length(mu),1)
  # #sampling yards
  # exp(rnorm(1,mu[id],sigma[id]))
  out <-  mclapply(1:nsim,function(i){drive_sim(qbdata,mu,sigma, alpha_kick, beta_kick)}, mc.cores = 4) %>% unlist()
  return(list(scores = out, fit_rstan = fit_rstan))
}
