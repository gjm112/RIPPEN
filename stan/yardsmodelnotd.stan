data {
  int<lower=0> N_obs;              // number of fully observed values
  vector<lower=0>[N_obs] y_obs;    // observed (uncensored) values
}


parameters {
  real mu;                         // mean of log(y)
  real<lower=0> sigma;             // sd of log(y)
}

model {
  // Priors
  mu ~ normal(0, 5);
  sigma ~ normal(0, 2);

  // Likelihood: observed data
  y_obs ~ lognormal(mu, sigma);

  

}


