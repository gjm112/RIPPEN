data {
  int<lower=0> N;                 // number of observations
  vector[N] x;                    // predictor
  int<lower=0, upper=1> y[N];     // binary outcome
}

parameters {
  real alpha;                     // intercept
  real beta;                      // slope
}

model {
  // Priors
  alpha ~ normal(0, 5);
  beta  ~ normal(0, 1);

  // Likelihood
  y ~ bernoulli_logit(alpha + beta * x);
}


