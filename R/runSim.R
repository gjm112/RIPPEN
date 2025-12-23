# Runs the drive simulations for a QB and a given number of simulations
runSim <- function(qbdata, kicker, num_sims, num_cores = 24) {
    # Model 3: Yards given completion
    subcompleted <- qbdata %>%
        filter(complete_pass == 1) %>%
        mutate(passing_yards = ifelse(passing_yards <= 0, 0.5, passing_yards))

    # Create a list of the data
    if (sum(subcompleted$touchdown == 1) > 0) {
        y_obs_values <- as.vector(subcompleted$passing_yards[subcompleted$touchdown == 0])
        N_obs <- sum(subcompleted$touchdown == 0)
        c_values <- as.vector(subcompleted$passing_yards[subcompleted$touchdown == 1])
        N_cens <- sum(subcompleted$touchdown == 1)
        # Ensure vectors are always passed as arrays, even if length 1
        stan_data <- list(
            y_obs = array(y_obs_values, dim = N_obs),
            N_obs = N_obs,
            c = array(c_values, dim = N_cens),
            N_cens = N_cens
        )
        fit_rstan <- stan(
            file = "./stan/yardsmodel.stan",
            data = stan_data,
            cores = num_cores
        )

        mu <- extract(fit_rstan)$mu
        sigma <- extract(fit_rstan)$sigma
    } else {
        y_obs_values <- subcompleted$passing_yards
        N_obs <- nrow(subcompleted)

        # Ensure vector is always passed as array, even if length 1
        stan_data <- list(
            y_obs = array(y_obs_values, dim = N_obs),
            N_obs = N_obs
        )

        fit_rstan <- stan(
            file = "./stan/yardsmodelnotd.stan",
            data = stan_data,
            cores = num_cores
        )

        mu <- extract(fit_rstan)$mu
        sigma <- extract(fit_rstan)$sigma
    }
    # id <- sample(1:length(mu), 1)
    # sampling yards
    # exp(rnorm(1, mu[id], sigma[id]))
    out <- mclapply(1:num_sims, function(i) {
        driveSim(qbdata, mu, sigma, kicker)
    }, mc.cores = num_cores) %>% unlist()
    return(list(scores = out, fit_rstan = fit_rstan))
}
