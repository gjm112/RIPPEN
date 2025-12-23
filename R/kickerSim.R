createKicker <- function(data) {
    # Get kicker data
    kicker_data <- subset(data,
        play_type == "field_goal" & season == 2016,
        select = c("field_goal_result", "kick_distance")
    )

    stan_data_kick <- list(
        N = nrow(kicker_data),
        x = kicker_data$kick_distance,
        y = (kicker_data$field_goal_result == "made") + 0
    )

    fit_kick <- stan(
        file = "./stan/kickers.stan",
        data = stan_data_kick,
        cores = parallel::detectCores(),
        auto_write = TRUE
    )

    alpha_kick <- extract(fit_kick)$alpha
    beta_kick <- extract(fit_kick)$beta

    # Build function to simulate a kicker
    fieldGoalSim <- function(togo) {
        id_kick <- sample(1:length(alpha_kick), 1)
        xb <- c(alpha_kick[id_kick], beta_kick[id_kick]) %*% c(1, togo + 17)
        pfg <- exp(xb) / (1 + exp(xb))
        fieldGoal <- rbinom(1, 1, pfg)
        return(fieldGoal)
    }

    return(list(
        fieldGoalSim = fieldGoalSim,
        alpha_kick = alpha_kick,
        beta_kick = beta_kick
    ))
}
