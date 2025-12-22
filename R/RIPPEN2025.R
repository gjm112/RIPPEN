library(nflreadr)
library(tidyverse)
library(rstan)
library(parallel)
source("./R/gatherData.R")
source("./R/gatherPassPlays.R")

# https://github.com/nflverse/nflverse-data/releases/tag/pbp
# data <- readRDS("./nflpbp2025/play_by_play_1999.rds")
data <- gatherData()

data_list <- list()
for (y in 1999:2024) {
    print(y)
    data_list[[y]] <- data %>% filter(season == y)
}


# boot <- glm(y ~ x,  family = "binomial")
# kickCoef <- boot$coefficients
kicker <- data_list[[1999]] %>%
    filter(play_type == "field_goal") %>%
    select(field_goal_result, kick_distance)

stan_data_kick <- list(
    N = nrow(kicker),
    x = kicker$kick_distance,
    y = (kicker$field_goal_result == "made") + 0
)

fit_kick <- stan(
    file = "./R/kickers.stan",
    data = stan_data_kick
)
fit_kick_1999 <- fit_kick
alpha_kick <- extract(fit_kick)$alpha
beta_kick <- extract(fit_kick)$beta

# table(data$play_type == "pass", data$pass_attempt)


# These are qb spikes
# data %>% filter(pass_attempt == 1 & data$play_type != "pass") %>% View()
dat <- do.call(rbind, data_list)
pass <- dat %>%
    filter(play_type == "pass") %>%
    select(
        passer_player_name,
        complete_pass,
        passing_yards,
        touchdown,
        interception
    ) %>%
    mutate(passing_yards = ifelse(is.na(passing_yards), 0, passing_yards))

qbs <- pass %>%
    group_by(passer_player_name) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    head(3) %>%
    pull(passer_player_name)

source("./R/run_sim2025.R")
source("./R/drive_sim2025.R")

test <- map(qbs, function(x) {
    temp <- data %>% filter(passer_player_name == x)
    run_sim(temp, alpha_kick, beta_kick)
})

qbs[1:3]
test[[1]]$scores
test[[1]]$scores
test[[1]]$scores
