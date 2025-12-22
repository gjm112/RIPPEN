setwd(this.path::here())
library(rstan)
library(dplyr)
library(parallel)

source("./R/gatherData.R")
source("./R/gatherPassPlays.R")
source("./R/kickerSim.R")
source("./R/driveSim.R")
source("./R/runSim.R")

# Collect play by play data
nfl <- gatherData()
print("Collected play by play data")

# Filter Pass Plays
pass_plays <- gatherPassPlays(nfl)
print("Filtered pass plays")
# Create Kicker
kicker <- createKicker(nfl)
print("Created kicker")
# Get QB list
qbs <- as.character(unique(pass_plays$passer_player_name))
qbs <- sort(qbs)
print("Got QB list")
# Run simulations
career_results <- list()
print("Starting simulations")
num_sims <- 1000000 # Number of drives to simulate

print("Gathering career results")
for (qb in qbs) {
    print(qb)
    qbdata <- subset(pass_plays, passer_player_name == qb)
    career_results[[qb]] <- runSim(qbdata, kicker, nsim = num_sims)
    print(paste0("Finished running simulations for ", qb))
}
save(career_results, file = "./data/career_results.rda")
print("Saved career results")

print("Gathering season results")
seasons <- unique(pass_plays$season)
season_results <- list()
for (s in seasons) {
    print(s)
    season_data <- subset(pass_plays, season == s)
    season_qbs <- as.character(unique(season_data$passer_player_name))
    season_qbs <- sort(season_qbs)
    season_results[[s]] <- list()
    for (qb in season_qbs) {
        print(qb)
        qbdata <- subset(season_data, passer_player_name == qb)
        season_results[[season]][[qb]] <- runSim(qbdata, kicker, nsim = num_sims)
        print(paste0("Finished running simulations for ", qb))
    }
    print(paste0("Finished running simulations for season ", s))
}

save(season_results, file = "./data/season_results.rda")
print("Saved season results")

print("Gathering game results")
games <- unique(pass_plays$game_id)
game_results <- list()
for (game in games) {
    print(game)
    game_data <- subset(pass_plays, game_id == game)
    game_qbs <- as.character(unique(game_data$passer_player_name))
    game_qbs <- sort(game_qbs)
    game_results[[game]] <- list()
    for (qb in game_qbs) {
        print(qb)
        qbdata <- subset(game_data, passer_player_name == qb)
        game_results[[game]][[qb]] <- runSim(qbdata, kicker, nsim = num_sims)
        print(paste0("Finished running simulations for ", qb))
    }
    print(paste0("Finished running simulations for game ", game))
}
save(game_results, file = "./data/game_results.rda")
print("Saved game results")
