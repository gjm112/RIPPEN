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
for (qb in qbs) {
    print(qb)
    qbdata <- subset(pass_plays, passer_player_name == qb)
    career_results[[qb]] <- runSim(qbdata, kicker, nsim = 1000000)
    print(paste0("Finished running simulations for ", qb))
}
save(career_results, file = "./data/career_results.rda")
print("Saved career results")
