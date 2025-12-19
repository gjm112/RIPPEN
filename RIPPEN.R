setwd(this.path::here())
library(rstan)

source("./R/gatherData.R")
source("./R/gatherPassPlays.R")
source("./R/kickerSim.R")
source("./R/driveSim.R")
source("./R/runSim.R")

# Collect play by play data
nfl <- gatherData()

# Filter Pass Plays
pass_plays <- gatherPassPlays(nfl)

# Create Kicker
kicker <- createKicker(nfl)

# Get QB list
# qbs <- as.character(unique(pass_plays$passer_player_name))
qbbig <- names(sort(table(pass_plays$passer_player_name)))[sort(table(pass_plays$passer_player_name)) > 1000]
