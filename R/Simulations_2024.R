# nfl <- nfl2018 <- read.csv("/Users/gregorymatthews/Dropbox/RIPPENgit/data/reg_pbp_2018.csv")
# library(RIPPEN)
library(invgamma)
library(parallel)
library(ggplot2)
library(plotly)
library(teamcolors)
library(dplyr)
source("./R/gatherData.R")
source("./R/gatherPassPlays.R")
source("./R/kickCoef.R")
source("./R/yardsSim.R")
source("./R/driveSim.R")
source("./R/generalSim.R")

# load the data
nfl <- gatherData()

# Coefficients for Field Goals
kickCoef <- kickCoef(nfl)

# Collect QB Data
passPlays <- gatherPassPlays(nfl)

qbList <- as.character(unique(passPlays$passer_player_name))

qbbig <- names(sort(table(passPlays$passer_player_name)))
qbbig <- qbbig[sort(table(passPlays$passer_player_name)) >= 400]
qbbig <- sort(qbbig)


sim <- function(i) {
    out <- driveSim(qbdata, kickCoef, 1, 1, z = z)
    return(out)
}

##### Investigation #####
results <- list()

output <- generalSim(qbbig, unique(nfl$season))
