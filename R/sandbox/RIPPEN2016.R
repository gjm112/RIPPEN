# Import data and libraries
library(RIPPEN)
library(parallel)

# Set working directory
setwd(this.path::here())

nfl <- gatherData()
# Collect QB Data
passPlays <- gatherPassPlays(nfl)

qbList <- as.character(unique(passPlays$passer_player_name))

# Collect league kicker data
kickCoef <- kickCoef(nfl)

# Collect QB Data
qbbig <- names(sort(table(passPlays$passer_player_name)))[sort(table(passPlays$passer_player_name)) > 3000]

# Run simulations
qbResults <- mclapply(qbbig, runSim, nsim = 20000, season = 2009, mc.cores = 4)
names(qbResults) <- as.character(qbbig)
save(qbResults, file = "data/qbResults20000.rda")

# Calculate mean results
meanResults <- data.frame(qb = names(qbResults), mean = unlist(lapply(qbResults, mean)))
gameMeanResults <- data.frame(qb = names(meanResults), mean = (meanResults$mean * 11))

meanResults[order(meanResults$mean), ]
save(meanResults, file = "data/meanResults20000.rda")

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 500]
meanResultsSub <- meanResults[meanResults$qb %in% qbbig, ]
meanResultsSub[order(meanResultsSub$mean), ]
