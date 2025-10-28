# Import data and libraries
library(RIPPEN)
library(parallel)
data(nfl)

# Set working directory
setwd(this.path::here())

#Collect QB Data
passPlays <- na.omit(subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble", "Date", "HomeTeam", "AwayTeam", "Season")))
passPlays$TotalYards<- passPlays$AirYards + passPlays$YardsAfterCatch

# Set negative yards to 0
passPlays$TotalYards[passPlays$TotalYards<0] <- 0
passPlays <- passPlays[!is.na(passPlays$Passer),]

qbList <- as.character(unique(passPlays$Passer))

#Collect league kicker data
kicker <- subset(nfl, PlayType=="Field Goal", select= c("FieldGoalDistance", "FieldGoalResult"))
kicker$Good <- (kicker$FieldGoalResult=="Good") + 0
kicker <- kicker[!is.na(kicker$Good),]
boot <- glm(Good ~ FieldGoalDistance, data = kicker, family = "binomial")
kickCoef <- boot$coefficients

# Collect QB Data
qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 3000]

# Run simulations
qbResults <- mclapply(qbbig, runSim, nsim = 20000, season=2009, mc.cores = 4)
names(qbResults) <- as.character(qbbig)
save(qbResults, file = "data/qbResults20000.rda")

# Calculate mean results
meanResults <- data.frame(qb = names(qbResults), mean=unlist(lapply(qbResults, mean)))
gameMeanResults <- data.frame(qb = names(meanResults), mean=(meanResults$mean * 11))

meanResults[order(meanResults$mean),]
save(meanResults, file = "data/meanResults20000.rda")

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 500]
meanResultsSub <- meanResults[meanResults$qb %in% qbbig,]
meanResultsSub[order(meanResultsSub$mean),]