#nfl<-read.csv("~/RIPPEN/NFL-Play-by-Play-2009-16.csv")
#nfl<-read.csv("~/Dropbox/RIPPEN/NFL-Play-by-Play-2009-16.csv")

library(RIPPEN)
data(nfl)

#Collect QB Data
passPlays <- subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))
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

qbResults <- lapply(qbList, runSim)
names(qbResults) <- as.character(qbList)

meanResults <- data.frame(qb = names(qbResults), mean=unlist(lapply(qbResults, mean)))
meanResults[order(meanResults$mean),]
#save(meanResults, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/data/meanResults.rda")

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 500]
meanResultsSub <- meanResults[meanResults$qb %in% qbbig,]
meanResultsSub[order(meanResultsSub$mean),]