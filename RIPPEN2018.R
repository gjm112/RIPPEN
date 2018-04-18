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

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 2000]

qbResults <- lapply(qbbig, runSim, nsim = 1000)
names(qbResults) <- as.character(qbList)

res <- data.frame(do.call(rbind,lapply(qbResults,table)), qb = names(qbResults))
res <- res[res$qb %in% qbbig,]
plot(res$X7/100, res$X3/100, pch=16, col= "white")
text(res$X7/100, res$X3/100, res$qb)
for (i in seq(0,5,0.25)){
curve((i-7*x)/3,0,100, add = TRUE,col="red")
}

meanResults <- data.frame(qb = names(qbResults), mean=unlist(lapply(qbResults, mean)))
meanResults[order(meanResults$mean),]
#save(meanResults, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/data/meanResults.rda")

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 500]
meanResultsSub <- meanResults[meanResults$qb %in% qbbig,]
meanResultsSub[order(meanResultsSub$mean),]
