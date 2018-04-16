source("~/Dropbox/RIPPENgit/driveSim.R")
source("~/Dropbox/RIPPENgit/passSim.R")
nfl<-read.csv("~/RIPPEN/NFL-Play-by-Play-2009-16.csv")
#nfl<-read.csv("~/Dropbox/RIPPEN/NFL-Play-by-Play-2009-16.csv")
library(invgamma)


#Run drive simulations for a given passer
#Add year and game.  
runSim <- function(passer, kappa_0=1, nu_0=3, nsim = 100){
  print(passer)
  qbdata <- subset(passPlays, Passer==passer)
  results <- replicate(nsim,driveSim(qbdata,kickCoef, kappa_0, nu_0))
  
  return(results)
}


#Collect QB Data
passPlays <- subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))
passPlays$TotalYards<- passPlays$AirYards + passPlays$YardsAfterCatch
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

meanResults <- data.frame(mean=unlist(lapply(qbResults, mean)))
