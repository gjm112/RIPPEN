#nfl<-read.csv("~/RIPPEN/NFL-Play-by-Play-2009-16.csv")
#nfl<-read.csv("~/Dropbox/RIPPEN/NFL-Play-by-Play-2009-16.csv")

library(RIPPEN)
library(parallel)
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

#qbResults <- lapply(qbbig, runSim, nsim = 2000)
qbResults <- mclapply(qbbig, runSim, nsim = 20000, mc.cores = 4)
names(qbResults) <- as.character(qbbig)
save(qbResults, file = "/Users/groot/RIPPEN/data/qbResults20000.rda")

# res <- data.frame(do.call(rbind,lapply(qbResults,table)), qb = names(qbResults))
# res <- res[res$qb %in% qbbig,]
# png("~/Dropbox/RIPPEN/Poster.png",res = 300, units = "in", h = 10, w = 12)
# #library(teamcolors)
# plot(res$X7/2000, res$X3/2000, pch=16, xlim = c(0.17, 0.27), ylim = c(0.16, 0.22),xlab = "TD Probability", ylab = "FG probability")
# for (i in seq(0,5,0.1)){
#   curve((i-7*x)/3,0,1, add = TRUE,col="grey", lty = 3)
# }
# pts <- curve((2-7*x)/3,0,1, add = TRUE,col="black",lwd=2)
# text(pts$x[21],pts$x[22], "RIPPEN = 2", srt = -70)
# text(res$X7[!res$qb%in%c("B.Roethlisberger","A.Dalton","E.Manning","P.Manning","T.Romo")]/2000, res$X3[!res$qb%in%c("B.Roethlisberger","A.Dalton","E.Manning","P.Manning","T.Romo")]/2000, res$qb[!res$qb%in%c("B.Roethlisberger","A.Dalton","E.Manning","P.Manning","T.Romo")],pos=1)
# text(res$X7[res$qb%in%c("B.Roethlisberger","A.Dalton","E.Manning")]/2000, res$X3[res$qb%in%c("B.Roethlisberger","A.Dalton","E.Manning")]/2000, res$qb[res$qb%in%c("B.Roethlisberger","A.Dalton","E.Manning")],pos=3)
# text(res$X7[res$qb%in%c("P.Manning")]/2000, res$X3[res$qb%in%c("P.Manning")]/2000, res$qb[res$qb%in%c("P.Manning")],pos=4)
# text(res$X7[res$qb%in%c("T.Romo")]/2000, res$X3[res$qb%in%c("T.Romo")]/2000, res$qb[res$qb%in%c("T.Romo")],pos=3)
# dev.off()


meanResults <- data.frame(qb = names(qbResults), mean=unlist(lapply(qbResults, mean)))
meanResults[order(meanResults$mean),]
save(meanResults, file = "/Users/groot/RIPPEN/data/meanResults20000.rda")

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 500]
meanResultsSub <- meanResults[meanResults$qb %in% qbbig,]
meanResultsSub[order(meanResultsSub$mean),]
