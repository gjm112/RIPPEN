nfl <- nfl2018 <- read.csv("/Users/gregorymatthews/Dropbox/RIPPENgit/data/reg_pbp_2018.csv")


#nfl<-read.csv("~/Dropbox/RIPPEN/NFL-Play-by-Play-2009-16.csv")
#data(nfl)

nfl$field_goal_distance <- nfl$yardline_100 + 17
#Collect league kicker data
kicker <- subset(nfl, play_type=="field_goal", select= c("field_goal_distance", "field_goal_result"))
kicker$Good <- (kicker$field_goal_result=="made") + 0
kicker <- kicker[!is.na(kicker$Good),]
boot <- glm(Good ~ field_goal_distance, data = kicker, family = "binomial")
#Coefficients for Field Goals
kickCoef <- boot$coefficients

#Collect QB Data
passPlays <- na.omit(subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble", "Date", "HomeTeam", "AwayTeam", "Season","Touchdown")))
passPlays$TotalYards<- passPlays$AirYards + passPlays$YardsAfterCatch

# Set negative yards to 0
passPlays$TotalYards[passPlays$TotalYards<0] <- 0
passPlays <- passPlays[!is.na(passPlays$Passer),]

qbList <- as.character(unique(passPlays$Passer))

qbbig <- names(sort(table(passPlays$Passer)))[sort(table(passPlays$Passer)) > 3000]


sim <- function(i){
  out <- driveSim(qbdata, kickCoef, 1, 1, z = z)
  return(out)
}

library(parallel)
results <- list()
for (q in qbbig){print(q)
  results[[q]] <- list()
  for (s in 2009:2016){print(s)
#This is the input to the function.  
qbdata <- subset(passPlays, Passer == q & Season ==  s)

z <- yardsSim(qbdata)
#save(z, file = "/Users/gregorymatthews/Dropbox/")

results[[q]][[as.character(s)]]<-unlist(mclapply(c(1:50000),sim))
    }
  }


output <- data.frame(RIPPEN= 10*unlist(lapply(results,function(x){lapply(x,mean)})), passer = rep(qbbig, each = 8), year = rep(2009:2016,17))

library(ggplot2)
library(plotly)
g <- ggplot(data = output, aes(x = year, y = RIPPEN, colour = passer)) + geom_line()
greg <- ggplotly(g)

library(teamcolors)
qb <- "T.Brady"
t <- "New England Patriots"
plot(output$year[output$passer == qb], output$RIPPEN[output$passer == qb], type = "l",col = teamcolors$primary[teamcolors$name==t], lwd = 6, xlab = "Season", ylab = "RIPPEN", ylim = c(15,35))
points(output$year[output$passer == qb], output$RIPPEN[output$passer == qb], type = "l",col = teamcolors$secondary[teamcolors$name==t], lwd = 6, lty = 3)
points(output$year[output$passer == qb], output$RIPPEN[output$passer == qb],col = teamcolors$tertiary[teamcolors$name==t], pch = 16, cex = 3)

qb <- "P.Manning"
t <- "Denver Broncos"
points(output$year[output$passer == qb], output$RIPPEN[output$passer == qb], type = "l",col = teamcolors$primary[teamcolors$name==t], lwd = 6)
points(output$year[output$passer == qb], output$RIPPEN[output$passer == qb], type = "l",col = teamcolors$secondary[teamcolors$name==t], lwd = 6, lty = 3)
points(output$year[output$passer == qb], output$RIPPEN[output$passer == qb],col = teamcolors$tertiary[teamcolors$name==t], pch = 16, cex = 3)




