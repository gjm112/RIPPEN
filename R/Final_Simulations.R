#nfl <- nfl2018 <- read.csv("/Users/gregorymatthews/Dropbox/RIPPENgit/data/reg_pbp_2018.csv")
#library(RIPPEN)
library(invgamma)
library(parallel)
library(ggplot2)
library(plotly)
library(teamcolors)
##### PROCESS FOR TOTAL DATA GATHERING:
####Only needs to be run once:
#   nfl <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
#   nfl <- subset(nfl, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
#   nfl$season <- 2018
#   for(year in 2009:2017){print(year)
#     print(sprintf("Processing %i:", year))
#     temp_data <- read.csv(sprintf("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_%i.csv", year))
#     #data$touchback <- NULL # Older datasets are similar except for touchback (which we don't need!)
#     temp_data <- subset(temp_data, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
#     temp_data$season <- year
#     nfl <- rbind(nfl, temp_data)
#   }
#   rm(data)
#   write.csv(nfl, "./data/total09-18_pbp_data.csv", append = F, row.names = F)
# }
#   season records
# 1    2009   18921
# 2    2010   19226
# 3    2011   19507
# 4    2012   19766
# 5    2013   20294
# 6    2014   19966
# 7    2015   20411
# 8    2016   20275
# 9    2017   19599
# 10   2018   19842

#load the data
  nfl <- read.csv(file = "/Users/gregorymatthews/Dropbox/RIPPENgit/data/total09-18_pbp_data.csv")


##################################### Variable Changes #######################################
#"Passer" ----> "passer_player_name"
#"PassOutcome" ----> done away with -- replaced with complete_pass, incomplete_pass, interception indicators
#"AirYards" ----> "air_yards"
#"YardsAfterCatch"  ----> "yards_after_catch"
#"InterceptionThrown"  ----> "interception"
#"Fumble" ----> "fumble"
#"Date"  ----> "game_date"
#"HomeTeam" ----> "home_team"
#"AwayTeam"  ----> "away_team"
#"Season"  ----> done away with - we will just initialize this (as 'season') on our own.
#"Field Goal" ----> "field_goal"   ((This is variable for play_type))
#"FieldGoalDistance" ----> (nfl$field_goal_distance <- nfl$yardline_100 + 17) where play_type = "field_goal"
nfl$field_goal_distance <- nfl$yardline_100 + 17
#"FieldGoalResult" ----> "field_goal_result" -- either "made", "missed", or "blocked"
#"PlayType" ----> "playtype"
##################################### Variable Changes #######################################

# Collect league kicker data
kicker <- subset(nfl, 
                 play_type=="field_goal", 
                 select= c("field_goal_distance", "field_goal_result"))

kicker$Good <- (kicker$field_goal_result=="made") + 0 # Converts T/F to numbers

kicker <- kicker[!is.na(kicker$Good),]

boot <- glm(Good ~ field_goal_distance, data = kicker, family = "binomial")

# Coefficients for Field Goals
kickCoef <- boot$coefficients

# Collect QB Data
passPlays <- (subset(nfl,
                     play_type == "pass", 
                     select= c("passer_player_name","air_yards","yards_after_catch",
                               "complete_pass", "incomplete_pass","interception",
                               "touchdown","fumble", "game_date", "home_team", "away_team", "season")))

passPlays$TotalYards <- passPlays$air_yards + passPlays$yards_after_catch

# Set negative yards to 0 
passPlays$TotalYards[passPlays$TotalYards < 0] <- 0

passPlays <- passPlays[!is.na(passPlays$passer_player_name),]

#Some manual fixes
passPlays$passer_player_name[passPlays$passer_player_name == "Alex Smith"] <- "A.Smith"
passPlays$passer_player_name[passPlays$passer_player_name == "Ale.Smith"] <- "A.Smith"
passPlays$passer_player_name[passPlays$passer_player_name == "B.St. Pierre (3rd QB)"] <- "B.St. Pierre"
passPlays$passer_player_name[passPlays$passer_player_name == "C.Hanie (3rd QB)"] <- "C.Hanie"
passPlays$passer_player_name[passPlays$passer_player_name == "M.Stafford (3rd QB)"] <- "M.Stafford"
passPlays$passer_player_name[passPlays$passer_player_name == "T.Pike (3rd QB)"] <- "T.Pike"
passPlays$passer_player_name[passPlays$passer_player_name == "C.Shorts III"] <- "C.Shorts"
passPlays$passer_player_name[passPlays$passer_player_name == "Dom.Davis"] <- "D.Davis"
passPlays$passer_player_name[passPlays$passer_player_name == "Jo.Freeman"] <- "J.Freeman"
passPlays$passer_player_name[passPlays$passer_player_name == "Josh.Brown"] <- "J.Brown"
passPlays$passer_player_name[passPlays$passer_player_name == "Mat.Moore"] <- "M.Moore"
passPlays$passer_player_name[passPlays$passer_player_name == "Matt.Moore"] <- "M.Moore"
passPlays$passer_player_name[passPlays$passer_player_name == "R.Griffin"] <- "R.Griffin III"
passPlays$passer_player_name[passPlays$passer_player_name == "Sh.Hill"] <- "S.Hill"
passPlays$passer_player_name[passPlays$passer_player_name == "Shaun.Hill"] <- "S.Hill"

passPlays$passer_player_name <- as.character(passPlays$passer_player_name)

qbList <- as.character(unique(passPlays$passer_player_name))

qbbig <- names(sort(table(passPlays$passer_player_name)))
#qbbig <- names(sort(table(passPlays$passer_player_name)))[sort(table(passPlays$passer_player_name)) > 100]
qbbig <- sort(qbbig)

library(dplyr)
##fret <- subset(passPlays, season == 2018) %>% group_by(passer_player_name) %>% summarise(att = n()) 
#qbbig <- fret$passer_player_name[fret$att >=100]
#qbbig



sim <- function(i){
  out <- driveSim(qbdata, kickCoef, 1, 1, z = z) 
  return(out)
}

######################## This is where we are "crashing" #####################
# It is processing the first 18 quarterbacks and then on B.Hoyer:
# Error in update.jags(jags, 5000) : Error in node y[1]
# Failure to calculate log density 

####################### Investigation time               ####################
source("/Users/gregorymatthews/Dropbox/RIPPENgit/R/yardsSim.R")
source("/Users/gregorymatthews/Dropbox/RIPPENgit/R/driveSim.R")
  results <- list()
  for (q in qbbig){
    print(q)
    results[[q]] <- list()
    
    for (s in 2012){
      print(s) #This is the input to the function.  
      qbdata <- subset(passPlays, passer_player_name == q & season ==  s)
      
      if (nrow(qbdata)>0){
        z <- yardsSim(qbdata)
        #save(z, file = "/Users/gregorymatthews/Dropbox/")
        
        results[[as.character(s)]][[q]]<-unlist(mclapply(c(1:50000),sim))
        print(mean(results[[as.character(s)]][[q]]))
      }
    }
  }


save(results, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/results_season_2012.RData")
mn <- unlist(lapply(results[["2012"]],mean))
var <- unlist(lapply(results[["2012"]],var))
rippen <- data.frame(name = names(mn),rippen = mn,var = var)
#rippen <- subset(rippen,name %in% qbbig)
rippen2012_season <- rippen[order(-rippen$rippen),]
save(rippen2012_season, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2012_season_df.RData")


plot(rippen$rippen,rippen$sd)

subset(passPlays, passer_player_name == "C.Henne" & season ==  s)



dist <- data.frame(do.call(rbind,lapply(results[["2018"]],table)))
dist$rippen <- 7*dist$X7/50000+3*dist$X3/50000
dist$X0 <- dist$X0/50000 
dist$X3 <- dist$X3/50000 
dist$X7 <- dist$X7/50000 
dist$name <- row.names(dist)

gg <- ggplot(aes(x = X7, y = X3, label = name ,colour = rippen),data = dist) + geom_point() + geom_abline(slope = rep(-7/3,4), intercept = c(1/3,2/3,3/3,4/3)) + scale_colour_gradient(high = "#D44500",low = "darkblue")
ggplotly(gg, tooltip = c("label","colour"))


plot(dist$X7/50000, dist$X3/50000, ylim = c(0.11,0.21), col = rgb(rip/max(rip),0,1-rip/max(rip)),pch = 16, xlab = "Touchdown Probability",ylab = "Field Goal Probablity", main = "RIPPEN")
text(dist$X7[c(4,50)]/50000, dist$X3[c(4,50)]/50000,dist$name[c(4,50)])
abline(a = 4/3, b = -7/3)
abline(a = 3/3, b = -7/3)
abline(a = 2/3, b = -7/3)
abline(a = 1/3, b = -7/3)
abline(a = 0/3, b = -7/3)

abline(a = 4.5/3, b = -7/3)
abline(a = 3.5/3, b = -7/3)
abline(a = 2.5/3, b = -7/3)
abline(a = 1.5/3, b = -7/3)
abline(a = 0.5/3, b = -7/3)


dist








#############################################
#Best games of 2018 
#############################################

dates <- sort(unique(passPlays$game_date[passPlays$season == 2018]))

source("./R/yardsSim.R")
source("./R/driveSim.R")
feeling_it = F
if(feeling_it == T){
  results_game <- list()
  for (q in qbbig){
    print(q)
    results_game[[q]] <- list()
    for (s in 2018:2018){
      
      for (d in dates){
        print(q)
        print(d) #This is the input to the function.  
        qbdata <- subset(passPlays, passer_player_name == q & season ==  s & game_date == d)
        
        if (nrow(qbdata)>0){
          
          #RUSTY! NO!
          #if(is.na(qbdata$TotalYards)){
          #  qbdata$TotalYards <- 0
          #}
          #Setting all the NA's to 0?  Is that a good idea?  
          #qbdata$TotalYards[is.na(qbdata$TotalYards)] <- 0
          
          z <- yardsSim(qbdata)
          #save(z, file = "/Users/gregorymatthews/Dropbox/")
          
          results_game[[as.character(s)]][[q]][[d]]<-unlist(mclapply(c(1:50000),sim))
          print(mean(results_game[[as.character(s)]][[q]][[d]]))
        }
      }
    }
  }
}

save(results_game, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/results_season_games_2018.RData")


players <- as.list(names(results_game[["2018"]]))
names(players) <- players
getmean <- function(x) {lapply(results_game[["2018"]][[x]], mean)}

rippen2018_games <- data.frame(date = substring(row.names(rippen2018_games),nchar(row.names(rippen2018_games))-9,nchar(row.names(rippen2018_games))),
                               name = substring(row.names(rippen2018_games),1,nchar(row.names(rippen2018_games))-11),
                               rippen = unlist(lapply(players, getmean)))

save(rippen2018_games, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_games_df.RData")



load("/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_games_df.RData")
load("/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_season_df.RData")
library(ggplot)
library(teamcolors)
prim <- c(teamcolors$primary[teamcolors$name == "New Orleans Saints"],
          teamcolors$primary[teamcolors$name == "Kansas City Chiefs"],
          teamcolors$primary[teamcolors$name == "Los Angeles Rams"],
          teamcolors$primary[teamcolors$name == "New England Patriots"],
          teamcolors$primary[teamcolors$name == "Chicago Bears"],
          teamcolors$primary[teamcolors$name == "Buffalo Bills"])

sec <- c(teamcolors$secondary[teamcolors$name == "New Orleans Saints"],
         teamcolors$secondary[teamcolors$name == "Kansas City Chiefs"],
         teamcolors$secondary[teamcolors$name == "Los Angeles Rams"],
         teamcolors$secondary[teamcolors$name == "New England Patriots"],
         teamcolors$secondary[teamcolors$name == "Chicago Bears"],
         teamcolors$secondary[teamcolors$name == "Buffalo Bills"])
qbs <- c("D.Brees","P.Mahomes","J.Goff","T.Brady","M.Trubisky","J.Allen")
rippen2018_games$Date <- as.factor(rippen2018_games$date) 
gg <- ggplot(aes(x = factor(name,levels = qbs), label = Date, y = rippen ,col = factor(name,levels = qbs), fill = factor(name,levels = qbs)), data = subset(rippen2018_games,name %in% qbs ), xlab = "QB")  + scale_colour_manual("name", values = sec) + scale_fill_manual("name",values = prim) + geom_boxplot() +geom_point() + geom_point(aes(x = factor(name,levels = qbs),y = rippen),data = subset(rippen2018_season,name %in% qbs), col = "white", size = 1) + xlab("name")
ggint <- ggplotly(gg, tooltip =c("y","label"))
ggint

#http://www.nfl.com/player/ryanfitzpatrick/2506581/gamelogs
#http://www.nfl.com/player/mitchelltrubisky/2558008/gamelogs

dates <- sort(unique(passPlays$game_date[passPlays$season == 2018]))


#Now bootstrap games fot Brady
source("./R/yardsSim.R")
source("./R/driveSim.R")

results_game_boot <- list()
for (s in 2018:2018){
  results_game_boot[[as.character(s)]] <- list()
  for (q in "T.Brady"){
    print(q)
    results_game_boot[[as.character(s)]][[q]] <- list()
    for (d in dates){
      results_game_boot[[as.character(s)]][[q]][[d]] <- c()
      print(q)
      print(d) 
      
      #This is the input to the function.  
      qbdata <- subset(passPlays, passer_player_name == q & season ==  s & game_date == d)
      
      if (nrow(qbdata)>0){
        
        for (b in 1:30){
          qbdata <- subset(passPlays, passer_player_name == q & season ==  s & game_date == d)
          
          qbdata <- qbdata[sample(1:nrow(qbdata),replace= TRUE),]
          
          
          #RUSTY! NO!
          #if(is.na(qbdata$TotalYards)){
          #  qbdata$TotalYards <- 0
          #}
          #Setting all the NA's to 0?  Is that a good idea?  
          #qbdata$TotalYards[is.na(qbdata$TotalYards)] <- 0
          
          z <- yardsSim(qbdata)
          #save(z, file = "/Users/gregorymatthews/Dropbox/")
          
          results_game_boot[[as.character(s)]][[q]][[d]][b]<-mean(unlist(mclapply(c(1:50000),sim)))
          print(mean(results_game_boot[[as.character(s)]][[q]][[d]][b]))
        }
      }
    }
  }
}


save(results_game_boot, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_games_boot_Brady.RData")

sort(unlist(lapply(results_game_boot[[as.character(s)]][[q]], sd)))


load("/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_season_boot_Brady.RData")
load("/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_games_boot_Brady.RData")
fret <- do.call(c, results_game_boot[[as.character(s)]][[q]])
df_brady_boot <- data.frame(date = c(substring(names(fret),1,10),rep("Season",30)), rippen = c(fret,results_season_boot[[1]][[1]]))
ggplot(aes(x = date, y = rippen), data = df_brady_boot) + geom_boxplot(fill = teamcolors$primary[teamcolors$name=="New England Patriots"],col = teamcolors$secondary[teamcolors$name=="New England Patriots"]) + coord_flip() + geom_point(aes(x = date, y = rippen),data = subset(rippen2018_games, name == "T.Brady"), col = "white")+ geom_point(aes(y = 2.68782,x = 17), col = "white")

#Now bootstrap season for Brady
source("./R/yardsSim.R")
source("./R/driveSim.R")

results_season_boot <- list()
for (s in 2018:2018){
  results_season_boot[[as.character(s)]] <- list()
  for (q in "T.Brady"){
    print(q)
    results_season_boot[[as.character(s)]][[q]] <- c()
    
    
    print(q)
    print(d) 
    
    #This is the input to the function.  
    qbdata <- subset(passPlays, passer_player_name == q & season ==  s)
    
    if (nrow(qbdata)>0){
      
      for (b in 1:30){
        qbdata <- subset(passPlays, passer_player_name == q & season ==  s)
        
        qbdata <- qbdata[sample(1:nrow(qbdata),replace= TRUE),]
        
        
        #RUSTY! NO!
        #if(is.na(qbdata$TotalYards)){
        #  qbdata$TotalYards <- 0
        #}
        #Setting all the NA's to 0?  Is that a good idea?  
        #qbdata$TotalYards[is.na(qbdata$TotalYards)] <- 0
        
        z <- yardsSim(qbdata)
        #save(z, file = "/Users/gregorymatthews/Dropbox/")
        
        results_season_boot[[as.character(s)]][[q]][b]<-mean(unlist(mclapply(c(1:50000),sim)))
        print(mean(results_season_boot[[as.character(s)]][[q]][b]))
      }
    }
  }
}


save(results_season_boot, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_season_boot_Brady.RData")



mn <- unlist(lapply(results[["2018"]],mean))
sd <- unlist(lapply(results[["2018"]],sd))
rippen <- data.frame(name = names(mn),rippen = mn, sd = sd)
rippen <- subset(rippen,name %in% qbbig)
rippen2018_season <- rippen[order(-rippen$rippen),]
save(rippen2018_season, file = "/Users/gregorymatthews/Dropbox/RIPPENgit/RIPPEN_2018_season_df.RData")


plot(rippen$rippen,rippen$sd)

subset(passPlays, passer_player_name == "C.Henne" & season ==  s)





# We should make this a function -- then we can lapply or pass list of qbs/years:
GeneralSim <- function(qbs, years){
  results <- list()
  for (q in qbs){
    print(q)
    results[[q]] <- list()
    print(results)
    
    for (s in years){
      print(s) #This is the input to the function.  
      qbdata <- subset(passPlays, passer_player_name == q & season ==  s)
      print(head(qbdata))
      ### I don't think we want to set Incomplete to 0 (as that's a valid catch possibility)
      ### Even more, it is currently incorrectly carried out so let's fix that. (My fault)
      # Here is the old one:
      #if(is.na(qbdata$TotalYards)){
      #qbdata$TotalYards <- 0
      #}
      qbdata[is.na(qbdata$TotalYards),]$TotalYards <- 0
      
      z <- yardsSim(qbdata)
      
      #save(z, file = "/Users/gregorymatthews/Dropbox/")
      
      results[[q]][[as.character(s)]] <- unlist(mclapply(c(1:50000),sim))
    }
  }
  output <- data.frame(RIPPEN = 10 * unlist(lapply(results,function(x){lapply(x,mean)})), passer = rep(qbs, each = 1), year = years) #Why are we repeating 8 times?
  return(output)
  #return(results)
}
output <- GeneralSim(qbbig, unique(nfl$season))

output <- GeneralSim("T.Brady", unique(nfl$season)) # 2018: 8.9586 ... yikes
output <- GeneralSim(c("T.Brady", "D.Brees"), unique(nfl$season))

g <- ggplot(data = output, aes(x = year, y = RIPPEN, colour = passer)) + geom_point() #This doesn't make sense for just our 2018 data. Let's try it with big boy data.

greg <- ggplotly(g) #interactive plot

############# Testing out smaller, individual cases:
qb <- "T.Brady"
t <- "New England Patriots"

#qb <- "P.Manning"
#t <- "Denver Broncos"


plot(output$year[output$passer == qb], 
     output$RIPPEN[output$passer == qb], 
     type = "l", col = teamcolors$primary[teamcolors$name==t], 
     lwd = 6, xlab = "Season", ylab = "RIPPEN", ylim = c(15,35))

points(output$year[output$passer == qb], 
       output$RIPPEN[output$passer == qb], type = "l",
       col = teamcolors$secondary[teamcolors$name==t], lwd = 6, lty = 3)

points(output$year[output$passer == qb], 
       output$RIPPEN[output$passer == qb],
       col = teamcolors$tertiary[teamcolors$name==t], pch = 16, cex = 3)
