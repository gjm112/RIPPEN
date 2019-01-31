#nfl <- nfl2018 <- read.csv("/Users/gregorymatthews/Dropbox/RIPPENgit/data/reg_pbp_2018.csv")
#library(RIPPEN)
library(invgamma)
library(parallel)
library(ggplot2)
library(plotly)
library(teamcolors)

#nfl2018 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")

#nfl2017 <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2017.csv")

#nfl2018$season <- 2018
#nfl2017$season <- 2017
#nfl <- rbind(nfl2017, nfl2018)

##### PROCESS FOR TOTAL DATA GATHERING:


first_time <- FALSE
if(first_time == TRUE){
  nfl <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
  nfl <- subset(nfl, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
  nfl$season <- 2018
  for(year in 2009:2017){
    print(sprintf("Processing %i:", year))
    temp_data <- read.csv(sprintf("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_%i.csv", year))
    #data$touchback <- NULL # Older datasets are similar except for touchback (which we don't need!)
    temp_data <- subset(temp_data, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
    temp_data$season <- year
    nfl <- rbind(nfl, temp_data)
  }
  rm(data)
  write.csv(nfl, "total09-18_pbp_data.csv", append = F, row.names = F)
}
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

if(first_time == FALSE){
  nfl <- read.csv(file = "total09-18_pbp_data.csv", row.names = FALSE)
}

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

qbList <- as.character(unique(passPlays$passer_player_name))

qbbig <- names(sort(table(passPlays$passer_player_name)))#[sort(table(passPlays$passer_player_name)) > 100]


sim <- function(i){
  out <- driveSim(qbdata, kickCoef, 1, 1, z = z) 
  return(out)
}

######################## This is where we are "crashing" #####################
# It is processing the first 18 quarterbacks and then on B.Hoyer:
  # Error in update.jags(jags, 5000) : Error in node y[1]
  # Failure to calculate log density 

####################### Investigation time               ####################
feeling_it = F
if(feeling_it == T){
  results <- list()
  for (q in qbbig){
      print(q)
      results[[q]] <- list()
      
      for (s in 2018:2018){
        print(s) #This is the input to the function.  
        qbdata <- subset(passPlays, passer_player_name == q & season ==  s)
  
        if(is.na(qbdata$TotalYards)){
          qbdata$TotalYards <- 0
        }
  
        z <- yardsSim(qbdata)
        #save(z, file = "/Users/gregorymatthews/Dropbox/")
  
        results[[q]][[as.character(s)]]<-unlist(mclapply(c(1:50000),sim))
      }
  }
}

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
