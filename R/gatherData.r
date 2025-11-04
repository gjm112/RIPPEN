# Gather data from 2009-2018 seasons
gatherData <- function(){
  nfl <- read.csv("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
  nfl <- subset(nfl, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
  nfl$season <- 2018
  for(year in 2009:2017){print(year)
    print(sprintf("Processing %i:", year))
    temp_data <- read.csv(sprintf("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_%i.csv", year))
    #data$touchback <- NULL # Older datasets are similar except for touchback (which we don't need!)
    temp_data <- subset(temp_data, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
    temp_data$season <- year
    nfl <- rbind(nfl, temp_data)
  }
  nfl$field_goal_distance <- nfl$yardline_100 + 17
  return(nfl)
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
#"FieldGoalResult" ----> "field_goal_result" -- either "made", "missed", or "blocked"
#"PlayType" ----> "playtype"
##################################### Variable Changes #######################################