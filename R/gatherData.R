# install.packages("nflreadr")
library(nflreadr)

# Gather data from 2009-2018 seasons
gatherData <- function(){
	data_file_path <- "./data/total09-24_pbp_data.csv"
	first_time <- !file.exists(data_file_path)
	nfl <- data.frame()
	if(first_time == TRUE){
		# Gather data from scratch
		print("Data file not found. Gathering data from scratch...")
		print("Gathering data from 2009-2024...")
		for(year in 2009:2024){print(year)
			print(sprintf("Processing %i:", year))
			temp_data <- load_pbp(year)
			#data$touchback <- NULL # Older datasets are similar except for touchback (which we don't need!)
			temp_data <- subset(temp_data, play_type == "pass" | play_type == "field_goal", select = c("passer_player_name","air_yards","yards_after_catch","complete_pass", "incomplete_pass","interception","touchdown","fumble", "game_date", "home_team", "away_team","yardline_100", "field_goal_result", "play_type"))
			temp_data$season <- year
			nfl <- rbind(nfl, temp_data)
		}
		nfl$field_goal_distance <- nfl$yardline_100 + 17
		write.csv(nfl, data_file_path, append = FALSE, row.names = FALSE)
		print(sprintf("Data gathered and saved to %s", data_file_path))
	} else {
		# Load existing data
		nfl <- read.csv(file = data_file_path)
		print("Data file found and loaded.")
	}
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