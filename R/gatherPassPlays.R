gatherPassPlays <- function(nfl) {
    # Collect QB Data
    passPlays <- (subset(nfl,
        play_type == "pass",
        select = c(
            "passer_player_name", "air_yards", "yards_after_catch",
            "complete_pass", "incomplete_pass", "interception",
            "touchdown", "fumble", "game_date", "home_team", "away_team", "season"
        )
    ))

    passPlays$TotalYards <- passPlays$air_yards + passPlays$yards_after_catch

    # Set negative yards to 0
    passPlays$TotalYards[passPlays$TotalYards < 0] <- 0

    passPlays <- passPlays[!is.na(passPlays$passer_player_name), ]

    # Some manual fixes
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
    # TODO: Filter out QB spikes?

    return(passPlays)
}
