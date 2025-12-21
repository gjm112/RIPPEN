gatherPassPlays <- function(nfl) {
    # Collect QB Data
    pass_plays <- (subset(nfl,
        play_type == "pass",
        select = c(
            "passer_player_name", "air_yards", "yards_after_catch",
            "complete_pass", "incomplete_pass", "interception",
            "touchdown", "fumble", "game_date", "home_team", "away_team", "season"
        )
    ))

    pass_plays$passing_yards <- pass_plays$air_yards + pass_plays$yards_after_catch

    # Set negative yards to 0
    pass_plays$passing_yards[pass_plays$passing_yards < 0] <- 0

    pass_plays <- pass_plays[!is.na(pass_plays$passer_player_name), ]

    pass_plays$passer_player_name <- as.character(pass_plays$passer_player_name)
    # TODO: Filter out QB spikes?

    return(pass_plays)
}
