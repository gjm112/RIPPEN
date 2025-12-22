gatherPassPlays <- function(nfl) {
    # Collect QB Data
    pass_plays <- nfl %>%
        filter(play_type == "pass") %>%
        select(
            passer_player_name,
            complete_pass,
            passing_yards,
            touchdown,
            interception,
            game_id,
            season
        ) %>%
        mutate(passing_yards = ifelse(is.na(passing_yards), 0, passing_yards))

    pass_plays <- pass_plays[!is.na(pass_plays$passer_player_name), ]

    pass_plays$passer_player_name <- as.character(pass_plays$passer_player_name)
    # TODO: Filter out QB spikes?

    return(pass_plays)
}
