generalSim <- function(qb_list, pass_plays, num_sims) {
    print("Starting simulations")
    results <- list()
    total_qbs <- length(qb_list)
    current_qb <- 1
    for (qb in qb_list) {
        print(paste("Running simulations for", qb, "(", current_qb, "of", total_qbs, ")"))
        qbdata <- pass_plays[pass_plays$passer_player_name == qb, ]
        results[[qb]] <- runSim(qbdata, kicker, num_sims)
        print(paste("Finished running simulations for", qb))
        current_qb <- current_qb + 1
    }
    print("Finished running simulations")
    return(results)
}
