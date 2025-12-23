generalSim <- function(qb_list, pass_plays, num_sims, cores = 3) {
    results <- mclapply(
        qb_list,
        function(qb) {
            print(qb)
            qbdata <- pass_plays[pass_plays$passer_player_name == qb, ]
            if (nrow(qbdata) == 0) {
                warning(paste("No data for QB:", qb))
                return(NULL)
            }
            tryCatch(
                {
                    result <- runSim(qbdata, kicker, num_sims)
                    return(list(qb = qb, result = result))
                },
                error = function(e) {
                    warning(paste("Error processing QB", qb, ":", e$message))
                    return(list(qb = qb, result = NULL, error = e$message))
                }
            )
        },
        mc.cores = cores
    )

    results_list <- list()

    for (item in results) {
        if (!is.null(item) && is.list(item) && "qb" %in% names(item) && "result" %in% names(item)) {
            if (!is.null(item$result)) {
                results_list[[item$qb]] <- item$result
            }
        } else {
            warning(paste("Unexpected item structure:", class(item), "- skipping"))
        }
    }
    return(results_list)
}
