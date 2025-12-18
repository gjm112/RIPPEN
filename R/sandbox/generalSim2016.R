generalSim <- function(qbs, years) {
    results <- list()

    for (q in qbs) {
        print(q)
        results[[q]] <- list()

        for (s in years) {
            print(paste("Season:", as.character(s)))
            qbdata <- subset(passPlays, passer_player_name == q & season == s)
            print(paste("Number of plays in season:", nrow(qbdata)))
            if (nrow(qbdata) > 0) {
                z <- yardsSim(qbdata)
                sim <- function(i) {
                    out <- driveSim(qbdata, kickCoef, 1, 1, z = z)
                    return(out)
                }

                # results[[as.character(s)]][[q]] <- unlist(mclapply(c(1:50000), sim))
                results[[q]][[as.character(s)]] <- unlist(mclapply(c(1:50000), sim))
                # print the mean of the results
                print(paste("Mean of results:", mean(results[[q]][[as.character(s)]])))
                print(paste("RIPPEN:", 10 * mean(results[[q]][[as.character(s)]])))
            }
        }
        # Remove the results for the QB if there are no results
        if (length(results[[q]]) == 0) {
            results[[q]] <- NULL
        }
    }
    print("=== RESULTS STRUCTURE ===")
    print(str(results))
    print(paste("Number of QBs:", length(results)))
    for (q in names(results)) {
        print(paste("QB:", q, "- Seasons:", paste(names(results[[q]]), collapse = ", ")))
    }
    # output <- data.frame(RIPPEN = 10 * unlist(lapply(results, function(x) {
    #     lapply(x, mean)
    # })), passer = rep(qbs, each = 1), year = years)
    output_list <- list()
    for (q in names(results)) {
        for (s in names(results[[q]])) {
            if (length(results[[q]][[s]]) > 0) {
                output_list[[length(output_list) + 1]] <- data.frame(
                    RIPPEN = 10 * mean(results[[q]][[s]]),
                    passer = q,
                    year = as.numeric(s)
                )
            }
        }
    }
    output <- do.call(rbind, output_list)
    return(output)
}
