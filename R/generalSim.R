generalSim <- function(qbs, years){
  results <- list()
  for (q in qbs){
    print(q)
    results[[q]] <- list()
    print(results)
    
    for (s in years){
      print(s) #This is the input to the function.  
      qbdata <- subset(passPlays, passer_player_name == q & season ==  s)
      print(head(qbdata))
      qbdata[is.na(qbdata$TotalYards),]$TotalYards <- 0
      
      z <- yardsSim(qbdata)
      
      results[[q]][[as.character(s)]] <- unlist(mclapply(c(1:50000),sim))
    }
  }
  output <- data.frame(RIPPEN = 10 * unlist(lapply(results,function(x){lapply(x,mean)})), passer = rep(qbs, each = 1), year = years)
  return(output)
}