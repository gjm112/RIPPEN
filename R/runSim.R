#' @title runSim
#'
#' @description This function simulates a number of drives for a given passer
#'
#' @details Fill in the details
#'
#' @param nu_0 prior degrees of freedom
#' @param kappa_0 prior something
#'
#' @return
#'
#' @export

runSim <-
function(passer, kappa_0=1, nu_0=3, nsim = 100, date=NULL, season=NULL){
  print(passer)
  
    if(!is.null(date)){
    # modify Passplays to contain only results from date
      gamePasses <- subset(passPlays, Date==date)
      qbdata <- subset(gamePasses, Passer==passer)
      
    } 
    else if(!is.null(season)){
    # modify Passplays to contain only results from season
      seasonPasses <- subset(passPlays, Season==season)
      qbdata <- subset(seasonPasses, Passer==passer)
      
    }
    else{
      qbdata <- subset(passPlays, Passer==passer)
      
    }
  results <- replicate(nsim,driveSim(qbdata,kickCoef, kappa_0, nu_0))
  return(results)
}
