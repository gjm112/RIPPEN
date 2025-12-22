#' @title runSim
#'
#' @description This function simulates a number of drives for a given passer
#'
#' @details Fill in the details
#'
#' @param nu_0 prior degrees of freedom
#' @param kappa_0 prior something
#' @param date date of game YYYY-MM-DD
#' @param season season 20XX (2009-2016)
#'
#' @return
#'
#' @export

runSim <-
function(passer, kappa_0=1, nu_0=3, nsim = 100, date=NULL, season=NULL){
  # NOTE: We should setup an error check in case Date/Season does not exist
  print(passer)
    if(!is.null(date)){
      print(date)
    # modify Passplays to contain only results from date
      gamePasses <- subset(passPlays, Date==date)
      qbdata <- subset(gamePasses, Passer==passer)
      
    } 
    else if(!is.null(season)){
      print(season)
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
