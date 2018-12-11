 #' @title driveSim
#'
#' @description This function simulates a single drive for RIPPEN
#'
#' @details Fill in the details
#'
#' @param nu_0 prior degrees of freedom
#' @param kappa_0 prior something
#'
#' @return
#'
#' @export


driveSim <- function(qbdata, kickCoef, kappa_0, nu_0, z){
  driveState <- list()
  driveState$down <- 1
  driveState$togo <- 10
  driveState$togoTD <- 80 #between 100 and 0
  
  nCompleted <- sum(qbdata$PassOutcome=="Complete")
  nPasses <- length(qbdata$PassOutcome)
  alphaP <- 1
  betaP <- 1
  
  nIncomp <- nPasses - nCompleted
  nInt <- sum(qbdata$InterceptionThrown)
  alphaI <- 1
  betaI <- 1
  
  #Add a while loop to make sure that down is always less than 4.
  while(driveState$down < 4){
    #Returns 1 if complete and 0 if incomplete
    pComp <- rbeta(1, alphaP + nCompleted, betaP + nPasses-nCompleted)
    pass <- rbinom(1, 1, pComp)
    
    # If incomplete check for interception or add down
    if (pass == 0){
      # Was the pass intercepted, only sampling from incomplete passes
      pInt <- rbeta(1, alphaI + nInt, betaI + nIncomp - nInt )
      int <- rbinom(1, 1, pInt)
      # int <- sample(qbdata$InterceptionThrown[qbdata$PassOutcome=="Incomplete Pass"],1)
      if (int == 1){return(0);}
      driveState$down <- driveState$down + 1
    }
    # Else get results of completed pass
    else {
      #yards <- passSim(qbdata, kappa_0, nu_0)
      yards <- getYardsSim(z)
      #yards <- sample(qbdata$TotalYards[qbdata$PassOutcome=="Complete"],1)
      # Check for first down
      if (driveState$togo < yards){
        driveState$down <- 1
        driveState$togo <- 10
      } else {
        driveState$down <- driveState$down + 1
        driveState$togo <- driveState$togo - yards
      }
      # Subtract yards left to score and check for TD
      driveState$togoTD <- driveState$togoTD - yards
      if(driveState$togoTD <= 0){return (7);}
    }
  }
  #If down reaches 4 for field goal
  #17 yards is 10 for endzone and 7 for where the holder holds the ball.
  xb <- kickCoef%*%c(1,driveState$togoTD+17)
  pfg <- exp(xb)/(1+exp(xb))
  fieldGoal <- rbinom(1,1,pfg)
  if(fieldGoal == 1){return(3);}
  #Return 0 if play also does not result in a field goal
  return(0);
}