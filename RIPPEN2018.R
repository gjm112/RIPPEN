nfl<-read.csv("~/RIPPEN/NFL-Play-by-Play-2009-16.csv")

###################################################
#Create a drive simulator
###################################################
drivesim <- function(qbdata, kickCoef){
	driveState <- list()
	driveState$down <- 1
	driveState$togo <- 10
	driveState$togoTD <- 80 #between 100 and 0
  
	nCompleted <- sum(qbdata$PassOutcome=="Completed")
	nPasses <- length(qbdata$passOutcome)
	alphaP <- 1
	betaP <- 1
	
	nIncomp <- nPasses - nCompleted
	nInt <- sum(qbdata$InterceptionThrown)
	alphaI <- 1
	betaI <- 1
	
	qbdata$TotalYards <- qbdata$AirYards + qbdata$YardsAfterCatch

	#Add a while loop to make sure that down is always less than 4.
	while(driveState$down < 4){
	  #Returns 1 if complete and 0 if incomplete
	  pComp <- rbeta(1, alphaP + nCompleted, betaP + nPasses-nCompleted )
	  pass <- rbinom(1, 1, pComp)
	  #pass <- (sample(qbdata$PassOutcome,1)=="Complete")+0
	  
		# If incomplete check for interception or add down
	  if (pass == 0){
			# Was the pass intercepted, only sampling from incomplete passes
	    pInt <- rebeta(1, alphaI + nInt, betaI + nIncomp-nInt )
	    int <- rbinom(1, 1, pInt)
			# int <- sample(qbdata$InterceptionThrown[qbdata$PassOutcome=="Incomplete Pass"],1)
			if (int == 1){return(0);}
	    driveState$down <- driveState$down + 1
	  }
		# Else get results of completed pass
		else {
		  #TODO - Implement Bayes model
		  yards <- sample(qbdata$TotalYards[qbdata$PassOutcome=="Complete"],1)
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

#Run drive simulations for a given passer
runSim <- function(passer){
  qbdata <- subset(passPlays, Passer==passer)
  #TODO Remove QBs with 0 incomplete passes and 0 complete passes
  if(!any(qbdata$PassOutcome=="Complete") | !any(qbdata$PassOutcome=="Incomplete")){
    print(qbdata$Passer[1])
    return(0)
  }
  results <- replicate(100,drivesim(qbdata,kickCoef))
  return(results)
}

#Collect QB Data
passPlays <- subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))
passPlays <- passPlays[!is.na(passPlays$Passer),]
qbList <- unique(passPlays$Passer)

#Collect league kicker data
kicker <- subset(nfl, PlayType=="Field Goal", select= c("FieldGoalDistance", "FieldGoalResult"))
kicker$Good <- (kicker$FieldGoalResult=="Good") + 0
kicker <- kicker[!is.na(kicker$Good),]
boot <- glm(Good ~ FieldGoalDistance, data = kicker, family = "binomial")
kickCoef <- boot$coefficients

qbResults <- lapply(qbList, runSim)
names(qbResults) <- as.character(qbList)

meanResults <- data.frame(mean=unlist(lapply(qbResults, mean)))
