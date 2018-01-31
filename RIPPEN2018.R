nfl<-read.csv("~/Dropbox/RIPPEN/NFL by Play 2009-2016 (v2).csv")

temp <- subset(nfl,Passer=="J.Cutler",select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))
kicker <- subset(nfl, PlayType=="Field Goal", select= c("FieldGoalDistance", "FieldGoalResult"))
kicker$Good <- (kicker$FieldGoalResult=="Good") + 0
kicker <- kicker[!is.na(kicker$Good),]

boot <- glm(Good ~ FieldGoalDistance, data = kicker, family = "binomial")
kickCoef <- boot$coefficients


###################################################
#Create a drive simulator
###################################################
qbdata<-temp
drivesim <- function(qbdata, kickCoef){
	driveState <- list()
	driveState$down <- 1
	driveState$togo <- 10
	driveState$togoTD <- 80 #between 100 and 0

	qbdata$TotalYards <- qbdata$AirYards + qbdata$YardsAfterCatch
	
	#Add a while loop to make sure that down is always less than 4.
	while(driveState$down < 4){
	  #Returns 1 if complete and 0 if incomplete
	  pass <- (sample(qbdata$PassOutcome,1)=="Complete")+0
		# If incomplete check for interception or add down
	  if (pass == 0){
			# Was the pass intercepted, only sampling from incomplete passes
			int <- sample(qbdata$InterceptionThrown[qbdata$PassOutcome=="Incomplete Pass"],1)
			if (int == 1){return(0);}
	    driveState$down <- driveState$down + 1
	  }
		# Else get results of completed pass
		else {
		  yards <- sample(qbdata$TotalYards[qbdata$PassOutcome=="Complete"],1)
			# Check for first down
	    if (driveState$togo-yards <= 0){
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
drivesim(qbdata,kickCoef)

test2<-replicate(1000,drivesim(qbdata,kickCoef))
