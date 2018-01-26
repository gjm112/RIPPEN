

nfl<-read.csv("~/RIPPEN/NFL-Play-by-Play-2009-16.csv")

temp <- subset(nfl,Passer=="T.Brady",select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))


###################################################
#Create a drive simulator
###################################################
qbdata<-temp
drivesim <- function(qbdata){
	driveState <- list()
	driveState$down <- 1
	driveState$togo <- 10
	driveState$togoTD <- 80 #between 100 and 0

	#Add a while loop to make sure that down is always less than 4.
	while(driveState$down < 4){
	  #Returns 1 if complete and 0 if incomplete
	  pass <- (sample(qbdata$PassOutcome,1)=="Complete")+0
		# If incomplete check for interception or add down
	  if (pass == 0){
			# Was the pass intercepted, only sampling from incomplete passes
			int <- sample(qbdata$InterceptionThrown[qbdata$PassOutcome=="Incomplete Pass"],1)
			if (int == 1){return(0)}
	    driveState$down <- driveState$down + 1
	  }
		# Else get results of completed pass
		else {
	    yards <- sample(qbdata$AirYards[qbdata$PassOutcome=="Complete"],1)
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
			if(driveState$togoTD <= 0){ return (7) }
    }
	}
	#If down is 4 check to see if within field goal range
	if(driveState$togoTD < 30){
		# TODO: More accurate way to calculate field goal for greater range
		return(3)
	}
	else{return(0)}
}
drivesim(qbdata)
