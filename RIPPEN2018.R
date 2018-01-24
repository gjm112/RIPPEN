nfl<-read.csv("~/Dropbox/RIPPEN/NFL by Play 2009-2016 (v2).csv")

temp <- subset(nfl,Passer=="J.Cutler",select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))



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
		#Was the pass intercepted
		int <- sample(qbdata$InterceptionThrown,1)
		if (int == 1){return(0)}
		if (int == 0){
		  #Returns 1 if complete and 0 if incomplete
		  comp <- (sample(qbdata$PassOutcome,1)=="Complete")+0
			# If incomplete add down
		  if (comp == 0){ driveState$down <- driveState$down + 1 }
			# Else get results of completed pass
			else {
		    yards <- sample(qbdata$AirYards[qbdata$PassOutcome=="Complete"],1)
		    if (driveState$togo-yards>0){
		      driveState$down <- driveState$down + 1
		      driveState$togo <- driveState$togo - yards
		    } else
		    {
		      driveState$down <- 1
		      driveState$togo <- 10
		    }

				# Subtract yards left to score and check for TD
		    driveState$togoTD <- driveState$togoTD - yards
				if(driveState$togoTD !> 0){ return (7) }
	    }
		}
	}
	#If down is 4 check to see if within field goal range
	# TODO: More accurate way to calculate field goal for greater range
	if(driveState$togoTD < 20){return(3)}
	else{return(0)}
}
