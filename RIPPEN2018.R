nfl<-read.csv("~/RIPPEN/NFL-Play-by-Play-2009-16.csv")
#nfl<-read.csv("~/Dropbox/RIPPEN/NFL-Play-by-Play-2009-16.csv")
library(invgamma)

###################################################
#Create a pass simulator
###################################################
passSim <- function(qbdata, kappa_0, nu_0){

	# 1. League Yardage / Completed Passes
	#Yards for every complete catch
	x <- passPlays$TotalYards[passPlays$PassOutcome=="Complete"]
	# 		y <- log(x + 1)
	#Transfromation to achieve approximate normality
	y <- log(x+1)
	# Prior data? (? significance of the vinculum over y)
	mu_0 <-  mean(y)
	# Weight of prior
	#kappa_0 <- 1
	# Degree of freedom (Why 3?)
	#nu_0 <- 3
	# Scale Parameter
	sigma2_0 <- var(y)

	# 2.	Vn <- Vo + nj
	# 		S2n <- Vo*S2o + (nj-1)S2j + [(Ko*nj)/(Ko+nj)](Yj - Mo)^2
	# 		Mn <- (Ko/Ko+nj)*Mo + (nj/Ko+nj)*(Yj)
	# 		Kn <- Ko + nj
	n_j <- length(qbdata$TotalYards[qbdata$PassOutcome=="Complete"])
	ybar_j <- mean(log(qbdata$TotalYards[qbdata$PassOutcome=="Complete"] + 1))
	if (n_j > 1){
	  S2_j <- var(log(qbdata$TotalYards[qbdata$PassOutcome=="Complete"] + 1))
	} else {
	  S2_j <- 0
	}
	nu_n <- nu_0 + n_j
	 
	sigma2_n <- (1/nu_n)*(nu_0*sigma2_0 + (n_j-1)*S2_j + (kappa_0*n_j)/(kappa_0+n_j)*(ybar_j - mu_0)^2)

	
	#kappa_n <- kappa_0 + n_j

	# 3.	Draw S2i
	sigma2_star <- rinvgamma(1, shape = nu_n/2, rate = (nu_n/2)*sigma2_n)
	#rinvchisq(1, nu_n, sigma2_n)
	# 4.	Draw Mi
	# 		Mi <- S2i * Yj * N(Mn, S2i/Kn)
	mu_n <- ((kappa_0/sigma2_star)*mu_0 + (n_j/sigma2_star)*ybar_j) / (kappa_0/sigma2_star + n_j/sigma2_star )
	
	mu_star <- rnorm(1, mu_n, sqrt(1 / (kappa_0/sigma2_star + n_j/sigma2_star)))

	# 5.	Draw y
	y_tilde <- rnorm(1, mu_star, sqrt(sigma2_star))

	# 6.	yards <- exp(Yi - 1)
	yards <- exp(y_tilde) - 1
	return(yards)
}

###################################################
#Create a drive simulator
###################################################

drivesim <- function(qbdata, kickCoef, kappa_0, nu_0){
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
		  #TODO - Implement Bayes model
			yards <- passSim(qbdata, kappa_0, nu_0)
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

#Run drive simulations for a given passer
#Add year and game.  
runSim <- function(passer, kappa_0=1, nu_0=3, nsim = 100){
  print(passer)
  qbdata <- subset(passPlays, Passer==passer)
  results <- replicate(nsim,drivesim(qbdata,kickCoef, kappa_0, nu_0))

  return(results)
}

#Collect QB Data
passPlays <- subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble"))
passPlays$TotalYards<- passPlays$AirYards + passPlays$YardsAfterCatch
passPlays$TotalYards[passPlays$TotalYards<0] <- 0
passPlays <- passPlays[!is.na(passPlays$Passer),]

qbList <- as.character(unique(passPlays$Passer)[1:10])

#Collect league kicker data
kicker <- subset(nfl, PlayType=="Field Goal", select= c("FieldGoalDistance", "FieldGoalResult"))
kicker$Good <- (kicker$FieldGoalResult=="Good") + 0
kicker <- kicker[!is.na(kicker$Good),]
boot <- glm(Good ~ FieldGoalDistance, data = kicker, family = "binomial")
kickCoef <- boot$coefficients

qbResults <- lapply(qbList, runSim)
names(qbResults) <- as.character(qbList)

meanResults <- data.frame(mean=unlist(lapply(qbResults, mean)))
