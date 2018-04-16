library(RIPPEN)
data(nfl)

passPlays <- subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble","Touchdown","Season"),PassOutcome == "Complete" & Passer == "T.Brady" & Season == 2015) 
passPlays$TotalYards <- passPlays$AirYards + passPlays$YardsAfterCatch
passPlays$TotalYards[passPlays$TotalYards<0] <- 0


#Likelihood
nottds <- log(passPlays$TotalYards[passPlays$Touchdown == 0] + 1)
tds <- log(passPlays$TotalYards[passPlays$Touchdown == 1] + 1)



ll <- function(tds,nottds,prior){
  function(x){(sum(dnorm(nottds,x[1],x[2],log = TRUE)) + sum(pnorm(tds,x[1],x[2],log = TRUE, lower.tail = FALSE))) + log(prior(x))}
}

prior <- function(x){
  a <- x[1]
  1/x[2]
}

log.post <- ll(tds,nottds,prior)
log.post(c(0,1))


muVec <- seq(1,3,0.1)
sigmaVec <- seq(0.1,0.3,0.01)
z <- matrix(NA, nrow = length(muVec),ncol= length(sigmaVec))
for (mu in 1:length(muVec)){
  for (sigma in 1:length(sigmaVec)){
    z[mu,sigma] <- (log.post(c(muVec[mu],sigmaVec[sigma])))
  }  
}
contour(muVec,sigmaVec,z)


optim(par = c(0,1),ll)


passPlays[1,]

