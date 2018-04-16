runSim <-
function(passer, kappa_0=1, nu_0=3, nsim = 100){
  print(passer)
  qbdata <- subset(passPlays, Passer==passer)
  results <- replicate(nsim,driveSim(qbdata,kickCoef, kappa_0, nu_0))
  
  return(results)
}
