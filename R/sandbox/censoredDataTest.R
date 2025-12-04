library(RIPPEN)
data(nfl)

passPlays <- subset(nfl,select= c("Passer","PassOutcome","AirYards","YardsAfterCatch","InterceptionThrown","Fumble","Touchdown","Season"),PassOutcome == "Complete" & Passer == "T.Brady" & Season == 2015) 
passPlays$TotalYards <- passPlays$AirYards + passPlays$YardsAfterCatch
passPlays$TotalYards[passPlays$TotalYards<0] <- 0


#Likelihood
nottds <- log(passPlays$TotalYards[passPlays$Touchdown == 0] + 1)
tds <- log(passPlays$TotalYards[passPlays$Touchdown == 1] + 1)

dat <- passPlays[,c("TotalYards","Touchdown")]
dat$y <- log(passPlays$TotalYards+1)

ll <- function(tds,nottds,prior){
  function(x){(sum(dnorm(nottds,x[1],x[2],log = TRUE)) + 
                 sum(pnorm(tds,x[1],x[2],log = TRUE, lower.tail = FALSE))) + 
      log(prior(x))}
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




dat <- passPlays[,c("TotalYards","Touchdown")]
dat$y <- log(passPlays$TotalYards+1)

y <- dat$y
y[dat$Touchdown == 1] <- NA

cens_y <- dat$y
isCensored <- dat$Touchdown


model.str = "
model {
for ( i in 1:n ) {
isCensored[i] ~ dinterval( y[i] , cens_y[i] )
y[i] ~ dnorm( mu , tau ) 
}
tau <- 1/pow(sigma,2)
sigma ~ dunif(0,100)
mu ~ dnorm(0,1E-6)
}"


setwd("/Users/gregorymatthews/Dropbox/RIPPENgit/")
write(model.str,"rippen.bug")
library(rjags)

n.chains<-1
n.adapt<-100
jags <- jags.model('/Users/gregorymatthews/Dropbox/RIPPENgit/rippen.bug',
                  data = list('y' = y, 'isCensored' = isCensored,  'n' = n, 'cens_y' = cens_y), 
                   n.chains = 1, n.adapt = 100)

update(jags, 5000)
#dic.pD <- dic.samples(jags, 20, type = "pD") # Deviance Information Criterion
#dic.popt <- dic.samples(jags, 100, type = "popt") # Penalized expected deviance
z <- jags.samples(jags, c("mu","tau"), 20000, thin = 10)


save(z, file = "/Users/gregorymatthews/Dropbox/")




model.str<-"model { 
for (i in 1:n) {
y[i,1:2] ~ dmnorm(mu[i,1:2], tau[,])
mu[i,1] <-  alpha1 + inprod(theta[s[i],w[i],],x1[i,])
mu[i,2] <-  alpha2 + inprod(theta[s[i],w[i],],x2[i,])
}




for (j in 1:(2*nTeams)) {
theta_star[1,1,j] ~ dnorm(0, tauSeason)
theta[1,1,j] <- theta_star[1,1,j] - mean(theta_star[1,1,])
}


for (www in 2:nWeeks) {  
for (j in 1:(2*nTeams)) {
theta_star[1,www,j] ~ dnorm(gammaWeek*theta[1,www-1,j], tauWeek)
theta[1,www,j] <- theta_star[1,www,j] - mean(theta_star[1,www,])
}
}

for (sss in 2:(nSeas - 1)) {
for (j in 1:(2*nTeams)) {
theta_star[sss,1,j] ~ dnorm(gammaSeason*theta[sss-1,nWeeks,j], tauSeason)
theta[sss,1,j] <- theta_star[sss,1,j] - mean(theta_star[sss,1,])
}

for (www in 2:nWeeks) {  
for (j in 1:(2*nTeams)) {
theta_star[sss,www,j] ~ dnorm(gammaWeek*theta[sss,www-1,j], tauWeek)
theta[sss,www,j] <- theta_star[sss,www,j] - mean(theta_star[sss,www,])
}
}
}

for (sss in nSeas:nSeas) {
for (j in 1:(2*nTeams)) {
theta_star[sss,1,j] ~ dnorm(gammaSeason*theta[sss-1,nWeeks,j], tauSeason)
theta[sss,1,j] <- theta_star[sss,1,j] - mean(theta_star[sss,1,])
}

for (www in 2:nWeeks) {  
for (j in 1:(2*nTeams)) {
theta_star[sss,www,j] ~ dnorm(gammaWeek*theta[sss,www-1,j], tauWeek)
theta[sss,www,j] <- theta_star[sss,www,j] - mean(theta_star[sss,www,])
}
}
}

alpha1 ~ dunif(-100,100)
alpha2 ~ dunif(-100,100)

tau[1,2] <- -6
tau[2,1] <- tau[1,2]

tau[1,1] <-100 
tau[2,2] <-100
tauGame ~ dunif(0,100)
tauWeek ~ dunif(0,100)
tauSeason ~ dunif(0,100)
tauAlpha ~ dunif(0, 100)
gammaWeek ~ dunif(0,1.5)
gammaSeason ~ dunif(0,1)

}"

setwd("/Users/gregorymatthews/Dropbox/SPORTS/Football/NFL2018/")
write(model.str,"nfl2018.bug")
library(rjags)

library(rjags)
n.chains<-1
n.adapt<-100
jags <- jags.model('/Users/gregorymatthews/Dropbox/SPORTS/Football/NFL2018/nfl2018.bug',
                   data = list('y' = y,'x1' = x1, 'x2'=x2, 's' = s, 'w' = w, 'n' = n, 
                               'nTeams' = nTeams, 
                               'nWeeks' = nWeeks, 'nSeas' = nSeas), 
                   n.chains = 1, n.adapt = 100)

update(jags, 5000)
#dic.pD <- dic.samples(jags, 20, type = "pD") # Deviance Information Criterion
#dic.popt <- dic.samples(jags, 100, type = "popt") # Penalized expected deviance
z <- jags.samples(jags, c("theta","gammaWeek","gammaSeason","alpha1","alpha2","tau"), 20000, thin = 10)
save(z, file = "/Users/gregorymatthews/Dropbox/SPORTS/Football/NFL2018/z20180912.RData")
