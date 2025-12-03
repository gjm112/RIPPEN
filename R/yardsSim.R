#' @title yardsSim
#'
#' @description This function estimates the distribution of yards on a completed pass
#'
#' @details Fill in the details
#'
#' @param
#' @param
#' @param
#' @param
#'
#' @return
#'
#' @export

library(rjags) # This can just be done in RIPPEN?
yardsSim <- function(qbdata) {
    dat <- subset(qbdata, complete_pass == 1, select = c("TotalYards", "touchdown"))
    # Remove records with missing yards
    dat <- dat[!is.na(dat$TotalYards), ]
    # dat <- qbdata[,c("TotalYards","touchdown")]
    dat$y <- log(dat$TotalYards + 1)

    y <- dat$y
    y[dat$touchdown == 1] <- NA

    cens_y <- dat$y
    isCensored <- dat$touchdown

    n <- nrow(dat)

    model.str <- "
  model {
  for ( i in 1:n ) {
  isCensored[i] ~ dinterval(y[i] , cens_y[i])
  y[i] ~ dnorm(mu , tau)
  }
  tau <- 1/pow(sigma,2)
  sigma ~ dunif(0,100)
  mu ~ dnorm(0,1E-6)
  }"


    # setwd("/Users/gregorymatthews/Dropbox/RIPPENgit/")
    # write(model.str,"rippen.bug")


    n.chains <- 1
    n.adapt <- 100
    jags <- jags.model(textConnection(model.str),
        data = list("y" = y, "isCensored" = isCensored, "n" = n, "cens_y" = cens_y),
        n.chains = 1, n.adapt = 100
    )

    update(jags, 5000)
    # dic.pD <- dic.samples(jags, 20, type = "pD") # Deviance Information Criterion
    # dic.popt <- dic.samples(jags, 100, type = "popt") # Penalized expected deviance
    z <- jags.samples(jags, c("mu", "tau"), 20000, thin = 10)
    return(z)
}


### We are using this
getYardsSim <- function(z) {
    i_star <- sample(1:2000, 1)
    mu_star <- z$mu[, i_star, 1]
    sigma2_star <- 1 / z$tau[, i_star, 1]

    x <- rnorm(1, mu_star, sqrt(sigma2_star))
    y <- exp(x) - 1
    return(y)
}
