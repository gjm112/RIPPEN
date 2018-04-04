#Make this a function
#Arguments : qbata, pass plays??
#Prior parameters: kappa_0, nu_0, etc.  (Set some default values)

# 1. League Yardage / Completed Passes
#Legue average yardage on completed passed
x <- passPlays$TotalYards[passPlays$PassOutcome=="Complete"]
# 		y <- log(x + 1)
#Transfromation to achieve approximate normality
y <- log(x+1)
# Prior data? (? significance of the vinculum over y)
mu_0 <-  mean(y)
# Weight of prior
kappa_0 <- 1
# Degree of freedom (Why 3?)
nu_0 <- 3
# Scale Parameter
sigma2_0 <- var(y)
#
# # Need values of nj & Yj
# 2.	Vn <- Vo + nj
# 		S2n <- Vo*S2o + (nj-1)S2j + [(Ko*nj)/(Ko+nj)](Yj - Mo)^2
# 		Mn <- (Ko/Ko+nj)*Mo + (nj/Ko+nj)*(Yj)
# 		Kn <- Ko + nj
n_j <- length(qbData$TotalYards[passPlays$PassOutcome=="Complete"])
ybar_j <- mean(log(qbData$TotalYards[passPlays$PassOutcome=="Complete"] + 1))
S2_j <- var(log(qbData$TotalYards[passPlays$PassOutcome=="Complete"] + 1))
nu_n <- nu_0 + n_j 
sigma2_n <- (1/nu_n)*(nu_0*sigma2_0 + (n_j-1)*S2_j + (kappa_0*n_j)/(kappa_0+n_j)*(ybar_j - mu_0)^2)

mu_n <- kappa_0/(kappa_0+n_j)*mu_0 + n_j/(kappa_0 + n_j)*ybar_j
kappa_n <- kappa_0 + n_j

#
# # Confused on the equations written for this section
# 3.	Draw S2i
library(invgamma)
sigma2star <- rinvchisq(1, nu_n, sigma2_n)
#
# 4.	Draw Mi
# 		Mi <- S2i * Yj * N(Mn, S2i/Kn)
#
mustar <- rnorm(1, mu_n, sqrt(sigma2star/kappa_n))

# 5.	Draw Yi
# 		
#
Ytilde <- rnorm(1, mustar, sqrt(sigma2star))

# 6.	yards <- exp(Yi - 1)
y <- exp(Ytilde) - 1

