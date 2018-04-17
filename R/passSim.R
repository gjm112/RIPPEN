#' @title passSim
#' 
#' @description What does this do?  
#' 
#' @details Fill in the details
#' 
#' @param nu_0 prior degrees of freedom
#' @param kappa_0 prior something
#' 
#' @return 
#' 
#' @export


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
