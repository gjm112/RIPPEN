\name{passSim}
\alias{passSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
passSim(qbdata, kappa_0, nu_0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qbdata}{
%%     ~~Describe \code{qbdata} here~~
}
  \item{kappa_0}{
%%     ~~Describe \code{kappa_0} here~~
}
  \item{nu_0}{
%%     ~~Describe \code{nu_0} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (qbdata, kappa_0, nu_0) 
{
    x <- passPlays$TotalYards[passPlays$PassOutcome == "Complete"]
    y <- log(x + 1)
    mu_0 <- mean(y)
    sigma2_0 <- var(y)
    n_j <- length(qbdata$TotalYards[qbdata$PassOutcome == "Complete"])
    ybar_j <- mean(log(qbdata$TotalYards[qbdata$PassOutcome == 
        "Complete"] + 1))
    if (n_j > 1) {
        S2_j <- var(log(qbdata$TotalYards[qbdata$PassOutcome == 
            "Complete"] + 1))
    }
    else {
        S2_j <- 0
    }
    nu_n <- nu_0 + n_j
    sigma2_n <- (1/nu_n) * (nu_0 * sigma2_0 + (n_j - 1) * S2_j + 
        (kappa_0 * n_j)/(kappa_0 + n_j) * (ybar_j - mu_0)^2)
    sigma2_star <- rinvgamma(1, shape = nu_n/2, rate = (nu_n/2) * 
        sigma2_n)
    mu_n <- ((kappa_0/sigma2_star) * mu_0 + (n_j/sigma2_star) * 
        ybar_j)/(kappa_0/sigma2_star + n_j/sigma2_star)
    mu_star <- rnorm(1, mu_n, sqrt(1/(kappa_0/sigma2_star + n_j/sigma2_star)))
    y_tilde <- rnorm(1, mu_star, sqrt(sigma2_star))
    yards <- exp(y_tilde) - 1
    return(yards)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
