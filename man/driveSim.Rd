\name{driveSim}
\alias{driveSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
driveSim(qbdata, kickCoef, kappa_0, nu_0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{qbdata}{
%%     ~~Describe \code{qbdata} here~~
}
  \item{kickCoef}{
%%     ~~Describe \code{kickCoef} here~~
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
function (qbdata, kickCoef, kappa_0, nu_0) 
{
    driveState <- list()
    driveState$down <- 1
    driveState$togo <- 10
    driveState$togoTD <- 80
    nCompleted <- sum(qbdata$PassOutcome == "Complete")
    nPasses <- length(qbdata$PassOutcome)
    alphaP <- 1
    betaP <- 1
    nIncomp <- nPasses - nCompleted
    nInt <- sum(qbdata$InterceptionThrown)
    alphaI <- 1
    betaI <- 1
    while (driveState$down < 4) {
        pComp <- rbeta(1, alphaP + nCompleted, betaP + nPasses - 
            nCompleted)
        pass <- rbinom(1, 1, pComp)
        if (pass == 0) {
            pInt <- rbeta(1, alphaI + nInt, betaI + nIncomp - 
                nInt)
            int <- rbinom(1, 1, pInt)
            if (int == 1) {
                return(0)
            }
            driveState$down <- driveState$down + 1
        }
        else {
            yards <- passSim(qbdata, kappa_0, nu_0)
            if (driveState$togo < yards) {
                driveState$down <- 1
                driveState$togo <- 10
            }
            else {
                driveState$down <- driveState$down + 1
                driveState$togo <- driveState$togo - yards
            }
            driveState$togoTD <- driveState$togoTD - yards
            if (driveState$togoTD <= 0) {
                return(7)
            }
        }
    }
    xb <- kickCoef \%*\% c(1, driveState$togoTD + 17)
    pfg <- exp(xb)/(1 + exp(xb))
    fieldGoal <- rbinom(1, 1, pfg)
    if (fieldGoal == 1) {
        return(3)
    }
    return(0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
