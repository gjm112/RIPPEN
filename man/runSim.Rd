\name{runSim}
\alias{runSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
runSim(passer, kappa_0 = 1, nu_0 = 3, nsim = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{passer}{
%%     ~~Describe \code{passer} here~~
}
  \item{kappa_0}{
%%     ~~Describe \code{kappa_0} here~~
}
  \item{nu_0}{
%%     ~~Describe \code{nu_0} here~~
}
  \item{nsim}{
%%     ~~Describe \code{nsim} here~~
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
function (passer, kappa_0 = 1, nu_0 = 3, nsim = 100) 
{
    print(passer)
    qbdata <- subset(passPlays, Passer == passer)
    results <- replicate(nsim, driveSim(qbdata, kickCoef, kappa_0, 
        nu_0))
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
