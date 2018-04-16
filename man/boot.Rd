\name{boot}
\alias{boot}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("boot")}
\format{
  The format is:
List of 30
 $ coefficients     : Named num [1:2] 5.818 -0.103
  ..- attr(*, "names")= chr [1:2] "(Intercept)" "FieldGoalDistance"
 $ residuals        : Named num [1:7898] -8.55 -14.97 1.3 1.08 1.09 ...
  ..- attr(*, "names")= chr [1:7898] "21" "66" "132" "147" ...
 $ fitted.values    : Named num [1:7898] 0.883 0.933 0.769 0.927 0.919 ...
  ..- attr(*, "names")= chr [1:7898] "21" "66" "132" "147" ...
 $ effects          : Named num [1:7898] -41.307 -27.281 0.577 0.346 0.36 ...
  ..- attr(*, "names")= chr [1:7898] "(Intercept)" "FieldGoalDistance" "" "" ...
 $ R                : num [1:2, 1:2] -30.7 0 -1338.5 265.9
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "(Intercept)" "FieldGoalDistance"
  .. ..$ : chr [1:2] "(Intercept)" "FieldGoalDistance"
 $ rank             : int 2
 $ qr               :List of 5
  ..$ qr   : num [1:7898, 1:2] -30.70633 0.00813 0.01373 0.0085 0.00887 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:7898] "21" "66" "132" "147" ...
  .. .. ..$ : chr [1:2] "(Intercept)" "FieldGoalDistance"
  ..$ rank : int 2
  ..$ qraux: num [1:2] 1.01 1.01
  ..$ pivot: int [1:2] 1 2
  ..$ tol  : num 1e-11
  ..- attr(*, "class")= chr "qr"
 $ family           :List of 12
  ..$ family    : chr "binomial"
  ..$ link      : chr "logit"
  ..$ linkfun   :function (mu)  
  ..$ linkinv   :function (eta)  
  ..$ variance  :function (mu)  
  ..$ dev.resids:function (y, mu, wt)  
  ..$ aic       :function (y, n, mu, wt, dev)  
  ..$ mu.eta    :function (eta)  
  ..$ initialize:  expression({  if (NCOL(y) == 1) {  if (is.factor(y))  y <- y != levels(y)[1L]  n <- rep.int(1, nobs)  y[weights =| __truncated__
  ..$ validmu   :function (mu)  
  ..$ valideta  :function (eta)  
  ..$ simulate  :function (object, nsim)  
  ..- attr(*, "class")= chr "family"
 $ linear.predictors: Named num [1:7898] 2.02 2.64 1.2 2.53 2.43 ...
  ..- attr(*, "names")= chr [1:7898] "21" "66" "132" "147" ...
 $ deviance         : num 6067
 $ aic              : num 6071
 $ null.deviance    : num 7015
 $ iter             : int 5
 $ weights          : Named num [1:7898] 0.1033 0.0623 0.1778 0.0681 0.0743 ...
  ..- attr(*, "names")= chr [1:7898] "21" "66" "132" "147" ...
 $ prior.weights    : Named num [1:7898] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:7898] "21" "66" "132" "147" ...
 $ df.residual      : int 7896
 $ df.null          : int 7897
 $ y                : Named num [1:7898] 0 0 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:7898] "21" "66" "132" "147" ...
 $ converged        : logi TRUE
 $ boundary         : logi FALSE
 $ model            :'data.frame':	7898 obs. of  2 variables:
  ..$ Good             : num [1:7898] 0 0 1 1 1 1 1 1 1 1 ...
  ..$ FieldGoalDistance: int [1:7898] 37 31 45 32 33 37 21 20 37 47 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula'  language Good ~ FieldGoalDistance
  .. .. ..- attr(*, "variables")= language list(Good, FieldGoalDistance)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "Good" "FieldGoalDistance"
  .. .. .. .. ..$ : chr "FieldGoalDistance"
  .. .. ..- attr(*, "term.labels")= chr "FieldGoalDistance"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(Good, FieldGoalDistance)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "Good" "FieldGoalDistance"
 $ call             : language glm(formula = Good ~ FieldGoalDistance, family = "binomial",      data = kicker)
 $ formula          :Class 'formula'  language Good ~ FieldGoalDistance
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
 $ terms            :Classes 'terms', 'formula'  language Good ~ FieldGoalDistance
  .. ..- attr(*, "variables")= language list(Good, FieldGoalDistance)
  .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "Good" "FieldGoalDistance"
  .. .. .. ..$ : chr "FieldGoalDistance"
  .. ..- attr(*, "term.labels")= chr "FieldGoalDistance"
  .. ..- attr(*, "order")= int 1
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(Good, FieldGoalDistance)
  .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. ..- attr(*, "names")= chr [1:2] "Good" "FieldGoalDistance"
 $ data             :'data.frame':	7898 obs. of  3 variables:
  ..$ FieldGoalDistance: int [1:7898] 37 31 45 32 33 37 21 20 37 47 ...
  ..$ FieldGoalResult  : Factor w/ 3 levels "Blocked","Good",..: 3 1 2 2 2 2 2 2 2 2 ...
  ..$ Good             : num [1:7898] 0 0 1 1 1 1 1 1 1 1 ...
 $ offset           : NULL
 $ control          :List of 3
  ..$ epsilon: num 1e-08
  ..$ maxit  : num 25
  ..$ trace  : logi FALSE
 $ method           : chr "glm.fit"
 $ contrasts        : NULL
 $ xlevels          : Named list()
 - attr(*, "class")= chr [1:2] "glm" "lm"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(boot)
## maybe str(boot) ; plot(boot) ...
}
\keyword{datasets}
