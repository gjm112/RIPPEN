\name{passPlays}
\alias{passPlays}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("passPlays")}
\format{
  A data frame with 149089 observations on the following 7 variables.
  \describe{
    \item{\code{Passer}}{a factor with levels \code{A.Andrews} \code{A.Boldin} \code{A.Brown} \code{A.Dalton} \code{A.Davis} \code{A.Edwards} \code{A.Feeley} \code{A.Foster} \code{A.Lee} \code{A.Luck} \code{A.McCarron} \code{A.Morrison} \code{A.Podlesh} \code{A.Randle El} \code{A.Rodgers} \code{A.Rolle} \code{A.Sanders} \code{A.Smith} \code{A.Tanney} \code{Ale.Smith} \code{ards. Backward} \code{B.Anger} \code{B.Banks} \code{B.Berrian} \code{B.Bortles} \code{B.Brohm} \code{B.Colquitt} \code{B.Croyle} \code{B.Daniels} \code{B.Edwards} \code{B.Favre} \code{B.Fields} \code{B.Gabbert} \code{B.Gradkowski} \code{B.Hoyer} \code{B.Hundley} \code{B.Kern} \code{B.LaFell} \code{B.Leftwich} \code{B.Lloyd} \code{B.Marshall} \code{B.Maynard} \code{B.Moorman} \code{B.Osweiler} \code{B.Petty} \code{B.Powell} \code{B.Quinn} \code{B.Rainey} \code{B.Roethlisberger} \code{B.Scott} \code{B.Smith} \code{B.Volek} \code{B.Walters} \code{B.Weeden} \code{B.Westbrook} \code{B.Wing} \code{C.Batch} \code{C.Beasley} \code{C.Brown} \code{C.Cook} \code{C.Daniel} \code{C.Frye} \code{C.Hanie} \code{C.Hanson} \code{C.Henne} \code{C.Henry} \code{C.Hogan} \code{C.Johnson} \code{C.Jones} \code{C.Kaepernick} \code{C.Keenum} \code{C.Kessler} \code{C.McCoy} \code{C.Meredith} \code{C.Newton} \code{C.Ochocinco} \code{C.Painter} \code{C.Palmer} \code{C.Pennington} \code{C.Ponder} \code{C.Portis} \code{C.Rachal} \code{C.Redman} \code{C.Santos} \code{C.Shaw} \code{C.Shorts} \code{C.Simms} \code{C.Wentz} \code{C.Whitehurst} \code{D.Akers} \code{D.Alexander} \code{D.Amendola} \code{D.Anderson} \code{D.Baldwin} \code{D.Brees} \code{D.Bryant} \code{D.Carr} \code{D.Colquitt} \code{D.Culpepper} \code{D.Dixon} \code{D.Driver} \code{D.Fales} \code{D.Garrard} \code{D.Johnson} \code{D.Johnson Jr} \code{D.McFadden} \code{D.McNabb} \code{D.Murray} \code{D.Orlovsky} \code{D.Poe} \code{D.Prescott} \code{D.Robinson} \code{D.Sepulveda} \code{D.Stanton} \code{D.Thomas} \code{Dom.Davis} \code{DOWN. Backward} \code{E.Decker} \code{E.Graham} \code{E.Manning} \code{E.Manuel} \code{E.Sanders} \code{E.Weddle} \code{EMPT. Alex Smith} \code{F.Jackson} \code{FETY. Backward} \code{G.Harrell} \code{G.McElroy} \code{G.Smith} \code{G.Tate} \code{H.Smith} \code{haun.Hill} \code{iver. Ryan} \code{J.Addai} \code{J.Beck} \code{J.Best} \code{J.Brissett} \code{J.Bush} \code{J.Campbell} \code{J.Clausen} \code{J.Cribbs} \code{J.Crowder} \code{J.Cutler} \code{J.Delhomme} \code{J.Feagles} \code{J.Flacco} \code{J.Freeman} \code{J.Garoppolo} \code{J.Goff} \code{J.Gordon} \code{J.Grimes} \code{J.Hekker} \code{J.Higgins} \code{J.Huff} \code{J.Johnson} \code{J.Kearse} \code{J.Kerley} \code{J.Kitna} \code{J.Landry} \code{J.Locker} \code{J.Losman} \code{J.Maclin} \code{J.Manziel} \code{J.McCown} \code{J.McKinnon} \code{J.Morgan} \code{J.Norwood} \code{J.Palmer} \code{J.Russell} \code{J.Ryan} \code{J.Skelton} \code{J.Tucker} \code{J.Tuel} \code{J.Webb} \code{J.Winston} \code{Jo.Freeman} \code{Josh.Brown} \code{K.Boller} \code{K.Clemens} \code{K.Collins} \code{K.Cousins} \code{K.Hogan} \code{K.Kolb} \code{K.Moore} \code{K.Null} \code{K.Orton} \code{K.Warner} \code{L.Bell} \code{L.Brown} \code{L.Fitzgerald} \code{L.Jones} \code{L.McCown} \code{L.Naanee} \code{L.Thomas} \code{L.Tomlinson} \code{L.Washington} \code{M.Barber} \code{M.Barkley} \code{M.Bradley} \code{M.Brunell} \code{M.Bulger} \code{M.Bush} \code{M.Cassel} \code{M.Clayton} \code{M.Flynn} \code{M.Forte} \code{M.Glennon} \code{M.Gray} \code{M.Grice} \code{M.Gutierrez} \code{M.Hall} \code{M.Hasselbeck} \code{M.James} \code{M.Jenkins} \code{M.Jones-Drew} \code{M.Kafka} \code{M.Koenen} \code{M.Lee} \code{M.Leinart} \code{M.Lynch} \code{M.Mariota} \code{M.Massaquoi} \code{M.McBriar} \code{M.McGloin} \code{M.Moore} \code{M.Prater} \code{M.Reece} \code{M.Robinson} \code{M.Ryan} \code{M.Sanchez} \code{M.Sanu} \code{M.Schaub} \code{M.Scifres} \code{M.Simms} \code{M.Spurlock} \code{M.Stafford} \code{M.Vick} \code{M.Wallace} \code{M.Williams} \code{Mat.Moore} \code{Matt.Moore} \code{N.Foles} \code{O.Beckham} \code{P.Cooper} \code{P.Crayton} \code{P.Dawson} \code{P.Garcon} \code{P.Hillis} \code{P.Lynch} \code{P.Manning} \code{P.McAfee} \code{P.Peterson} \code{P.Rivers} \code{P.White} \code{R.Bartel} \code{R.Brown} \code{R.Bush} \code{R.Cobb} \code{R.Dixon} \code{R.Fitzpatrick} \code{R.Golden} \code{R.Griffin} \code{R.Grossman} \code{R.Helu} \code{R.Hodges} \code{R.Lindell} \code{R.Lindley} \code{R.Mallett} \code{R.Moss} \code{R.Nassib} \code{R.Quigley} \code{R.Rice} \code{R.Smith} \code{R.Tannehill} \code{R.Wayne} \code{R.Williams} \code{R.Wilson} \code{RSED. Alex Smith} \code{RSED. Ryan} \code{S.Bradford} \code{S.Breaston} \code{S.Hill} \code{S.Holmes} \code{S.Koch} \code{S.Lanning} \code{S.Lechler} \code{S.Logan} \code{S.Mannion} \code{S.Martin} \code{S.McGee} \code{S.Renfree} \code{S.Rice} \code{S.Schillinger} \code{S.Tolzien} \code{S.Wallace} \code{S.Weatherford} \code{Sh.Hill} \code{St. Pierre} \code{T.Austin} \code{T.Benjamin} \code{T.Bouman} \code{T.Boykin} \code{T.Brady} \code{T.Brandstater} \code{T.Bridgewater} \code{T.Cadet} \code{T.Castille} \code{T.Collins} \code{T.Edwards} \code{T.Ginn} \code{T.Graham} \code{T.Hilton} \code{T.Jackson} \code{T.Lewis} \code{T.Masthay} \code{T.McEvoy} \code{T.Palko} \code{T.Pryor} \code{T.Romo} \code{T.Savage} \code{T.Siemian} \code{T.Smith} \code{T.Taylor} \code{T.Tebow} \code{T.Thigpen} \code{T.Way} \code{T.Yates} \code{V.Young} \code{W.Snead} \code{Z.Mettenberger}}
    \item{\code{PassOutcome}}{a factor with levels \code{Complete} \code{Incomplete Pass}}
    \item{\code{AirYards}}{a numeric vector}
    \item{\code{YardsAfterCatch}}{a numeric vector}
    \item{\code{InterceptionThrown}}{a numeric vector}
    \item{\code{Fumble}}{a numeric vector}
    \item{\code{TotalYards}}{a numeric vector}
  }
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
data(passPlays)
## maybe str(passPlays) ; plot(passPlays) ...
}
\keyword{datasets}
