\name{lasR-package}
\alias{lasR-package}
\alias{lasR}
\docType{package}
\title{Laser processing in R
}
\description{
Handy functions for forest inventory researches using airborne laser scanning
}
\details{
\tabular{ll}{
Package: \tab lasR\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2012-06-012\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
Currently the package has functions for computing standard metrics used in area-based forest inventory assisted by airborne laser scanning. 
}
\author{
Hans Ole Orka <hans.ole.orka@gmail.com>
}
\references{
Naesset, E. (2002). Predicting forest stand characteristics with airborne scanning laser using 
a practical two-stage procedure and field data. Remote Sensing of Environment,Vol. 80, No. 1, pp. 88-99.

Naesset, E. (2004). Practical large-scale forest stand inventory using a small-footprint airborne 
scanning laser. Scandinavian Journal of Forest Research,Vol. 19, No. 2, pp. 164-179.
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(las)
dens <- laser.density(las$dz,las$ID,gtv=2)
dist <- laser.distribution(las$dz,las$ID,gtv=2)
X <- laser.metrics(las$dz,las$ID,gtv=2)
ID <- row.names(X)
 
#Metrics from only first of many and single echoes
first <- laser.metrics(las$dz[las$r==1],las$ID[las$r==1],gtv=2,suffix=".F")
names(first)
#Merics from last of many and single echoes
last <- laser.metrics(las$dz[las$r==las$n],las$ID[las$r==las$n],gtv=2,suffix=".L")
names(last)

}
