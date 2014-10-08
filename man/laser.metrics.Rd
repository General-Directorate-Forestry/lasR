\name{laser.metrics}
\alias{laser.metrics}
\alias{laser.distribution}
\alias{laser.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute laser metrics}
\description{
laser.metrics are a wrapper of the individual functions laser.distribution and laser.density. 
}
\usage{
laser.metrics(x,id,gtv=2,ctv = function(x){quantile(x,probs=0.95,type=2)},percentiles=seq(0.1,0.9,0.1),prefix="",suffix="",distpre="H",denspre="D")
laser.density(x, id, gtv = gtv, ctv =  function(x){quantile(x,probs=0.95,type=2)}, prefix = "D", suffix = "")
laser.distribution(x, id, gtv = gtv, percentiles = seq(0.1, 0.9, 0.1), prefix = "H", suffix = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The values to compute metrics from, usually the canopy heigth (dz), but the intensity could also be used. 
}
  \item{id}{
A vector of ids
}
  \item{percentiles}{
The percentiles to be computed by laser.distribution. Default is seq(0.1, 0.9, 0.1) 
}
  \item{gtv}{Ground threshold value, default is 2
}
  \item{ctv}{Canopy threshold value, default is function(x){quantile(x,probs=0.95,type=2)}
}
  \item{prefix}{a prefix added to variable names 
}
  \item{suffix}{a suffix added to variable names 
}
  \item{distpre}{A prefix added to variable names derived using laser.distribution. Default is H.  
}
  \item{denspre}{A suffix added to variable names derived using laser.distribution. Default is D.  
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
N?sset, E. (2002). Predicting forest stand characteristics with airborne scanning laser using 
a practical two-stage procedure and field data. Remote Sensing of Environment,Vol. 80, No. 1, pp. 88-99.

Naesset, E. (2004). Practical large-scale forest stand inventory using a small-footprint airborne 
scanning laser. Scandinavian Journal of Forest Research,Vol. 19, No. 2, pp. 164-179.
}
\author{
Hans Ole ?rka
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(las)
#Density metrics
dens <- laser.density(las$dz,las$ID,gtv=2)
#Height distibution metrics
dist <- laser.distribution(las$dz,las$ID,gtv=2)
X <- laser.metrics(las$dz,las$ID,gtv=2)
 
#Metrics from only first of many and single echoes
first <- laser.metrics(las$dz[las$r==1],las$ID[las$r==1],gtv=2,suffix=".F")
names(first)
#Merics from last of many and single echoes
last <- laser.metrics(las$dz[las$r==las$n],las$ID[las$r==las$n],gtv=2,suffix=".L")
names(last)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
