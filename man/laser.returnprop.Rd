\name{laser.returnprop}
\alias{laser.returnprop}
\title{Compute the number and proportion of returntypes}
\usage{
  laser.returnprop(dz, RetNum, NumRet, id)
}
\arguments{
  \item{z}{surface height}

  \item{RetNum}{return numb}

  \item{NumRet}{number of returns}

  \item{id}{}
}
\value{
  data.frame with the numbers and proportion of echoes in
  differnt categories.
}
\description{
  Read the header of a las-file into R
}
\examples{
data(las)
prop <- laser.returnprop(las$dz,las$r,las$n,ID)
Echo.cat <- echo.cat(las$r,las$n)
}
\author{
  Hans Ole Orka \email{hans.ole.orka@gmail.org}
}

