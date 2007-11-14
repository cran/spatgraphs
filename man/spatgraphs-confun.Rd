\name{spatgraphs-confun}
\alias{confun}
\title{confun}
\description{
  Compute the connectivity function for a given point pattern. Only 2D-patterns at the moment, but will be fixed when the theory is checked.
}
\details{
  \tabular{ll}{
    Date: \tab 2007-10-16\cr
    License: \tab GPL v2 or later\cr
  }
}

\arguments{
\item{pp}{Point pattern.}
\item{R}{Geometric graph parameter.}
\item{h}{Kernel smoothing bandwith.}
\item{rvec}{Distance values of interest.}
\item{ker}{Kernel type: 0=box, 1=Epanechnikov.}
\item{tor}{Toroidal correction.}
}
\references{
Illian, Penttinen, Stoyan, Stoyan: Statistical analysis and modelling of spatial point patterns. Springer, 2008.
}
\keyword{spatial}