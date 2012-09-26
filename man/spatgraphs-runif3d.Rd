\name{spatgraphs-runif3d}
\alias{runif3d}
\title{runif3d}

\description{
  Simple simulation of uniform 3d point pattern.
}
\usage{
runif3d(n = c(10), window = list(x = c(0, 1), y = c(0, 1), z = c(0, 1)))
}
\arguments{
\item{n}{Vector of point counts, e.g. c(10,10) is two type pattern with 10 points per type.}
\item{window}{Rectangular cuboid window limits.}
}
\details{
 Simulates uniformly distributed points in 3d rectangle.
}
\keyword{spatial}