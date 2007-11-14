\name{spatgraphs-spatgraph}
\alias{spatgraph}
\title{spatgraph}
\description{
  Compute an adjacency matrix for a given 2D- or 3D- point pattern.
}
\details{
  \tabular{ll}{
    Date: \tab 2007-10-16\cr
    License: \tab GPL v2 or later\cr
  }
}

\arguments{
\item{pp}{Point pattern with members x,y,z,n,window. z is not oblicatory. Window must have x-,y- and z-limits according to given x,y,z.}
\item{type}{Supported types are: geometric, mark\_geometric, knn, kmnn, markcross, SIG, RST, MST, CCC, gabriel.}
\item{pars}{List of parameters for a give graph type, with name(s) in respective order: "R", "", "k", "k", "", "", "x,y,z", "","", "".}
\item{toroidal.correction}{Toroidal correction in distance calculation.}
\item{print_dbg}{Print some additional information.}
}
\keyword{spatial}