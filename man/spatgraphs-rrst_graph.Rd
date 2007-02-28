\name{spatgraphs-rrst_graph}
\alias{rrst_graph}
\title{rrst\_graph}
\description{
  Compute the adjacency matrix for point pattern X using the radial spanning tree rule. That is, choose a point in space 'o' as an 'center', and then connect each point x to it's nearest neighbour inside the ball B('o',||x-'o'||).
}

\details{
  \tabular{ll}{
    Date: \tab 2007-03-01\cr
    License: \tab GPL v2 or later\cr
}
}

\arguments{
\item{pp}{Point pattern with (at least) members x,y,n}
\item{x0}{The x coordinate of the center}
\item{y0}{The y coordinate of the center}
}

\keyword{spatial}
