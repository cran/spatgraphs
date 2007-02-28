\name{spatgraphs-rsig_graph}
\alias{rsig_graph}
\title{rsig\_graph}
\description{
  Compute the adjacency matrix for point pattern X using the relation
\preformatted{
    x~y <==> ||x-y||<d(x)+d(y)
}
where d(x) is the distance to the nearest neighbour of x.
}

\details{
  \tabular{ll}{
    Date: \tab 2007-03-01\cr
    License: \tab GPL v2 or later\cr
}
}

\arguments{
\item{pp}{Point pattern with (at least) members x,y,n}
}

\keyword{spatial}
