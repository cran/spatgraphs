\name{spatgraphs-sg2igraph}
\alias{sg2igraph}
\alias{igraph2sg}
\title{sg2igraph}
\description{
  Convert spatgraph-object to an igraph-object, and vice versa.
}
\details{
  \tabular{ll}{
    Date: \tab 2009-04-29\cr
    License: \tab GPL v2 or later\cr
  }
}
\usage{
sg2igraph(g, pp=NULL)
igraph2sg(g)
}
\arguments{
\item{g}{The object to be converted.}
\item{pp}{point pattern. If none given, no details of the points will survive.}
}

\keyword{spatial}