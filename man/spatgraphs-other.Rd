\name{spatgraphs-other}
\alias{sg2sym}
\alias{sg.edgelengths}

\title{Make the graph symmetric/compute edge lengths}

\description{
\code{sg2sym} makes the graph symmetric.

\code{sg.edgelengths} returns the distances of edge-connected points as a list x such that distance(x\$i,x\$j)=x\$d 
}
\details{
  \tabular{ll}{
    Date: \tab 2009-09-14\cr
    License: \tab GPL v2 or later\cr
  }
}
\usage{
   sg2sym(x, way=1)
   sg.edgelengths(x, pp, ...)
}
\arguments{
\item{x}{sg-object.}
\item{way}{If 1, use (xy OR yx) rule, if 2 or anything else use (xy AND yx) rule.}
\item{pp}{Point pattern, for distances.}
\item{...}{ignored}
}

\keyword{spatial}