\name{spatgraphs-other}
\alias{sg2sym}
\alias{edgeLengths}
\alias{edgelengths.sg}

\title{Make the graph symmetric/compute edge lengths}

\description{
\code{sg2sym} makes the graph symmetric.

\code{edgeLengths} returns the distances of edge-connected points as a list x=(i,j,d) such that distance(x$i[k],x$j[k])=x$d[k], k=1,...,NumbeOfEdges. 
}

\usage{
   sg2sym(x, way=1)
   edgeLengths(x, pp, ...)
}
\arguments{
\item{x}{sg-object.}
\item{pp}{Point pattern, for distances.}
\item{way}{(in sg2sym). If 1, use (xy OR yx) rule, if anything else use (xy AND yx) rule.}
\item{...}{ignored}
}

\keyword{spatial}