\name{spatgraphs-mst_cut}
\alias{mst_cut}
\title{mst\_cut}
\description{
  Cut the MST-graph, that is remove all "too long" edges.
}
\details{
  \tabular{ll}{
    Date: \tab 2007-10-16\cr
    License: \tab GPL v2 or later\cr
  }
}

\arguments{
\item{mst}{Adjacency matrix of the MST. If none given, use pp to compute it.}
\item{pp}{Point pattern. Not needed if mst is provided.}
\item{dists}{Distance matrix for pp. See sg\_dists for example.}
\item{R}{The minimum distance for cutting.}
\item{level}{If R not provided use this level quantile.}
\item{tor}{Use toroidal correction}
}
\keyword{spatial}