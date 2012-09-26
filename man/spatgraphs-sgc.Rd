\name{spatgraphs-sgc}
\alias{sgc}
\title{sgc}
\description{
Cluster-object class for \code{\link{spatgraphs}}. Methods: print, plot. A cluster is a connected component.
}
\details{
  A list with elements
  \tabular{rl}{
  'clusters'\tab A list with N vectors, each containing the indices of corresponding cluster's points. N clusters.\cr
  'nclusters'\tab Number of clusters\cr
  'N'\tab Number of points in the pattern\cr
  'type'\tab 'type' of the original graph\cr
  'parameters'\tab 'parameters' of the original graph\cr
  }
}
\keyword{spatial}