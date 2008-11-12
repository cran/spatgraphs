\name{spatgraphs-shortestPath}
\alias{shortestPath}
\title{Shortest path between nodes i and j}

\description{
  Find the shortest edgeconnected path between two given nodes/points with indices i and j (in pp). 
  
}
  \arguments{
  \item{pp}{Point pattern, as 'ppp' in 'spatstat'.}
  \item{i}{The start node of the path to find.}
  \item{j}{The target node of the path to find.}
  \item{g=NULL}{Graph which defines the edges. If NULL computes one for given additional parameters.}
  \item{...}{Passed on to 'spatgraph' if g is not given.}
  
  }

\details{
  \tabular{ll}{
    Date: \tab 2008-09-25\cr
    License: \tab GPL v2 or later\cr
  }
  Value is the sum of Euclidian lenghts of the edges in the shortest path. The algorithm is Dijkstra\'s algorithm.
}
\references{
E. W. Dijkstra: A note on two problems in connexion with graphs. 'Numerische Mathematik', 1 (1959), p. 269-271.

\url{http://en.wikipedia.org/wiki/Dijkstra's_algorithm}
}
\keyword{spatial}