\name{spatgraphs-shortestPath}
\alias{shortestPath}
\title{Shortest path between nodes i and j}

\description{
  Find the shortest edgeconnected path between two given nodes/points with indices i and j (in pp). 

}
\usage{
  shortestPath(i, j, g, pp=NULL, dbg=FALSE)
  
}
  \arguments{
  \item{i}{The start node of the path to find.}
  \item{j}{The target node of the path to find.}
  \item{g}{Graph which defines the edges.}
  \item{pp}{Point pattern. If given, the edges are of Euclidian length, otherwise each edge is of length 1.}
  \item{dbg}{Print runtime messages.}
  }

\details{
  \tabular{ll}{
    Date: \tab 2008-09-25\cr
    License: \tab GPL v2 or later\cr
  }
  Returns the distance and edges along the shortest path.
  
  Make sure the graph is symmetric.  
  
  The algorithm is Dijkstra\'s algorithm.
}
\references{
E. W. Dijkstra: A note on two problems in connexion with graphs. 'Numerische Mathematik', 1 (1959), p. 269-271.

\url{http://en.wikipedia.org/wiki/Dijkstra's_algorithm}
}
\keyword{spatial}