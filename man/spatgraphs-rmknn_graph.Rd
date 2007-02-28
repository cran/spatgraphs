\name{spatgraphs-rmknn_graph}
\alias{rmknn_graph}
\title{rmknn\_graph}
\description{
  Compute the adjacency matrix for point pattern X with relation
\preformatted{
    x~y <==> x\in knn(y) AND y\in knn(x)
}
where knn(y) is the set of k-nearest neighbours (other points) of y.

}

\details{
  \tabular{ll}{
    Date: \tab 2007-03-01\cr
    License: \tab GPL v2 or later\cr
}
}

\arguments{
\item{pp}{Point pattern with elements x,y,n. }
\item{k}{How big neighbourhood?}
}

\keyword{spatial}
