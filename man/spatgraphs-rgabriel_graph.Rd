\name{spatgraphs-rgabriel_graph}
\alias{rgabriel_graph}
\title{rgabriel\_graph}

\description{
  Compute the adjacency matrix for point pattern X with relation
\preformatted{
    x~y <==> X\cap ( B(x,||x-y||)\cap B(y,||x-y||) ) = \emptyset
}

}

\details{
  \tabular{ll}{
    Date: \tab 2007-03-01\cr
    License: \tab GPL v2 or later\cr
}
This is still slow R-implementation, C-version will be made when there is enough reason.
}

\arguments{
\item{pp}{Point pattern with elements x,y,n. }
}

\keyword{spatial}
