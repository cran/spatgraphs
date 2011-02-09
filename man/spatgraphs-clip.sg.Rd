\name{spatgraphs-clip.sg}
\alias{clip.sg}
\title{Clip edges that cross window border}

\description{
Cuts off the edges that cross pattern's window border. Needs 'owin'-object from spatstat.
}
\usage{
clip.sg(x, pp, window=NULL)
}
\arguments{
\item{x}{spatgraph object.}
\item{pp}{Point pattern from which x is computed.}
\item{window}{Optional owin-object if the one in pp is not suitable.}
}

\details{
  \tabular{ll}{
    Date: \tab 2008-07-30\cr
    License: \tab GPL v2 or later\cr
  }
}
\keyword{spatial}