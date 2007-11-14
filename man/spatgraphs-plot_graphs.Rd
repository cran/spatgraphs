\name{spatgraphs-plot_graph}
\alias{plot_graph}
\title{plot\_graph}
\description{
  Plot the graph on top of point pattern plot using the information in the neighbourhood matrix.
}

\details{
  \tabular{ll}{
    Date: \tab 2007-10-16\cr
    License: \tab GPL v2 or later\cr
}
}

\arguments{
\item{pp}{Point pattern with at least members x,y,n}
\item{edges}{Neighbourhood matrix}
\item{col="black"}{Line colour}
\item{bg="white"}{Background color (3d)}
\item{pointcolor="black"}{Point color (if add=F)}
\item{add=F}{Add to existing plot or create a new plot.}
\item{lty=1}{Line type, 1=solid,2=dashed etc.}
\item{pointsize=1}{Size of points to plot}
\item{axes=T} {Draw axes? (3D)}
\item{main=""}{Main title.}
\item{nticks=2}{Number of ticks in axes (3D)}
}

\keyword{spatial}
