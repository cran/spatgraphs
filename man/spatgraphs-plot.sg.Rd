\name{spatgraphs-plot.sg}
\alias{plot.sg}
\alias{plot.sgc}
\alias{plot.sgadj}
\title{plot.sg}
\description{
  Plot the edges of graph, or color the clusters.
}
\details{
  \tabular{ll}{
    Date: \tab 2008-07-30\cr
    License: \tab GPL v2 or later\cr
  }
}
\usage{
   \method{plot}{sg}(x, pp, add=TRUE, which=NULL, directed=0, add.points=FALSE, 
           points.col="black", points.pch=1, points.cex=1, lines.col="gray30", ...)
}
\arguments{
\item{x}{spatgraph/spatcluster object}
\item{pp}{point pattern}
\item{add}{Add the lines to an existing plot (such as \code{plot(pp)}) or draw a new plot.}
\item{which}{Vector of indices or a Boolean vector of size \code{n}: Draw only edges starting at these points.}
\item{directed}{Draw arrows with this size. If 0, no arrows.}
\item{add.points}{Should we draw the points after the lines are drawn. Uses \code{points}, only 2D.}
\item{points.col}{Color(s) for the points if they are added.}
\item{points.pch}{Plotting character for points if they are added.}
\item{points.cex}{Plotting size of point character if points are added.}
\item{lines.col}{Color(s) for the lines.}
\item{...}{line size etc. for corresponding function (\code{lines}, \code{arrows}, \code{rgl.lines}).}
}
\keyword{spatial}