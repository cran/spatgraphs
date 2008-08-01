\name{spatgraphs-spatgraph}
\alias{spatgraph}
\alias{SG_GRAPH_PARS}
\alias{SG_SUPPORTED_GRAPHS}
\alias{sg_modify_pp}
\alias{sg_verify_parameters}
\title{spatgraph}
\description{
  Compute an adjacency list-of-lists for a given 2D- or 3D- point pattern. 

  \tabular{ll}{
    Date: \tab 2008-07-29\cr
    License: \tab GPL v2 or later\cr
  }


}

\arguments{
\item{pp}{Point pattern with members x,y,n,window. Window must have x- and y-limits according to given x,y. see package 'spatstat', class 'ppp'.}
\item{type}{One of the supported graph types, see below.}
\item{par}{Parameter(s) for the graph, see below.}
\item{preprocessR}{If >0 first compute geometric graph and then the 'type' graph using the preprocessed edgelist. Useful to narrow the search space for bigger pp's.}
\item{dbg}{Boolean, print additional information of the execution.}
\item{doDists}{Boolean, default FALSE. If true, precompute the distance diagonal matrix. Speeds up quite a lot but takes O(n\^2) memory!}
\item{toroidal}{Make a toroidal distance calculation. Not useful when visualizing but useful for edge correction in summary calculations.}
}

\details{



The following 'type' values are accepted, note that some of them need also the 'par': 

 \tabular{rl}{
 'geometric'\tab par=numeric>0. Geometric graph, par = connection radius.\cr
 'knn'\tab par=integer>0. k-nearest neighbours graph, par = k.\cr
 'mass\_geometric'\tab Connect two points if ||x-y||<m(x).\cr
 'gabriel'\tab Gabriel graph.\cr
 'delauney'\tab Delauney triangulation. !Only 2D!\cr
 'MST'\tab Minimal spanning tree.\cr
 'markcross'\tab Connect two points if ||x-y||<m(x)+m(y).\cr
 'SIG'\tab Spheres of Influence.\cr
 'RST'\tab par=c(x0,y0,z0). Radial spanning tree, par=origin of radiation.\cr
 'RNG'\tab Relative neighbourhood graph.\cr
 'CCC'\tab par=integer (or string). Class-Cover-Catch, par=target type.\cr
 'STIR'\tab Signal-to-noise-ratio. NOT YET IMPLEMENTED.\cr
}
where
\preformatted{
 m(x) ~ real valued mark for x (size, mass, diameter, transmission power...)
}

}
\keyword{spatial}
