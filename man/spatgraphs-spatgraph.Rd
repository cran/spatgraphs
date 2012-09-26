\name{spatgraphs-spatgraph}
\alias{spatgraph}
\alias{SG_GRAPH_PARS}
\alias{SG_SUPPORTED_GRAPHS}
\alias{sg_modify_pp}
\alias{sg_verify_parameters}
\alias{sg_default_par}
\title{spatgraph}
\description{
  Compute a spatial graph for a given 2D- or 3D- point pattern. 
}
\usage{
 spatgraph(pp, type="knn", par=NULL, preprocessR=0, dbg=FALSE, 
           doDists=FALSE, preDists=NULL, preGraph=NULL, toroidal=FALSE, include=NULL)
}

\arguments{
\item{pp}{Point pattern with members x,y,n,window. Window must have x- and y-limits according to given x,y. see package \code{spatstat}, class \code{ppp}.}
\item{type}{One of the supported graph types, see below.}
\item{par}{Parameter(s) for the graph, see below.}
\item{preprocessR}{If >0 first compute geometric graph and then the \code{type} graph using the preprocessed edgelist. Useful for narrowing down the search space for bigger pp's.}
\item{dbg}{Boolean, print additional information during the execution.}
\item{doDists}{Boolean, default FALSE. If true, precompute and store the pairwise distances. Speeds things up quite a lot but takes O(n\^2) memory!}
\item{preDists}{Optional: precalculated distance matrix for the points. Can be used in case of non-standard metric spaces.}
\item{preGraph}{Precalculated graph to be used as search space for neighbours. Useful for large computation of e.g. Gabriel graphs.}
\item{toroidal}{Make a toroidal distance calculation. Not useful when visualizing but useful for edge correction in summary calculations.}
\item{include}{A 0-1-vector describing which points' neighbourhoods to calculate, i.e. include[i]=1 => compute neighbourhood of pp[i]. }
}

\details{
The following 'type' values are accepted, note that some of them need also the 'par': 

 \tabular{rl}{
 'geometric'\tab par=numeric>0. Geometric graph, par = connection range.\cr
 'knn'\tab par=integer>0. k-nearest neighbours graph, par = k.\cr
 'mass_geometric'\tab Connect two points if ||x-y||<m(x).\cr
 'gabriel'\tab Gabriel graph. Optional parameter \code{par=k}: Allow k points in the shared circle between two points. Normal Gabriel equals k=0.\cr
 'delaunay'\tab Delaunay triangulation. Note! Only 2D, and no check for collinearity: use \code{rjitter} from \code{spatstat} if unsure.\cr
 'MST'\tab Minimal spanning tree.\cr
 'markcross'\tab Connect two points if ||x-y||<m(x)+m(y).\cr
 'SIG'\tab Spheres of Influence. Connect x and y iff ||x-y|| < dn(x) + dn(y), i.e. if their 'individual domains' intersect.\cr
 'RST'\tab par=c(x0,y0,z0). Radial spanning tree, par=origin of radiation. z0 i.e. z-coordinate can be omitted if in 2D.\cr
 'RNG'\tab Relative neighbourhood graph. Connect x and y if the lens between them is empty. Lens in this case means the intersection of discs b(x,r) and b(y,r), r=||x-y||.\cr
 'CCC'\tab par=integer (or string). Class-Cover-Catch, par=target type. This needs a multivariate point pattern.\cr
 'STIR'\tab par=c(noise,alpha,beta,gamma). Signal-To-Noise-Ratio, par: background noise, signal attenuation (1/r^alpha), 
       signal threshold beta, interference tolerance gamma. See ref 1.\cr
}
where
\preformatted{
 m(x) ~ real valued mark for x (size, mass, diameter, transmission power...)
 dn(x) ~ distance to the nearest neighbour of x
}
The graphs 'mass_geometric', 'markcross' and 'STIR' use scalar marks, 'CCC' class marks (e.g. integer). If given 'pp' has no marks it will be marked with 1.0's.

If your window is non-convex polygon, you can use \code{\link{clip.sg}} to cut extra edges that cross the border.


}

\value{
An object of class \code{\link{sg}}.
}


\references{
Dousse, O., Baccelli, F. \& Thiran, P.:  Impact of Interferences on Connectivity in Ad Hoc
Networks. IEEE/ACM Transactions on Networking,13 (2),p. 425-436,2005.

Marchette, D.: Random Graphs for Statistical Pattern Recognition, Wiley 2004.
}
\keyword{spatial}
