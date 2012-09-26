\name{spatgraphs-package}
\alias{spatgraphs-package}
\alias{spatgraphs}
\docType{package}

\title{
Graphs for spatial point patterns
}


\description{
Compute various spatial graphs for 2D and 3D spatial point patterns such as the \code{ppp}-objects in R-package \code{spatstat}. Also capable of cluster/component computation and visualization.
}

\details{
This package provides the following graph computations,
all handled by the \code{\link{spatgraph}}-function:

\preformatted{

 Graph                  relation x~y
-------------------------------------------------------------
 Geometric              ||x-y||<R                   
 Mass geometric         ||x-y||<m(x)                 
 Spheres of Influence   ||x-y||<dn(x)+dn(y)            
 Mark crossing          ||x-y||<m(x)+m(y)            
 k-Nearest neighbour    y in knn(x)
 Relative Neighbourhood see refs.                 
 Radial spanning tree   see refs.            
 Minimum spanning tree  see refs.            
 Gabriel graph          see refs.            
 Class cover catch      see refs.            
 Delaunay triangulation see refs.
 Signal-to-noise-ratio  see refs.

where 
    ||.|| ~ Euclidian distance
    m(x) ~ mass, size i.e. real mark of x
    dn(x) ~ the distance to the nearest neighbour of x.
    knn(x) ~ the k nearest neighbours set of x    
}
The minimum spanning tree is computed using Prim's algorithm.

The classes sg and sgc are defined, with their own plot- and print-methods. 3D plotting
requires package \code{rgl}.

For adjacency matrices see functions sg2adj and adj2sg. 

In addition to the main workhorse \code{\link{spatgraph}}-function are the following functions:
\preformatted{
Function name          Description
----------------------------------------------------------
spatcluster           Compute clusters (connected components)
shortestPath          Find the shortest edge-path between two points
edgeLengths           Lenghts of edges in a graph
sg2sym                Make the edges symmetric
sg2adj                Convert to an adjacency matrix form
cut.sg                Cut edges longer than given R>0
prune.sg              Prunes the graph, aimed for MST
clip.sg               Clip edges crossing the borders of a window. Useful for non-convex regions.
sg2dxf                Write a graph to a dxf file
sg2igraph             Change the sg-object to igraph-object of package \code{\link{igraph}}
sg2sparse             Convert between sg-object and Matrix-package sparseMatrix 
runif3d               Simple 3d uniform pp generation
spectral.sg	          Spectral clustering
}

}

\author{
\preformatted{
Tuomas Rajala
University of Jyvaskyla, Finland
tuomas.rajala@iki.fi
}
}
\references{
Dousse, O., Baccelli, F. \& Thiran, P.:  Impact of Interferences on Connectivity in Ad Hoc
Networks. IEEE/ACM Transactions on Networking,13 (2),p. 425-436,2005.

Marchette, D.: Random Graphs for Statistical Pattern Recognition, Wiley 2004.

}

\seealso{ 
Spatial point processes in general, see the package \code{spatstat}.

For more versatile Voronoi/Delaunay handling, see the package \code{tripack} or \code{deldir}.

The package \code{rgl} is required for 3D-plotting.
}

\examples{
  graph_example2d<-function(n=50,k=2,R=0.2)
  {
   pp2d<-list(x=runif(n),y=runif(n),n=n,window=list(x=c(0,1),y=c(0,1)))
   e1<-spatgraph(pp2d,"geometric",par=R)
   e2<-spatgraph(pp2d,"knn",par=k)
   e3<-spatgraph(pp2d,"MST")
   A<-spatcluster(e2)
   par(mfrow=c(1,3))
   plot(pp2d,main=paste("Geometric,R =",R))
   plot(e1,pp2d)
   plot(pp2d,main=paste("k-nn, k =",k))
   plot(e2,pp2d)
   plot(A,pp2d,pch=19)
   plot(pp2d, main="Minimum spanning tree")
   plot(e3,pp2d)
  }
  graph_example2d()

\dontrun{
## 3d example, requires library rgl
  library(rgl)
  graph_example3d<-function(n=200)
  {
	w<-c(0,1)
	phi<-runif(n,0,pi);tau<-runif(n,0,2*pi);r<-runif(n)^0.33
	pp3d<-list(x=r*sin(tau)*cos(phi),y=r*cos(phi)*cos(tau),z=r*cos(phi),n=n,window=list(x=w,y=w,z=w))
	e<-spatgraph(pp3d,"RST",par=c(x=0,y=0,z=0))
	plot3d(pp3d,size=2, main="Radial spanning tree",col="black")
	plot(e,pp3d,col="plum")
  }
  graph_example3d()
}
}
\keyword{spatial}
