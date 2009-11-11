\name{spatgraphs-package}
\alias{spatgraphs-package}
\alias{spatgraphs}
\docType{package}

\title{
Graphs for spatial point patterns
}


\description{
Compute various graph edge sets for 2D and 3D spatial point patterns such as the ppp-objects in R-package 'spatstat'. Also capable of cluster/component computation and visualization.
}

\details{
This package provides the following graph computations,
all handled by the spatgraph()-function:

\preformatted{

 Graph                  relation x~y
-------------------------------------------------------------
 Geometric              ||x-y||<R                   
 Mass geometric         ||x-y||<m(x)                 
 Spheres of Influence   ||x-y||<d(x)+d(y)            
 Mark crossing          ||x-y||<m(x)+m(y)            
 k-Nearest neighbour    x in knn(y)
 Relative Neighbourhood see refs.                 
 Radial spanning tree   see refs.            
 Minimum spanning tree  see refs.            
 Gabriel graph          see refs.            
 Class cover catch      see refs.            
 Delauney triangulation see refs.
 Signal-to-noise-ratio  see refs.

where 
    ||.|| ~ euclidian distance
    m(x) ~ mass, size i.e. real mark of x
    d(x) ~ the distance to the nearest neighbour of x.
    knn(x) ~ the k nearest neighbours set of x    

The minimum spanning tree is computed using Prim's algorithm.

The classes sg and sgc are defined, with their own plot- and print-methods.
For adjacency matrices there is a class sgadj. This is mainly unused due to
the memory requirements.

Also included are the following functions (3d stuff requires rgl):

Function name          Description
----------------------------------------------------------
spin3d                Little program to animate the rgl-3d image
runif3d				  Simple 3d uniform pp generation
spatcluster           Compute clusters from adjacency matrix
tosgadj				  Convert to adjacency matrix form
tosg				  Convert to edge list from sgadj
cut					  Cut edges longer than given R>0
prune				  Prunes the graph, aimed for MST
sg2dxf				  Write graph to a dxf file
shortestPath		  Find the shortest edgepath between two points
sg2igraph             Change the sg-object to igraph-object of package igraph 
}

}

\author{
\preformatted{
Tuomas Rajala
University of Jyvaskyla, Finland
tarajala@maths.jyu.fi
}
}
\references{
Dousse, O., Baccelli, F. \& Thiran, P.:  Impact of Interferences on Connectivity in Ad Hoc
Networks. IEEE/ACM Transactions on Networking,13 (2),p. 425-436,2005.

Marchette, D.: Random Graphs for Statistical Pattern Recognition, Wiley 2004.

}

\seealso{ 
Spatial point processes in general, see the package 'spatstat'

For more versatile Voronoi/Delauney handling, see the package 'tripack'

The package 'rgl' is required for 3D-plotting.
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
  
  #library(rgl)
  graph_example3d<-function(n=200)
  {
	w<-c(0,1)
	phi<-runif(n,0,pi);tau<-runif(n,0,2*pi);r<-runif(n)^0.33
	pp3d<-list(x=r*sin(tau)*cos(phi),y=r*cos(phi)*cos(tau),z=r*cos(phi),n=n,window=list(x=w,y=w,z=w))
	e<-spatgraph(pp3d,"RST",par=c(x=0,y=0,z=0))
	plot3d(pp3d,size=2, main="Radial spanning tree",col="black")
	plot(e,pp3d,col="plum")
  }
  #graph_example3d()
  #spin3d()
}
\keyword{spatial}
