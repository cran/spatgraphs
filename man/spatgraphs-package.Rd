\name{spatgraphs-package}
\alias{spatgraphs-package}
\alias{spatgraphs}
\docType{package}

\title{
Graphs for spatial point patterns
}


\description{
Compute various graph adjacency matrices for 2D and 3D spatial point patterns such as the ppp-objects in R-package 'spatstat'. Also included: plotting graphs, cluster/component computation and visualization and two summaries, connectivity function and clustering function.
}

\details{
  \tabular{ll}{
    Date: \tab 2007-10-14\cr
    License: \tab GPL v2 or later\cr
}


This package provides the following graph adjacency matrix computations,
all handled by the spatgraph()-function:

\preformatted{

 Graph                  relation x~y
-------------------------------------------------------------
 Geometric              ||x-y||<R                   
 Marked geometric       ||x-y||<m(x)                 
 Spheres of Influence   ||x-y||<d(x)+d(y)            
 Mark crossing          ||x-y||<m(x)+m(y)            
 Nearest neighbour      ||x-y||<d(x)                 
 k-Nearest neighbour    x in knn(y)                  
 Mutual k-Nearest       x in knn(y) AND y in knn(x) mknn
 Radial spanning tree   see refs.            
 Minimum spanning tree  see refs.            
 Gabriel graph          see refs.            
 Class cover catch      see refs.            

where 
    ||.|| ~ euclidian distance
    m(x) ~ mark of x
    d(x) ~ the distance to the nearest neighbour of x.
    knn(x) ~ the k nearest neighbours set of x    

The minimum spanning tree is computed using Prim's algorithm.

Also included are the following functions:

Function name          Description
----------------------------------------------------------
plot_graph             Plot the graph on top of point pattern
spin3d                 Little program to animate the rgl-3d image

sg_cluster             Compute clustermatrix from adjacency matrix
sg_clusterlist         List the clusters in clustermatrix
plot_clusters          Plot the clusters

NOTE: As the algorithms currently work on the adjacency matrices the memory requirement is (n^2).

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
Marchette, David J.: Random Graphs for Statistical Pattern Recognition, Wiley 2004.
}

\keyword{ package }
\seealso{ 
Spatial point processes in general, see the package 'spatstat'

For Voronoi/Delauney duality, see the package 'tripack'

The package 'rgl' is required for 3D-plotting.
}

\examples{
  graph_example2d<-function(n=50,k=3,R=0.2)
  {
   pp2d<-list(x=runif(n),y=runif(n),n=n,window=list(x=c(0,1),y=c(0,1)))
   e1<-spatgraph(pp2d,"geometric",pars=list(R=R))
   e2<-spatgraph(pp2d,"kmnn",pars=list(k=k))
   e3<-spatgraph(pp2d,"MST")
   A<-sgcluster(e2)
   par(mfrow=c(1,3))
   plot(pp2d,main=paste("Geometric,R =",R));plot_graph(pp2d,e1,add=TRUE)
   plot_graph(pp2d,e2,main=paste("Mutual k-nn, k =",k))
   plot_clusters(pp2d,A,add=TRUE)
   plot_graph(pp2d,e3,main="Minimum spanning tree")
  };graph_example2d()

  graph_example3d<-function(n=200)
  {
    library(rgl)
    w<-c(0,1)
    phi<-runif(n,0,pi);tau<-runif(n,0,2*pi);r<-runif(n)^0.33
    pp3d<-list(x=r*sin(tau)*cos(phi),y=r*cos(phi)*cos(tau),z=r*cos(phi),n=n,window=list(x=w,y=w,z=w))
    e<-spatgraph(pp3d,"RST",pars=list(x=0,y=0,z=0))
    plot_graph(pp3d,e,pointsize=2,main="Radial spanning tree",linecolor="plum")
  };
  graph_example3d()
  
}
\keyword{spatial}
