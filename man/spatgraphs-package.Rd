\name{spatgraphs-package}
\alias{spatgraphs-package}
\alias{spatgraphs}
\docType{package}

\title{
Graphs for spatial point patterns
}


\description{
Compute various graph adjacency matrices for 2D- spatial point patterns such as the ppp-objects in R-package 'spatstat'. Also included are plotting of these graphs and cluster/component computation and visualization. 
}

\details{
  \tabular{ll}{
    Date: \tab 2007-02-26\cr
    License: \tab GPL v2 or later\cr
}


This package provides the following graph adjacency matrix computations:

\preformatted{

Function name    Graph type             relation x~y          sym?
-------------------------------------------------------------
 rgeo_graph       Geometric              ||x-y||<R            yes
 rmark_graph      Marked geometric       ||x-y||<m(x)         no
 rsig_graph       Spheres of Influence   ||x-y||<d(x)+d(y)    yes
 rcross_graph     Marked SIG             ||x-y||<m(x)+m(y)    yes
 rnn_graph        Nearest neighbour      ||x-y||<d(x)         no
 rknn_graph       k-Nearest neighbour    x in knn(y)          no
 rmknn_graph      Mutual k-Nearest       x in knn(y) AND y in knn(x)  yes
 rrst_graph       Radial spanning tree   see refs.            no
 rmst_graph       Minimum spanning tree  see refs.            no
 rgabriel_graph   Gabriel graph          see refs.            yes

where 
    ||.|| ~ euclidian distance in R^2
    m(x) ~ mark of x
    d(x) ~ the distance to the nearest neighbour of x.
    knn(x) ~ the k nearest neighbours set of x
    sym? ~ is the relation symmetric
    

The minimum spanning tree is computed using Prim's algorithm.       
    
Also included are the following functions:

Function name          Description
----------------------------------------------------------
plot_graph             Plot the graph on top of point pattern

rclustermatrix         Compute clustermatrix from adjacency matrix
rlist_clusters         List the clusters in clustermatrix
plot_clusters          Plot the clusters
rclustercounts         Compute clustercounts for various parameters

NOTE: As the algorithms currently work on the adjacency matrices the memory requirement is (n^2). This is to be improved in the future.

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
}

\examples{
  graph_example<-function(n=50,k=2,R=0.1)
  {
   pp<-list(x=runif(n),y=runif(n),n=n)
   e1<-rgeo_graph(pp,R=R)
   e2<-rmknn_graph(pp,k=k)
   e3<-rmst_graph(pp)
   A<-rclustermatrix(e2)
   par(mfrow=c(1,3))
   plot(pp,main=paste("Geometric,R =",R));plot_graph(pp,e1)
   plot(pp,main=paste("Mutual k-nn, k =",k));plot_graph(pp,e2)
   plot_clusters(pp,A)
   plot(pp,main="Minimum spanning tree");plot_graph(pp,e3)
  }
}
\keyword{spatial}
