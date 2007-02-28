\name{spatgraphs-rclustercounts}
\alias{rclustercounts}
\title{Compute clustercounts}
\description{
Compute clustercounts (componentcounts) of a point pattern graph. Two graphs are supported: geometric and mutual k-nearest neighbour (mknn). These are both symmetric graphs with suitable parameters 'k' and 'R'.  
}

\details{
  \tabular{ll}{
    Date: \tab 2007-02-26\cr
    License: \tab GPL v2 or later\cr
}
}

\arguments{
\item{pp}{Point pattern with at least members x,y,n}
\item{R}{If Geometric graph, the vector of R for which the clustercount is needed. R>0!!!!}
\item{k}{If mknn-graph, the vector of k for which the clustercount is needed.}
\item{graph}{1 = geometric, 2 = mknn}
}

\value{
Returns a with the counts as 'n' and 'r' or 'k' depending on the graph.
}

\examples{
library(spatstat)
p<-rpoispp(100)
R<-seq(0.0001,0.25,length=50)
result1<-rclustercounts(p,R=R,graph=1)
k<-1:10
result2<-rclustercounts(p,k=k,graph=2)
par(mfrow=c(1,2))
plot(result1$r,result1$n,type="l",main="Geometric graph")
plot(result2$k,result2$n,type="l",main="Mutual k-nearest neighbours graph");
}
\keyword{spatial}
