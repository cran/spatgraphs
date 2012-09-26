\name{spatgraph-spectral}
\alias{spectral.sg}
\title{Spectral clustering}

\description{
Spectral clustering: Given a weighted adjacency matrix W and G=diag(rowSums(W)),
the Laplacian of the graph is L=G-W. We compute the eigenvalues of L, and for the 2:(m+1) smallest
eigenvalues, we use the corresponding eigenvectors to do a K-means clustering. 
}
\usage{
	spectral.sg(x, pp, m=2, K=3, diagplot=FALSE, ...) 
}
\arguments{
\item{x}{Weighted graph, sg-object. Will be transformed to W using sg2wadj().}
\item{pp}{Point pattern.}
\item{m}{Number of eigenvectors to use.}
\item{K}{Number of clusters to look for using K-means.}
\item{diagplot}{Plot some diagnostics: Result: first 20 eigenvalues and m eigenvectors.}
\item{...}{Not used at the moment.}
}
\value{
List with $id element denoting each points cluster id, and $sgc is a spatcluster-object with the clusters.
}
\details{
  \tabular{ll}{
    Date: \tab 2011-04-15\cr
    License: \tab GPL v2 or later\cr
  }
}
\keyword{spatial}