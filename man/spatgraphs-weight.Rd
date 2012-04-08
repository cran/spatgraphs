\name{spatgraphs-weight}
\alias{weight.sg}
\alias{sg2wadj}

\title{Weight edges}

\description{
Weight edges with a given function of node-node Euclidian distances.  
}
\details{
  \tabular{ll}{
    Date: \tab 2011-04-14\cr
    License: \tab GPL v2 or later\cr
  }
}
\usage{
   weight.sg(x, pp, f=function(x) exp(-x^2/scale), scale=1, ...)
   sg2wadj(x)
}
\arguments{
\item{x}{Graph.}
\item{pp}{Point pattern.}
\item{f}{R-function for weight computation; a function of distance.}
\item{scale}{Scaling factor used in (possibly) in f.}
\item{...}{Further parameters (ignored at the moment.)}
}
\value{
	returns the graph x with additional compontent $weights that contains the weights 
	for edges listed in $edges. For example, if x$edges[[1]] is c(2,3), and x$weights
	is c(1, 0.5), then the weight of edge 1->2 is 1 and edge 1->3 is 0.5.
}
