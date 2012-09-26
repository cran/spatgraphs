\name{spatgraphs-sg}
\alias{sg}
\alias{verifyclass}
\alias{summary.sg}
\alias{t.sg}
\alias{t.sgadj}
\alias{summary.sgc}
\title{sg}


\description{
  Edge list-of-lists class for spatgraphs. Methods: print, plot, summary, t.

  Transposing is reversing edges.
}
\details{
The object is a list with members
\tabular{rl}{
'edges'\tab a list with each element edges[[i]] being a vector of point i's neighbours' indices. So i->j iff j in edges[[i]].\cr
'N'\tab cardinality of the point pattern.\cr
'symmetric'\tab Boolean. Is the graph symmetric. Might also be '?' (would not trust this one).\cr
'type'\tab Type of the graph as in the function call.\cr
'parameters'\tab Parameters, as in the function call.\cr
}


}
\keyword{spatial}