# adjacency.R
# First version used to do things with matrices instead of lists.
# 
# Convert from edge list to adjacency matrix and vice versa
#
# Author: Tuomas Rajala <tuomas.rajala@iki.fi>
##############################################################################

##############################################################################
# transpose adjacency matrix
t.sg<-function(x)
{
	z<-sg2adj(x)
	z$matrix<-t(z$matrix)
	adj2sg(z)
}
t.sgadj<-function(x)
{
	x$matrix<-t(x$matrix)
	x
}
##############################################################################
sg2sparse<-function(x)
{
	require(Matrix)
	ij<-NULL
	for(i in 1:x$N)
		if(length(x$edges[[i]])>0)
			ij<-rbind(ij, cbind(i, x$edges[[i]]))
	sparseMatrix(i=ij[,1], j=ij[,2], dims=c(x$N, x$N))
}

sparse2sg<-function(x)
{
	if(ncol(x)!=nrow(x))stop("parse2sg: adjacency matrix needs to be a square matrix.")
	edges<-vector("list", ncol(x))
	for(i in 1:ncol(x)){
		edges[[i]]<-which(x[i,]!=0)
	}
	sg(edges=edges,type="?",pars=NULL,sym=FALSE, note="From sparseMatrix")
}


##############################################################################
sg2adj<-function(x)
{
	verifyclass(x,"sg")
	A<-sg2sparse(x)
	sgadj(A, type=x$type, pars=x$parameters, sym=x$symmetric)
}

adj2sg<-function(x)
{
	verifyclass(x,"sgadj")
	A<-list()
	for(i in 1:x$N)
	{
		A[[i]]<-(1:x$N)[x$matrix[i,]==1]
	}
	sg(A, type=x$type, pars=x$parameters, sym=x$symmetric)	
}
##############################################################################
## what is this...
sg2wadj<-function(x)
{
	verifyclass(x,"sg")
	if(is.null(x$weights)) stop("No weights. Run weight.sg(x,...) .")
	W<-diag(0,x$N)
	for(i in 1:x$N)
	{
		W[i,x$edges[[i]]]<-x$weights[[i]]
	}
	sgadj(W, type=x$type, pars=x$parameters, sym=x$symmetric, other="weighted")
}
##############################################################################
# the adjacency matrix version
sgadj<-function(edges=NULL,type="?",pars=NULL,sym=TRUE, other="")
{
	e<-list(matrix=edges)
	e$N<-dim(edges)[1]
	e$symmetric<-sym
	e$type<-type
	e$parameters<-pars
	e$other<-other
	class(e)<-"sgadj"
	e
}
##############################################################################
print.sgadj<-function(x,...)
{
	par_should_be<-unlist(SG_GRAPH_PARS[which(x$type==SG_SUPPORTED_GRAPHS)])
	nam<-names(x$parameters)
	p<-"?"
	
	p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")
	cat(paste("'Spatgraphs' ",x$other," adjacency matrix:",
					"\ngraph type '",x$type,"'",p,", for ",x$N," points.\n",sep=""))
	if(!is.null(x$note))cat(paste("Note: ", x$note,".\n",sep=""))
	
}


##############################################################################
plot.sgadj<-function(x,...)
{
	plot.sg(adj2sg(x),...)
}

# eof