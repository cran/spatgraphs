# adjacency.R
# First version used to do things with matrices instead of lists.
# 
# Convert from edge list to adjacency matrix and vice versa
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
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

sg2adj<-function(x)
{
	verifyclass(x,"sg")
	A<-diag(0,x$N)
	for(i in 1:x$N)
	{
		A[i,x$edges[[i]]]<-1
	}
	sgadj(A,type=x$type,pars=x$parameters,sym=x$symmetric)
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
# the adjacency matrix version
sgadj<-function(edges=diag(0),type="?",pars=NULL,sym=TRUE)
{
	e<-list(matrix=edges)
	e$N<-dim(edges)[1]
	e$symmetric<-sym
	e$type<-type
	e$parameters<-pars
	class(e)<-"sgadj"
	e
}
##############################################################################
print.sgadj<-function(x,...)
{
	par_should_be<-unlist(SG_GRAPH_PARS[which(x$type==SG_SUPPORTED_GRAPHS)])
	nam<-names(x$parameters)
	p<-"?"
#   if(length(par_should_be)<2){if(par_should_be=="") p<-""}
	p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")
	cat(paste("'Spatgraphs' adjacency matrix:",
					"\ngraph type '",x$type,"'",p,", for ",x$N," points.\n",sep=""))
	if(!is.null(x$note))cat(paste("Note: ", x$note,".\n",sep=""))
	
}


##############################################################################
plot.sgadj<-function(x,...)
{
	plot.sg(adj2sg(x),...)
}

# eof