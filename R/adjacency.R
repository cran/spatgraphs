# adjacency.R
# First version used to do things with matrices instead of lists,
# so stuff here exists only for backwise compatibility.
# 
# Convert from edge list to adjacency matrix and vice versa
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
##############################################################################


tosgadj<-function(x)
{
	if(!verifyclass(x,"sg"))stop("Give class 'sg' object.")
	A<-diag(0,x$N)
	for(i in 1:x$N)
	{
		A[i,x$edges[[i]]]<-1
	}
	sgadj(A,type=x$type,pars=x$parameters,sym=x$symmetric)
}

tosg<-function(x)
{
	if(!verifyclass(x,"sgadj"))stop("Give class 'sgadj' object.")
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
	type<-x$type
	nam<-names(x$parameters)
	p<-NULL
	if(!is.null(nam)){
		for(i in 1:length(nam))
			p<-paste(p,nam[i],"=",x$parameters[[i]],sep=" ",collapse=",")
		p<-paste("(",p,")") 
	}
	if(class(x)=="sgadj")kumpi<-"adjacency"
	cat(paste("'Spatgraphs' ",kumpi," matrix:",
					"\ngraph type '",type,"' ",p,", for ",x$N," points\n",sep=""))
	
}
##############################################################################
plot.sgadj<-function(x,...)
{
	plot.sg(tosg(x),...)
}