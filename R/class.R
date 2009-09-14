# class.R
# 
# Defines the graph-class and cluster-class
#
#
# TODO: - consider including double edges
# TODO: - how about weighted edges?
#
# Author: Tuomas Rajala  <tarajala@maths.jyu.fi>
# 150408
###############################################################################

# the main spatgraphs graph-class
#
sg<-function(edges=list(),type="?",pars=NULL,sym=TRUE, note=NULL)
{
	for(i in 1:length(edges)) edges[[i]]<-union(NULL, edges[[i]]) # remove possible dublicates amongst links -> simple graph 
	e<-list(edges=edges)
	e$N<-length(edges)
	e$symmetric<-"?"
	e$type<-type
	e$parameters<-pars
	if(!is.null(note))e$note<-note
	class(e)<-"sg"
	e
}
###############################################################################

# print method for sg

print.sg<-function(x,...)
{
   par_should_be<-unlist(SG_GRAPH_PARS[which(x$type==SG_SUPPORTED_GRAPHS)])
   nam<-names(x$parameters)
   p<-"?"
#   if(length(par_should_be)<2){if(par_should_be=="") p<-""}
   p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")
   cat(paste("'Spatgraphs' edge connection list-of-lists:",
             "\ngraph type '",x$type,"'",p,", for ",x$N," points.\n",sep=""))
   if(!is.null(x$note))cat(paste("Note: ", x$note,".\n",sep=""))
             
}
###############################################################################

# spatgraphs cluster class
sgc<-function(edges, type="?",pars=NULL,note=NULL)
{
   e<-sg(edges,type,pars,sym=TRUE)
   e$parameters<-pars
   e$nclusters<-length(edges)
   e$N<-max(unlist(lapply(edges,max)))
   names(e)[1]<-"clusters"
   class(e)<-"sgc"
   if(!is.null(note))e$note<-note
   e
}
###############################################################################
# sgc print method
print.sgc<-function(x,...)
{
	par_should_be<-unlist(SG_GRAPH_PARS[which(x$type==SG_SUPPORTED_GRAPHS)])
	nam<-names(x$parameters)
	p<-"?"
#   if(length(par_should_be)<2){if(par_should_be=="") p<-""}
	p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")
	
	cat(paste("'Spatgraphs' cluster/component list-of-lists:",
	"\ngraph type '",x$type,"'",p,", ",x$nclusters," component",ifelse(x$N>1,"s","")," (",x$N," points).\n",sep=""))
	if(!is.null(x$note))cat(paste("Note: ", x$note,".\n",sep=""))
}

###################################################################
# symmetrisation
# wrapper for 1way and 2way symmetrisation
sg2sym<-function(x, way=1)
{
	if(way==1)# symmetrisize with OR : one direction link is enough
	{
		for(i in 1:length(x$edges) )
			if(length(x$edges[[i]])>0)
				for(j in x$edges[[i]])
					x$edges[[j]]<-union(i,x$edges[[j]])
		x$symmetric<-TRUE
	}
	else # symmetrisize with AND: remove one direction links
	{
		for(i in 1:length(x$edges) )
			if(length(x$edges[[i]])>0)
				for(j in x$edges[[i]])
					if(! (i%in%x$edges[[j]]) )
						x$edges[[i]]<-setdiff(x$edges[[i]],j)
		x$symmetric<-TRUE
	}
	return(x)
}
#####################################################################
# ripped from package 'spatstat', 121108
verifyclass<-function (X, C, N = deparse(substitute(X)), fatal = TRUE)
{
	if (!inherits(X, C)) {
		if (fatal) {
			gripe <- paste("argument", sQuote(N), "is not of class",
					sQuote(C))
			stop(gripe)
		}
		else return(FALSE)
	}
	return(TRUE)
}
