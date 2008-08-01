# class.R
# 
# Defines the graph-class and cluster-class
#
#
# TODO: - consider including double edges
#		- how about weighted edges?
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
   if(par_should_be=="") p<-""
   else p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")
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
	if(par_should_be=="") p<-""
	else p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")
	
	cat(paste("'Spatgraphs' cluster/component list-of-lists:",
	"\ngraph type '",x$type,"'",p,", ",x$N," component",ifelse(x$N>1,"s",""),".\n",sep=""))
	if(!is.null(x$note))cat(paste("Note: ", x$note,".\n",sep=""))
}

###################################################################
# symmetrisize
sg_to_sym<-function(e)
{
	for(i in 1:length(e$edges) )
		if(length(e$edges[[i]])>0)
			for(j in e$edges[[i]])
				e$edges[[j]]<-union(i,e$edges[[j]])
	e$symmetric<-TRUE
	e
}
