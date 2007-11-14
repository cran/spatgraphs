# class.R
# 
# Defines the graph-class and clustermatrix-class
#
# Author: Tuomas Rajala  <tarajala@maths.jyu.fi>
###############################################################################

sg<-function(edges=diag(0),type="?",pars=NULL,sym=TRUE)
{
	e<-list(matrix=edges)
	e$N<-dim(edges)[1]
	e$symmetric<-sym
	e$type<-type
	e$parameters<-pars
	class(e)<-"sg"
	e
}

print.sg<-function(x,...)
{
   type<-x$type
   nam<-names(x$parameters)
   p<-NULL
   if(!is.null(nam)){
   	for(i in 1:length(nam))
   	p<-paste(p,nam[i],"=",x$parameters[[i]],sep="",collapse=",")
    p<-paste("(",p,")") 
   }
   if(class(x)=="sg")kumpi<-"adjacency"
   else kumpi<-paste("cluster count",x$nclusters,"clustering")
   cat(paste("'Spatgraphs' ",kumpi," matrix:",
             "\ngraph type '",type,"' ",p,", for ",x$N," points\n",sep=""))
             
}

sgc<-function(edges, type="?",pars=NULL,n)
{
   e<-sg(edges,type,pars,sym=TRUE)
   e$nclusters<-n
   class(e)<-"sgc"
   e
}
print.sgc<-function(x,...)
{
	print.sg(x)
}