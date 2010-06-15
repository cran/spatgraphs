# sg2igraph.R
# 
# Convert the sg-object to a igraph-object of the package 'igraph'
# And vise versa
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################


sg2igraph<-function(g,pp=NULL)
{
	elist<-NULL
	actors<-data.frame(label=as.character((1:g$N)-1))
	
	for(i in 1:g$N)
	{
		if(length(g$edges[[i]]>0))
		{
			a<-cbind(i-1,g$edges[[i]]-1)
			elist<-rbind(elist, a)
		}
	}
	
	elist<-data.frame(from=as.character(elist[,1]),to=as.character(elist[,2]))
	
	if(!is.null(pp))
	{
		x<-pp$x
		y<-pp$y
		if(is.null(pp$z))pp$z<-pp$x*0
		z<-pp$z
		if(is.null(pp$marks))pp$marks<-pp$x*0
		mark<-pp$marks
		actors<-data.frame(names=as.character((1:length(x))-1),x=x,y=y,z=z,mark=mark)
	}
	
	a<-graph.data.frame(elist, vertices=actors, directed=FALSE)
	a$N<-g$N
	a$parameters<-g$parameters
	a$type<-g$type
	a
}

####################
igraph2sg<-function(g)
{
	b<-get.adjlist(g)
	for(i in 1:length(b))
		b[[i]]<-union(b[[i]]+1,NULL)
	
	sg(b, type=g$type, pars=g$parameters, note="coverted from igraph-object")
}