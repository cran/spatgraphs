# edgelengths.R
# 
# Return the edgelenghts
#
# Author: Tuomas Rajala <tuomas.a.rajala@jyu.fi>
###############################################################################
edgelengths.sg<-
edgeLengths<-function(x, pp, ...)
{
	if(missing(pp)) stop("Need 'pp' for distances.")
	verifyclass(x,"sg")
	res<-list()
	ivec<-jvec<-dvec<-NULL
	if(is.null(pp$z))pp$z<-pp$x*0
	for(i in 1:x$N)
	{
		iedges<-x$edges[[i]]
		for(j in iedges)
		{
			ivec<-c(ivec,i)
			jvec<-c(jvec,j)
			d<- sqrt( diff(pp$x[c(i,j)])^2 + diff(pp$y[c(i,j)])^2 + diff(pp$z[c(i,j)])^2)
			dvec<-c(dvec,d)
			x$edges[[j]]<-setdiff(x$edges[[j]],i)
		}
	}
	
	res$i<-ivec
	res$j<-jvec
	res$d<-dvec
	res$n<-length(res$i)
	res	
}