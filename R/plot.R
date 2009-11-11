# plot.R
# 
# Plotting of graphs and clusters
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
# 080608
###############################################################################

plot.sg<-function(x, pp, which=NULL, directed=0, ...)
# plot the edges on top of a point pattern pp
{
	
	if(is.null(which))which<-1:length(x$edges)
    if(is.null(pp[['z']])| length(pp[['z']])!=length(pp[['x']]))
	{
		if(names(dev.cur())=="null device")plot(pp$x,pp$y,cex=.9,pch=19, xlab="",ylab="")
		for(i in which)
		{
			s<-x$edges[[i]]
			n<-length(s)
			if(directed==0) # no arrows
			{
				x0<-pp[['x']][s]
				y0<-pp[['y']][s]
				
				x0<-as.vector( rbind(rep(pp[['x']][i],n),x0 ))
				y0<-as.vector( rbind(rep(pp[['y']][i],n),y0 ))
				lines(x0, y0, ... )
			}
			else # arrows
			{
				n<-length(s)
				x0<-rep(pp[['x']][i],n)
				y0<-rep(pp[['y']][i],n)
				x1<-pp[['x']][s]
				y1<-pp[['y']][s]
				arrows(x0, y0, x1, y1, length=directed, ... )
			}
			
		}
	}
	else #3d
	{
		for(i in which){
			s<-x$edges[[i]]
			x0<-pp[['x']][s]
			y0<-pp[['y']][s]
			z0<-pp[['z']][s]
			x0<-as.vector( rbind(rep(pp[['x']][i],length(s)),x0 ))
			y0<-as.vector( rbind(rep(pp[['y']][i],length(s)),y0 ))
			z0<-as.vector( rbind(rep(pp[['z']][i],length(s)),z0 ))
			rgl.lines(x0, y0, z0,...)
		}
	}
}

# plot clusters
plot.sgc<-function(x, pp, atleast=2, spheres=FALSE,...)
{
	w<-(1:x$nclusters)[lapply(x$clusters,length)>=atleast]
	j<-1
	if(is.null(pp[['z']])| length(pp[['z']])!=length(pp[['x']]))
		for(i in w)
		{
			points(pp$x[x$clusters[[i]]],pp$y[x$clusters[[i]]],col=j,...)
			j<-j+1
		}
	else
	{
		if(spheres)f<-spheres3d
		else f<-points3d
		for(i in w)
		{
			f(pp$x[x$clusters[[i]]],pp$y[x$clusters[[i]]],pp$z[x$clusters[[i]]],col=j,...)
			j<-j+1
		}
	}
}
