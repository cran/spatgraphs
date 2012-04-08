# plot.R
# 
# Plotting of graphs and clusters
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
# 080608
###############################################################################

plot.sg<-function(x, pp, add=TRUE, which=NULL, directed=0, 
		          add.points=FALSE, points.col="black", points.pch=1, points.cex=1, lines.col="gray30", ...)
# plot the edges on top of a point pattern pp
{
	
	if(is.null(which))which<-1:length(x$edges)
    if(is.null(pp[['z']])| length(pp[['z']])!=length(pp[['x']]))
	{
		if(!add)
		{
			if(is.null(pp$window$x))pp$window$x<-range(pp$x)
			if(is.null(pp$window$y))pp$window$y<-range(pp$y)
			plot(NA, NA,cex=0.01, xlim=pp$window$x, ylim=pp$window$y, asp=1, xlab="x", ylab="y")
			add.points<-TRUE
		}
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
				lines(x0, y0, col=lines.col, ... )
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
		if(add.points)
		{
			points(pp$x, pp$y, pch=points.pch, col=points.col, cex=points.cex)
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
plot.sgc<-function(x, pp, atleast=2, add=TRUE, pch=19, cex=1, spheres=FALSE, col, ...)
{
	w<-(1:x$nclusters)[sapply(x$clusters,length)>=atleast]
	n<-x$nclusters
	j<-1
	if(missing(col))col<-rgb(red=runif(n,0.1,1),green=runif(n,0.1,1), blue=runif(n,0.1,1) )
	
	if(is.null(pp[['z']])| length(pp[['z']])!=length(pp[['x']])){
		if(!add)
		{
			if(is.null(pp$window$x))pp$window$x<-range(pp$x)
			if(is.null(pp$window$y))pp$window$y<-range(pp$y)
			plot(NA, NA,cex=0.01, xlim=pp$window$x, ylim=pp$window$y, asp=1, xlab="x", ylab="y")
		}	
		for(i in w)
		{
			points(pp$x[x$clusters[[i]]],pp$y[x$clusters[[i]]],col=col[i], pch=pch, cex=cex, ...)
		}
	}
	else{
		if(spheres)f<-spheres3d
		else f<-points3d
		for(i in w)
		{
			f(pp$x[x$clusters[[i]]],pp$y[x$clusters[[i]]],pp$z[x$clusters[[i]]],col=col[i],...)
		}
	}
}
