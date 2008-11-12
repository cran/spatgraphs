# plot.R
# 
# Plotting of graphs and clusters
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
# 080608
###############################################################################
plot.sg<-function(x,...)plot_sg(x,...)
plot_sg<-function(g,pp, which=NULL,...)
# plot the edges on top of a point pattern pp
{
	if(is.null(which))which<-1:length(g$edges)
    if(is.null(pp[['z']])| length(pp[['z']])!=length(pp[['x']]))
		for(i in which)
		{
			s<-g$edges[[i]]
			x<-pp[['x']][s]
			y<-pp[['y']][s]
			n<-length(s)
			x<-as.vector( rbind(rep(pp[['x']][i],length(s)),x ))
			y<-as.vector( rbind(rep(pp[['y']][i],length(s)),y ))
			lines(x, y, ... )
		}
	else #3d
	{
		for(i in which){
			s<-g$edges[[i]]
			x<-pp[['x']][s]
			y<-pp[['y']][s]
			z<-pp[['z']][s]
			x<-as.vector( rbind(rep(pp[['x']][i],length(s)),x ))
			y<-as.vector( rbind(rep(pp[['y']][i],length(s)),y ))
			z<-as.vector( rbind(rep(pp[['z']][i],length(s)),z ))
			rgl.lines(x,y,z,...)
		}
	}
}
plot.sgc<-function(x,...)plot_sgc(x,...)
plot_sgc<-function(x,pp,...)
{
	for(i in 1:x$nclusters)
		points(pp$x[x$clusters[[i]]],pp$y[x$clusters[[i]]],col=i+1,...)
}
