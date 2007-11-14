# runif3d.R
# 
# Simple uniformly random point pattern generation in a 3d box
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################


runif3d<-function(n, window=list(x=c(0,1),y=c(0,1),z=c(0,1)))
{
	x<-runif(n,window$x[1],window$x[2])
	y<-runif(n,window$y[1],window$y[2])
	z<-runif(n,window$z[1],window$z[2])
	list(x=x,y=y,z=z,n=n,window=window)
}