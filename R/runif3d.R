# runif3d.R
# 
# Simple uniformly random point pattern generation in a 3d box
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################


runif3d<-function(n=c(10), window=list(x=c(0,1),y=c(0,1),z=c(0,1)))
{
	n0<-n
	x<-runif(sum(n),window$x[1],window$x[2])
	y<-runif(sum(n),window$y[1],window$y[2])
	z<-runif(sum(n),window$z[1],window$z[2])
	if(length(n)>1) 
	{
		m<-rep(1:length(n),n)
		return(list(x=x,y=y,z=z,n=sum(n),marks=factor(m),window=window))
	}
	else return(list(x=x,y=y,z=z,n=sum(n),window=window))
}

##
spin3d<-function(sec = 10, theta=0, rotspeed = 1, ang = 0, zoom = 1, fov = 60, capture=FALSE, cap.prefix="temp",j=100000) 
{
	start <- proc.time()[3]
	while ((i <- rotspeed * 36 * (proc.time()[3] - start)) < 36 * sec * rotspeed) 
	{
		rgl.viewpoint(i+theta, ang, zoom = zoom, fov = fov)
		j<-j+1
		if(capture) snapshot3d(file=paste(cap.prefix,substr(as.character(j),2,6),".png",sep=""), top=TRUE)
	}
	c(theta=theta+i,j=j)
}
