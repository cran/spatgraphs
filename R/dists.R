# dists.R
# 
# Distance computation functions for spatgraphs
#
# Author: Tuomas Rajala  <tarajala@maths.jyu.fi>
###############################################################################


sg_dists<-function(pp,tor=0)
{
    distf<-"sg_dists"
    if(is.null(pp$z)){pp$window$z<-c(0,0);pp$z<-0*pp$x}
    else distf<-paste(distf,"3d",sep="")
    if(tor) distf<-paste(distf,"_tor",sep="")
    dists<-diag(0,pp$n)
    dists<-.C(distf,
          x=as.double(as.vector(pp$x)),
          y=as.double(as.vector(pp$y)),
          z=as.double(as.vector(pp$z)),
          n=as.integer(pp$n),
          xlim = as.double(as.vector(pp$window$x)),
          ylim = as.double(as.vector(pp$window$y)),
          zlim = as.double(as.vector(pp$window$z)),
          values=as.double(as.vector(dists))
          )
    matrix(dists$values,ncol=pp$n)
}

## EO distance function
################
