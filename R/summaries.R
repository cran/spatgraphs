# summaries.R
# 
# Graph based summaries for spatgraphs
#
# Author: Tuomas Rajala   <tarajala@maths.jyu.fi>
###############################################################################

# See page 109, Wong,Pattison,Robinson: Spat mod for sos netw, 2005, Physica A
#tor ~ toroidal correction, needs rect. window
clusfun<-function(pp,rvec,tor=1)
{
    if(is.null(pp$z)) {pp$z<-0*pp$x; pp$window$z<-c(0,1)}
    foo<-.C("sg_clusfun",
          x=as.double(as.vector(pp$x)),
          y=as.double(as.vector(pp$y)),
          z=as.double(as.vector(pp$z)),
          n=as.integer(pp$n),
          rvec=as.double(as.vector(rvec)),
          nR=as.integer(length(rvec)),
          xlim = as.double(as.vector(pp$window$x)),
          ylim = as.double(as.vector(pp$window$y)),
          zlim = as.double(as.vector(pp$window$z)),
          values=as.double(rep(0,length(rvec))),
          tor=as.integer(tor),
          PACKAGE="spatgraphs"
          )
    foo$values[foo$values>1]=NA
    foo$values
}

###########################################################
confun<-function(pp,R,h,rvec,ker=0,tor=0){
#computes the connectivity function
#point pattern as in library(spatstat)
#R=geometric graph parameter
#h=parameter for the kernel
#rvec=come on...
#ker=kernel function. 0=unif square,1=epanechnikov   
# also check the partial functions underneath, the clustering is a bit slow
#returns a list with $v=values, $e=edges of R-graph, $ec=clustermatrix of R-graph
    e<-spatgraph(pp,type="geometric",pars=list(R=R),toroidal.correction=tor)
    ec<-sgcluster(e)
    n<-pp$n
    xlim<-pp$window$x
    ylim<-pp$window$y
   	dists<-sg_dists(pp,tor=tor)
    if(is.null(pp$z)){pp$z<-0*pp$x;zlim<-c(0,1)}
	else zlim<-pp$window$z
    w<-rep(0,n*n)
    foo<-.C("sg_confun_weights",
          x=as.double(as.vector(pp$x)),
          y=as.double(as.vector(pp$y)),
          z=as.double(as.vector(pp$z)),
          w=as.double(as.vector(w)),
          n=as.integer(n),
          xlim=as.double(as.vector(xlim)),
          ylim=as.double(as.vector(ylim)),
          zlim=as.double(as.vector(zlim)),
          PACKAGE="spatgraphs"
          )
    v<-rep(0.0,length(rvec))
    result<-.C("sg_confun",
                e=as.integer(as.vector(ec$matrix)),
                n=as.integer(n),
                h=as.double(h),
                ker=as.integer(ker),
                rvec=as.double(as.vector(rvec)),
                rn=as.integer(length(rvec)),
                v=as.double(as.vector(v)),
                dists=as.double(as.vector(dists)),
                w=as.double(as.vector(foo$w)),
                PACKAGE="spatgraphs"
              )
    list(v=result$v,e=e,ec=ec)
}

###########################################################
cumconfun<-function(pp,rvec,type="geometric",pars=list(R=0.1),toroidal.correction=0){
#computes the cumulative connectivity function
#point pattern as in library(spatstat)
#rvec=come on...
#returns a list with $v=values, $e=edges of R-graph, $ec=clustermatrix of R-graph
# + $kest=estimator for K-function
    e<-spatgraph(pp,type=type,pars=pars,toroidal.correction=toroidal.correction)
    ec<-sgcluster(e)
    n<-pp$n
    xlim<-pp$window$x
    ylim<-pp$window$y
    if(is.null(pp$z)){pp$z<-0*pp$x;zlim<-c(0,1)} 
    else zlim<-pp$window$z
    w<-rep(0,n*n)
    foo<-.C("sg_confun_weights",
          x=as.double(as.vector(pp$x)),
          y=as.double(as.vector(pp$y)),
          z=as.double(as.vector(pp$z)),
          w=as.double(as.vector(w)),
          n=as.integer(n),
          xlim=as.double(as.vector(xlim)),
          ylim=as.double(as.vector(ylim)),
          zlim=as.double(as.vector(zlim)),
          PACKAGE="spatgraphs"
          )
    v<-rep(0.0,length(rvec))
    dists<-as.matrix(dist(cbind(pp$x,pp$y),upper=T))
    result<-.C("sg_cumconfun",
                e=as.integer(as.vector(ec$matrix)),
                n=as.integer(n),
                rvec=as.double(as.vector(rvec)),
                rn=as.integer(length(rvec)),
                v=as.double(as.vector(v)),
                dists=as.double(as.vector(dists)),
                w=as.double(as.vector(foo$w)),
                kest=as.double(rep(0.0,length(rvec))),
                ratio=as.double(rep(0.0,length(rvec))),
                PACKAGE="spatgraphs"
              )
    e<-matrix(e,byrow=T,ncol=n)
    kest<- (result$kest)/ (summary(pp)$int)^2
    list(v=result$v,ratio=result$ratio,e=e,ec=ec,kest=kest)
}

