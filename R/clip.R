# clip.R
# 
# Cut edges with window, useful for non-convex windows.
#
# Author: Tuomas Rajala <tuomas.rajala@jyu.fi>
###############################################################################

clip.sg<-function(x, pp, window=NULL)
{
	if(is.null(window)){
		window<-pp$window
		if(is.null(window))
			stop("Either provide pp with a window-element or a window-parameter.")
	}
	
	if(window$type!="polygonal") stop("window should be polygonal and contain no holes (see spatstat, 'owin').")
	
	## boundary lineset
	bx<-by<-NULL
	for(xy in window$bdry) {
#		if(xy$hole)stop("Can't handle window with a hole: Please contact spatgraphs-developer tuomas.rajala@jyu.fi .")
		x0<-rep(xy$x,each=2)
		bx<-c(bx,x0[-1],x0[1])
		y0<-rep(xy$y,each=2)
		by<-c(by,y0[-1],y0[1])
	}
	boundary<-cbind(matrix(bx, ncol=2, byrow=TRUE), matrix(by, ncol=2, byrow=TRUE))[,c(1,3,2,4)]
	## the id's of line endpoints so that we know which edges are cut
	e<-x$edges
	idp0<-rep(1:x$N, sapply(e, length))
	idp1<-unlist(e)
	id<-1:length(idp0)

	## create lineset from graph
	nl<-sapply(e, length)
	x0<-rep(pp$x, nl)
	y0<-rep(pp$y, nl)
	x1<-pp$x[idp1]
	y1<-pp$y[idp1]
	lineset<-data.frame(x0,y0,x1,y1, idp0, idp1,id)
	
	### Find intersections, brute force:
	check.crossing<-function(xxyy, b){
		xxyy<-as.numeric(xxyy)
		aoxy<-xxyy[1:2]
		avxy<-xxyy[3:4]-aoxy
		Bo<-t(b[,1:2])-aoxy
		Ba<-t(b[,3:4])-aoxy
		th<- -atan2(avxy[2],avxy[1])  # rotation 
		M<-matrix(c(cos(th),sin(th),-sin(th),cos(th)),2)
		xo<-M%*%avxy
		coxy<-M%*%(Bo)
		cvxy<-M%*%(Ba)-coxy
		h<-(-coxy[2,]/cvxy[2,])
		x0<-coxy[1,]+h*cvxy[1,]
		( 0<h & h<1 & 0<x0 & x0<xo[1] )
	}
	cuts<-rowSums(apply(boundary, 1, check.crossing, b=lineset[,1:4]))>0
	
	## remove border crossing edges from the graph
	w<-which(cuts)
	losers<-lineset[w,"idp0"]
	lost<-lineset[w,"idp1"]
	for(i in unique(losers))
		x$edges[[i]]<-setdiff(x$edges[[i]], lost[losers==i])
	
	## done
	x	
}