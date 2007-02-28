##########################################################
#   last mod: 28.02.07 / TR
#
#   This file is part of package 'spatgraphs' for R
#
#   Functions for computing and plotting 
#   different kinds of graphs and computing and plotting
#   graph clusters/components for point patterns
#   such as the ppp-objects in package 'spatstat' by A. Baddeley
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
#
# 
#
#   Depends on c-module spatgraphs.c
#
# functions:
#   plot_graph(pp,e,arrows=F)    = adds the edges as lines to 
#                                  EXISTING pp plot.  
#   rclustermatrix      = return the clustermatrix (see above)
#   rlist_clusters      = return clusters as an list
#   plot_clusters       = plot the clusters with different colors
#   rconfun             = connectivity function calculation, complete
#   rconfunv            = confun without clustercomputation, takes clustermatrix as parameter
#   rclustercounts      = clustercounts with various parameters
#
#
#   graph_example()
#
#  *Implemented graphs:
#   rgeo_graph     = geometric random graph  
#   rmark_graph    = mark as geometric random graph parameter
#   rcross_graph   = mark balls cross
#   rnn_graph      = nearest neighbour graph
#   rknn_graph     = k-nearest neighbour graph
#   rmknn_graph    = mutual k-nearest neighbour graph
#   rrst_graph     = radial spanning tree
#   rmst_graph     = minimal spanning tree
#   rsig_graph     = SIG 
#   rgabriel_graph = Gabriel graph, slow R-implementation
#   
#
#
#
# TODO:
#   more more more
#
#
#
########################################################
#This is for testing 
#if(!is.loaded("graphs.so")) dyn.load("graphs.so")

########################################################
# random geometric graph with one parameter R
rgeo_graph<-function(pp,R)
# pp = point pattern as in library(spatstat)
{
    n<-pp$n
    e<-rep(0,n*n)
    result<-.C("cgeom_graph",
               x=as.numeric(as.vector(pp$x)),
               y=as.numeric(as.vector(pp$y)),
               R=as.numeric(R),
               n=as.integer(n),
               e=as.integer(as.vector(e))
               )
    matrix(result$e,ncol=n,byrow=T)
}
##########################################################
rmark_graph<-function(pp)
#random geometric graph, where the R is the mark, i.e. x~y iff ||x-y||< m(x)
# directed
{
    if(length(pp$mark)<length(pp$x)) {cat("No marks?.\n");return(-1)}
    n<-pp$n
    e<-rep(0,n*n)
    result<-.C("cmark_graph",
               x=as.numeric(as.vector(pp$x)),
               y=as.numeric(as.vector(pp$y)),
               m=as.numeric(as.vector(pp$marks)),
               n=as.integer(n),
               e=as.integer(as.vector(e))
               )
    matrix(result$e,ncol=n,byrow=T)
}

##########################################################
rcross_graph<-function(pp)
#crown cross graph, where x~y iff ||x-y||< m(x)+m(y)
#example: x~tree m(x)~tree crown radius
# compare to rmark_graph: this one is symmetric
{
    if(length(pp$mark)<length(pp$x)) {cat("No marks?.\n");return(-1)}
    n<-pp$n
    e<-rep(0,n*n)
    result<-.C("ccross_graph",
               x=as.numeric(as.vector(pp$x)),
               y=as.numeric(as.vector(pp$y)),
               m=as.numeric(as.vector(pp$marks)),
               n=as.integer(n),
               e=as.integer(as.vector(e))
               )
    matrix(result$e,ncol=n,byrow=T)
}

########################################################
rnn_graph<-function(pp)
#nearest neighbour graph
#connect x to it's nearest neighbour
#asymmetric graph e[i,j] -> nn(i)=j
{
    rknn_graph(pp,k=1)
}

########################################################
rknn_graph<-function(pp,k=1)
#nearest neighbour graph
#connect x to it's nearest neighbour
#asymmetric graph e[i,j] -> nn(i)=j
{
    n<-pp$n
    e<-rep(0,n*n)
    result<-.C("cknn_graph",
               x=as.numeric(as.vector(pp$x)),
               y=as.numeric(as.vector(pp$y)),
               n=as.integer(n),
               k=as.integer(k),
               e=as.integer(as.vector(e))
               )               
    t(matrix(result$e,ncol=n,byrow=T))
}

###########################################################
rmknn_graph<-function(pp,k)
#mutual k-nearest neighbour graph
#connect x to it's k-nearest neighbour, for whom x is also one of k-nearest neigbour
#i.e.  x~y iff. y \in knn(x) AND x \in knn(y)
#symmetric graph
{
     n<-pp$n
     e<-rep(0,n*n)
     result<-.C("cmknn_graph",
                x=as.numeric(as.vector(pp$x)),
                y=as.numeric(as.vector(pp$y)),
                n=as.integer(n),
                k=as.integer(k),
                e=as.integer(as.vector(e))
                )               
     matrix(result$e,ncol=n,byrow=T)
}

###########################################################
rrst_graph<-function(pp,x0,y0)
#radial spanning tree graph
#connect x to it's nearest neighbour inside the ball B(center=(x0,y0), r=||(x0,y0)-x||)
#
{
    n<-pp$n
    e<-rep(0,n*n)
    result<-.C("crst_graph",
               x=as.numeric(as.vector(pp$x)),
               y=as.numeric(as.vector(pp$y)),
               x0=as.numeric(x0),
               y0=as.numeric(y0),
               n=as.integer(n),
               e=as.integer(as.vector(e))
               )
    t(matrix(result$e,ncol=n,byrow=T))
}

###########################################################
rmst_graph<-function(pp)
#minimum spanning tree
#make the graph connected with miminal edge-length
#
{
    n<-pp$n
    e<-rep(0,n*n)
    result<-.C("cmst_graph",
               x=as.numeric(as.vector(pp$x)),
               y=as.numeric(as.vector(pp$y)),
               n=as.integer(n),
               e=as.integer(as.vector(e))
               )
    t(matrix(result$e,ncol=n,byrow=T))
}

###########################################################
rsig_graph<-function(pp)
{
    dists<-as.matrix(dist(cbind(pp$x,pp$y),upper=T))+diag(9999999,pp$n)
    marks<-apply(dists,1,min)
    pp<-list(x=pp$x,y=pp$y,marks=marks,n=pp$n)
    rcross_graph(pp)
}

###########################################################
rgabriel_graph<-function(pp)
{
    e<-matrix(rep(0,pp$n^2),ncol=pp$n)
    D<-as.matrix(dist(cbind(pp$x,pp$y),diag=T,upper=T))
    z<-TRUE
    for(i in 1:(pp$n-1))
        for(j in (i+1):pp$n)
        {
            pot<-as.numeric(D[i,]<=D[i,j])*as.numeric(D[j,]<=D[i,j])
            x<-min(pp$x[i],pp$x[j])+abs(pp$x[i]-pp$x[j])/2
            y<-min(pp$y[i],pp$y[j])+abs(pp$y[i]-pp$y[j])/2
            for( k in setdiff(which(pot==1),c(i,j)))
            {
                if( sqrt((pp$x[k]-x)^2+(pp$y[k]-y)^2) <= (D[i,j]/2) ) {z<-FALSE;break;}   
            }
            if(z) {e[i,j]<-1;e[j,i]<-1;}
            z<-TRUE
        }
    e
}

###########################
###########################################################
plot_graph<-function(pp, edges,arrows=0,col=1,lty=1)
# plot the edges on top of point pattern
{
    edges<-edges*(diag(-1,pp$n)+1)
    x<-y<-NULL    
    s<-NULL
    

    if(arrows==1){ 
        w<-length(pp$x)
        for (i in 1:w)
            for (j in 1:w){
                if (edges[i,j]==1 ) {
                arrows(pp$x[i],pp$y[i],pp$x[j],pp$y[j],length=0.1,col=col,lty=lty)
}
}
}    
    else{
        w<-length(pp$x)
        for(i in 1:pp$n){
            s<-edges[i,]==1
            x<-pp$x[s]
            y<-pp$y[s]
            x<-as.vector( rbind(rep(pp$x[i],length(s[s>0])),x,rep('NA',length(s[s>0])))) 
            y<-as.vector( rbind(rep(pp$y[i],length(s[s>0])),y,rep('NA',length(s[s>0])))) 
            lines(x,y,col=col,lty=lty)
}
#         for (i in 1:w)
#             for (j in 1:w){
#                 if (edges[i,j]==1 ) {
#                 lines(c(pp$x[i],pp$x[j]),c(pp$y[i],pp$y[j]),col=col,lty=lty)
#             }
#         }
}
}
#############################################################
# graph_example<-function()
# #plot one of each for random point pattern
# {
#   
#     k=4
#     par(mfrow=c(1,5))
#     pp<-list(x=runif(20),y=runif(20),n=20)
#     plot(pp,main="Geometric graph, R=0.2");plot_graph(pp,rgraph(pp,R=0.2))
#     plot(pp,main="k-nearest neighbour graph");plot_graph(pp,rknn_graph(pp,k=k))
#     plot(pp,main="k-mutual nearest neighbour graph, k=4");plot_graph(pp, rmknn_graph(pp,k=k) )
#     plot(pp,main="Radial spanning tree, (0,0)");plot_graph(pp,rrst_graph(pp,0,0))
#     plot(pp,main="Minimal spanning tree");plot_graph(pp,rmst_graph(pp))
# }

###################################################################
rclustermatrix<-function(e){
#e = symmetric neighbourhoodmatrix
#load the mighty c-object    
  if ( sum(t(e)*e)<sum(e) ) { print("We don't eat directed graphs!");return;} 
    n<-length(e[,1])
    e<-e-diag(1,n) 
    ec<-e
    result<-.C("compute_clusters",
                 e=as.integer(as.vector(e)),
                 ec=as.integer(as.vector(ec)),
                 m=as.integer(n)
                )

#     list(ec=matrix(result$ec+1,byrow=T,ncol=n),e=matrix(result$e,byrow=T,ncol=n))
    matrix(result$e,byrow=T,ncol=n)
}

###################################################################
rlist_clusters<-function(ec)
#ec= clustermatrix from rclustermatrix
#return list of clusters
{
  a<-list()
    n<-length(ec[1,])
    for(i in 1:n)
{
  z<-(1:n)[ec[,i]==1]
        for(j in setdiff(z,i))
            ec[,j]<-rep(0,n)
        if(length(z)>0) a<-append(a,list(c(z)))
}
    a
}
###################################################################
plot_clusters<-function(pp,ec,b=2,d=1)
#b= starting color
#ec=clustermatrix from rclustermatrix
#d= size of points
#if no device open, open one, otherwise plot on top
{    
  a<-rlist_clusters(ec)
    left<-(1:pp$n)
    z<-rep(0,pp$n)
    
    for(j in 1:length(a)){
  for(i in left){
  for(k in a[[j]]){
  if(i == k){
  z[i] = colors()[b+3*j]
}
}
}
} 
    if(dev.cur()==1) plot(pp,cex=d);
    points(pp$x,pp$y,col=z,pch=19,cex=d)
}

##########################################################################
rclustercounts<-function(pp, R=0, k=0, graph=0)
{
    if( graph == 0) cat("
        No graph type specified, use one of the following as parameter 'graph= ':\n
        1 geometric graph\n
        2 rmknn_graph\n
        And give a vector 'k' or 'R' for the values of interest.\n
    ")
    else{
        v<-NULL
        if ( graph == 1 && R!=0){
            for(r in R) {
                e<-rgeo_graph(pp,r)
                a<-rclustermatrix(e)
                v<-c(v, length( rlist_clusters(a) ) )
            }
            return (list(n=v,r=R))
        }
#         if ( graph == 2 && k!=0){
#             for(i in k) {
#                 e<-rknn_graph(pp,k=i)
#                 a<-rclustermatrix(e)
#                 v<-c(v, length( rlist_clusters(a) ) )
#             }
#             return (list(n=v,k=k))
#         }
         if ( graph == 2 && k!=0){
            for(i in k) {
                e<-rmknn_graph(pp,k=i)
                a<-rclustermatrix(e)
                v<-c(v, length( rlist_clusters(a) ) )
            }
            return (list(n=v,k=k))
        }
        cat(" Wrong parametertype/graph type/something...\n Fatal error 9999101 contact package administrator!\n");
    }
}








###################################################################
rconfun<-function(pp,R,h,rvec,ker=0){
##computes the connectivity function
#point pattern as in library(spatstat)
#R=geometric graph parameter
#h=parameter for the kernel
#rvec=come on...
#ker=kernel function. 0=unif square,1=epanechnikov   
# also check the partial functions underneath, the clustering is a bit slow
#returns a list with $v=values, $e=edges of R-graph, $ec=clustermatrix of R-graph
#load the mighty c-object    
#     dyn.load("confunc.so")
    e<-rgeo_graph(pp,R)
    ec<-rclustermatrix(e)
    n<-pp$n
    v<-rconfunv(pp,ec,h,rvec,ker)
    e<-matrix(e,byrow=T,ncol=n)
    list(v=v,e=e,ec=ec)
}

########################################################

rconfunv<-function(pp,ec,h,rvec,ker=0){
#just the value computation part, needs clustermatrix
#pp= powerpoint.. point process?
#e=edges of the graph
#ec=clustermatrix of the graph
#h=parameter for the kernel
#rvec=come on...
#ker=kernel function. 0=unif square,1=epanechnikov   
# also check the partial functions underneath, the clustering is a bit slow
#load the mighty c-object    
#     dyn.load("confunc.so")    
    n<-pp$n
    xlim<-pp$window$x
    ylim<-pp$window$y
    w<-rep(0,n*n)
    foo<-.C("weights",
          x=as.double(as.vector(pp$x)),
          y=as.double(as.vector(pp$y)),
          w=as.double(as.vector(w)),
          n=as.integer(n),
          xlim=as.double(as.vector(xlim)),
          ylim=as.double(as.vector(ylim))
          )
    v<-rep(0.0,length(rvec))
    dists<-as.matrix(dist(cbind(pp$x,pp$y),upper=T))
    result<-.C("confunc",
                e=as.integer(as.vector(ec)),
                n=as.integer(n),
                h=as.double(h),
                ker=as.integer(ker),
                rvec=as.double(as.vector(rvec)),
                rn=as.integer(length(rvec)),
                v=as.double(as.vector(v)),
                dists=as.double(as.vector(dists)),
                w=as.double(as.vector(foo$w))
              )
#     list(v=result$v,w=matrix(result$w,byrow=T,ncol=n))
    result$v
}

