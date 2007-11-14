# graphs.R
# 
# Main graph calculation functions for package spatgraphs
#
# Author: Tuomas Rajala  <tarajala@maths.jyu.fi>
###############################################################################

spatgraph<-function(pp, type="", pars=list(), toroidal.correction=FALSE,print_dbg=FALSE)
{
    #check conditions etc
    SUPPORTED_GRAPHS<-c("geometric","mark_geometric","knn","kmnn","markcross","SIG","RST","MST","CCC","gabriel")
    GRAPH_PARS<-list("R","","k","k","","","x,y,z","","m","")
    issym<-c(1,1,0,1,1,1,0,0,0,1)
    if(!(type%in% SUPPORTED_GRAPHS)) 
        return ( cat(paste("Error sg_type:'",type,"' is not valid graph type. Pick one from:",paste(SUPPORTED_GRAPHS,collapse=" "),"\n")) )
    pars_should_be<-GRAPH_PARS[[which(type==SUPPORTED_GRAPHS)]]
    if(is.null(names(pars)))
    {
        if(!pars_should_be=="")
            return ( cat(paste("Error sg_par:",type,"graph takes parameters '",pars_should_be,"'.\n")) )
        else pars<-0
    } else if(!paste(names(pars),collapse=",")==pars_should_be)
            return ( cat(paste("Error sg_par:",type,"graph takes parameters '",pars_should_be,"', you gave '",paste(names(pars),collapse=","),"'.\n") ))
    if(is.null(pp$window)) return( cat(paste("Error sg: no window specified.")))
#Enough checking, let's get on with it.
    npoints<-pp$n
    if(type=="RST"){pp$x<-c(pp$x,pars$x);pp$y<-c(pp$y,pars$y);pp$z<-c(pp$z,pars$z);pp$n<-pp$n+1}#trick?
    dists<-sg_dists(pp,tor=toroidal.correction)
    if(is.null(pp$z)){pp$z<-pp$x*0;pp$window$z<-c(0,1)}
    if(type=="CCC") return(ccc_graph(pp,pars$m,dists,toroidal.correction,type,pars))# c TODO
    if(type=="gabriel") return(gabriel_graph(pp,dists,type,pars)) #c TODO
    if(length(pp$m)<length(pp$x)) pp$m<-0*pp$x
    graphf<-paste("sg_",type,sep="")
    edges<-diag(0,npoints)
    res<-.C(graphf,
          x=as.double(as.vector(pp$x)),
          y=as.double(as.vector(pp$y)),
          z=as.double(as.vector(pp$z)),
          m=as.double(as.vector(as.numeric(pp$m))),
          n=as.integer(npoints),
          dists = as.double(as.vector(dists)),
          pars = as.double(pars),
          edges = as.integer(as.vector(edges)),
          p = as.integer(print_dbg),
          PACKAGE="spatgraphs"
         )
    sg(matrix(res$edges,ncol=npoints),type=type,pars=pars,sym=as.logical(issym[which(SUPPORTED_GRAPHS==type)]) )
}


##EO main graph function
###########################################################
#Begin TODO need-to-convert-to-c graphs 
#TODO EI TOIMI gabriel
###########################################################
gabriel_graph<-function(pp,D,type,pars)
{
    e<-diag(0,pp$n)
    s<-TRUE
    for(i in 1:(pp$n-1))
        for(j in (i+1):pp$n)
        {
            pot<-as.numeric(D[i,]<=D[i,j])*as.numeric(D[j,]<=D[i,j])
            x<-min(pp$x[i],pp$x[j])+abs(pp$x[i]-pp$x[j])/2
            y<-min(pp$y[i],pp$y[j])+abs(pp$y[i]-pp$y[j])/2
            z<-min(pp$z[i],pp$z[j])+abs(pp$z[i]-pp$z[j])/2
            
            for( k in setdiff(which(pot==1),c(i,j)))
            {
                if( sqrt((pp$x[k]-x)^2+(pp$y[k]-y)^2+(pp$z[k]-z)^2) <= (D[i,j]/2) ) {s<-FALSE;break;}   
            }
            if(s) {e[i,j]<-1;e[j,i]<-1;}
            s<-TRUE
        }
    sg(e,type,pars,sym=TRUE)
}

###########################################################
ccc_graph<-function(pp,m0,dists,tor,type,pars)  #TODO 
#Class cover catch digraph for bivariate pp
# see Marchette p.132
{
    N<-pp$n
    m<-0*pp$m
    for( i in (1:N)[pp$m==m0])
    {
        m[i]<-min(dists[i,pp$m!=m0] )
    }
    pp$m<-m
    e<-spatgraph(pp,type="mark_geometric",toroidal.correction=tor)
    e$type<-"CCC"
    e
}

## EO need-to-convert-to-c graphs
###########################################################


mst_cut<-function(pp=NULL,dists=NULL,mst=NULL,level=0.975,R=NULL,tor=0)
{
    if(is.null(dists))
        if(is.null(pp)){
            cat("Provide pp or distance matrix!\n");
            return('Error with cut_mst');
        }
        else {dists<-sg_dists(pp,tor)}
    
    if(is.null(mst)) mst<-spatgraph(pp,type="MST")
    if(is.null(R)){
    l<-dists[mst$matrix==1]
    ql<-quantile(l,level,names=F)
}
    else{ ql<-R }
    mst$matrix<-mst$matrix*as.numeric(dists<ql)
    mst
}
####################################################
mst_prune<-function(mst=NULL,pp=NULL,level=1)
{
    if(is.null(mst)) if(!is.null(pp)) mst<-spatgraph(pp,type="MST")
    else {print("Provide either mst or pp!");return("Prune mst failed.")}
    mst$matrix<-matrix(as.numeric(mst$matrix+t(mst$matrix) > 0),ncol=dim(mst$matrix)[1])
    rs<-rowSums(mst$matrix)
    left<-which(rs<2)
    while(length(left)>0)
{
        i0<-left[1]
        k<-1
        i<-i0
        j<-which(mst$matrix[i0,]==1)
        while(rs[j]<3)
{
            k<-k+1
            i<-c(i,j)
            j<-setdiff(which(mst$matrix[j,]==1),i)
}
        i<-c(i,j)
        if(k<=level) { mst$matrix[i,i]<-mst$matrix[i,i]<-0 }
        left<-setdiff(left,i0)
}
    mst
}
