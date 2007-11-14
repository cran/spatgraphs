# clustering.R
# 
# Cluster/connected component computation
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################
sgcluster<-function(e,print_dbg=0){
#e = symmetric neighbourhoodmatrix
    if ( !e$symmetric ){
      cat("Needed to convert to a symmetric adjacency matrix.\n")
      e$matrix<-e$matrix+t(e$matrix)
      e$matrix<-ceiling(e$matrix/max(e$matrix))
    } 
    n<-length(e$matrix[,1])
    e$matrix<-e$matrix*(diag(-1,n)+1)#make sure diagonals are 0
    ec<-e$matrix
    result<-.C("sg_cluster",
                 e=as.integer(as.vector(e$matrix)),
                 ec=as.integer(as.vector(ec)),
                 m=as.integer(n),
                 n=as.integer(0),
                 print=as.integer(print_dbg),
                 PACKAGE="spatgraphs"
                )
    sgc(matrix(result$e,byrow=TRUE,ncol=n),e$type,e$pars,result$n)
}

###################################################################
sgclusterlist<-function(ec)
#ec= clustermatrix from rclustermatrix
#return list of clusters
{
  a<-list()
    n<-length(ec$matrix[1,])
    for(i in 1:n)
    {
        z<-(1:n)[ec$matrix[,i]==1]
        for(j in setdiff(z,i))
            ec$matrix[,j]<-rep(0,n)
        if(length(z)>0) a<-append(a,list(c(z)))
    }
  a
}
##EO clustering

