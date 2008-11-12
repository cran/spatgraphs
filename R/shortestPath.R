# shortestPath.R
# 
# Find the shortest Path between two given points in a given graph using 
# Djikstra's algorithm
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################


shortestPath<-function(pp, i, j, g=NULL, dbg=FALSE, ...)
{
	if(is.null(g)) g<-spatgraph(pp, ...)
	if(!(i%in%1:pp$n)| !(j%in%1:pp$n) | i==j) stop("Give i,j different and between 1,...,n.")
	
	e<-spatcluster(g)
	
	for(k in 1:length(e$clusters))
	{
		if(i%in%e$clusters[[k]]) break;
	}
	
	if(!(j%in%e$clusters[[k]])) return(Inf);
	
	d<-as.matrix(dist(cbind(pp$x,pp$y),upper=TRUE))
	
	cluster<-e$clusters[[k]]
	
	first<-g$edges[[i]]
	
	if(j %in%first) return(list(d=d[i,j],path=c(i,j)))
	
	
    #loop<-TRUE
	
	dists<-rep(Inf, length(cluster))
	previous<-rep(NA,length(cluster))
	ii<-which(i==cluster)
	dists[ii]<-0
	Q<-cluster
	left<-rep(TRUE,length(Q))
	
	while(sum(left)>0)
	{
		u<-which(min(dists[left])==dists)
		uu<-cluster[u]
		for(vv in g$edges[[uu]])
		{
			alt = dists[u] + d[uu,vv]
			v<-which(cluster==vv)
			
			if(alt < dists[v])
			{
				dists[v]<-alt
				previous[v]<-u
			}
			if(vv == j) left<-left=="B"
		}
		left[u]<-FALSE
		if(dbg)cat(paste("left: ",sum(left),"\r"))
	}
	if(dbg)cat("\n")
	
	path<-NULL
	jj<-which(j==cluster)
	u<-jj
	while(!is.na(previous[u]))
	{
		path<-c(path,cluster[u])
		u = previous[u]
		
	}
	path<-(rev(c(path,i)))
	path_length<-function(path,d){S<-0; for(i in 2:length(path)) S<- S + d[path[i-1],path[i]];return(S)}  
	
	
	
	return(list(d=path_length(path,d),path=path))
}

#EOF

