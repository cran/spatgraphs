# clustering.R
# 
# Cluster/connected component computation
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################
spatcluster<-function(x, dbg=FALSE, sym=TRUE)
{
#x = spatgraph result object 
	if(sym) x<-sg2sym(x)
	for(i in 1:length(x$edges))x$edges[[i]]<-as.integer(x$edges[[i]])
    clusters<-.External("sg_cluster", x$edges, as.integer(dbg) , PACKAGE="spatgraphs")
    sgc(clusters, x$type, x$parameters)
}



##EO clustering

