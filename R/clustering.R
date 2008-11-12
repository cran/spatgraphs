# clustering.R
# 
# Cluster/connected component computation
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################
spatcluster<-function(e,dbg=FALSE, sym=TRUE){
#e = spatgraph result object 
	if(sym) e<-sg_to_sym(e)
	for(i in 1:length(e$edges))e$edges[[i]]<-as.integer(e$edges[[i]])
    clusters<-.External("sg_cluster", e$edges, as.integer(dbg) , PACKAGE="spatgraphs")
    sgc(clusters,e$type,e$parameters)
}



##EO clustering

