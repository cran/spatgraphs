# MST operations: prune and cut. Works also for the other graphs.
# 
# Tuomas Rajala <tarajala@maths.jyu.fi>
# 070808
#############

# cut mst (or any graph for that matter...)
cut.sg<-function(x, ..., pp, R=NULL,doDists=FALSE, toroidal=FALSE, dbg=FALSE)
{
	if(is.null(R)) return(x)
	if(R<=0)return(x)
	verifyclass(x,"sg")
	pp<-sg_modify_pp(pp)
	edges<-.External("sg_cutprune_c", pp, x, 
			as.numeric(R), as.integer(doDists), 
			as.integer(toroidal), as.integer(dbg),
			as.integer(1), PACKAGE="spatgraphs")
	sg(edges,type=x$type,pars=x$parameters,note=paste("cut with R=",R,sep=""))
}


# init prune if the method does not exist
#	if(!exists("prune")) 
#	prune<-function(x,...) base::UseMethod("prune")


# prune
prune.sg<-function(x, ..., pp, level=1, doDists=FALSE, toroidal=FALSE, dbg=FALSE)
{
	if(is.null(level)) return(x)
	if(level<=0)return(x)
	verifyclass(x,"sg")
	pp<-sg_modify_pp(pp)
	x<-sg2sym(x)
	edges<-.External("sg_cutprune_c", pp, x, 
			as.numeric(level), as.integer(doDists), 
			as.integer(toroidal), as.integer(dbg), 
			as.integer(0), PACKAGE="spatgraphs")
	sg(edges,type=x$type,pars=x$parameters,note=paste("pruned with level=",as.integer(level),sep=""))
}
