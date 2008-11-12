# MST operations: prune and cut. Works also for the other graphs.
# 
# Tuomas Rajala <tarajala@maths.jyu.fi>
# 070808
#############

cut.sg<-function(x, ...)cut_sg(x,...)
cut_sg<-function(x, pp, R=NULL,doDists=FALSE, toroidal=FALSE, dbg=FALSE)
{
	if(is.null(R)) return(x)
	if(R<=0)return(x)
	verifyclass(pp,"ppp")
	verifyclass(x,"sg")
	pp<-sg_modify_pp(pp)
	#x<-sg_to_sym(x)
	edges<-.External("sg_cutprune_c", pp,x, as.numeric(R), as.integer(doDists), as.integer(toroidal), as.integer(dbg),as.integer(1), PACKAGE="spatgraphs")
	sg(edges,type=x$type,pars=x$parameters,note=paste("cut with R=",R,sep=""))
}

if(!exists("prune")) 
	prune<-function(x,...) UseMethod("prune")

prune.sg<-function(x,...) prune_sg(x,...)
prune_sg<-function(x, pp, level=1, doDists=FALSE, toroidal=FALSE, dbg=FALSE)
{
	if(is.null(level)) return(x)
	if(level<=0)return(x)
	verifyclass(pp,"ppp")
	verifyclass(x,"sg")
	pp<-sg_modify_pp(pp)
	x<-sg_to_sym(x)
	edges<-.External("sg_cutprune_c", pp,x,as.numeric(level), as.integer(doDists), as.integer(toroidal), as.integer(dbg),as.integer(0), PACKAGE="spatgraphs")
	sg(edges,type=x$type,pars=x$parameters,note=paste("pruned with level=",as.integer(level),sep=""))
}
