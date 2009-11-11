# graphs.R
# 
# Spatial graph calculation function for package spatgraphs
# For point patterns in R^2 or R^3
# 
# 
# Author: Tuomas Rajala  <tarajala@maths.jyu.fi>
# 060608
########################################################################################

SG_SUPPORTED_GRAPHS<-c("geometric","knn","mass_geometric",
					   "gabriel","delauney","MST","markcross",
					   "SIG","RST","RNG","CCC","STIR")
SG_GRAPH_PARS<-list(R="numeric>0",k="integer>0","",
					  k="integer>=0","","","",
					  "",list(x0="numeric",y0="numeric",z0="numeric=0"),"",type0="factor",
					   list(noise="numeric>0",alpha="numeric",beta="numeric>0",gamma="numeric>=0") #STIR
			       )
########

spatgraph<-function(pp, type="knn", par=NULL, preprocessR=0, dbg=FALSE, doDists=FALSE, preDists=NULL, toroidal=FALSE)
{
    #check conditions etc
	if(is.null(par))par<-sg_default_par(pp,type)
	error<-sg_verify_parameters(pp,type,par,preprocessR)
	if(length(error)>1) return(stop(error))
	
	npoints<-length(pp[["x"]])
	# if the distance matrix is given, make it upper triangle
	if(!is.null(preDists))
	{
		if(!is.matrix(preDists) | !any(diag(preDists)==0) | !any(dim(preDists)==c(npoints,npoints)) )
			stop("sg: given preDists not a distance matrix (diag=0, nxn) ")
		preDists<- t(preDists)[!upper.tri(preDists,diag=TRUE)]
	}
	else preDists<--1
	
	#all ok!
    if(dbg) cat("Parameter verification ok\n")
	#Some special modifications to the ppp-class object 
    pp<-sg_modify_pp(pp)
	if(type=="RST")#TODO: put this RST check inside c for modularity
	{
		pp[["x"]]<-c(pp[["x"]],as.numeric(par[1]))
		pp[["y"]]<-c(pp[["y"]],as.numeric(par[2]))
		pp[["z"]]<-c(pp[["z"]],as.numeric(par[3]))
	}
	typei<-which(SG_SUPPORTED_GRAPHS==type)-1
	
    #and off we go
	edges<-vector("list",npoints)
	edges<-.External("spatgraph_c", pp, as.integer(typei), as.numeric(par), 
			preprocessR, as.integer(toroidal), as.integer(doDists), as.numeric(preDists), as.integer(dbg), 
			PACKAGE="spatgraphs")
 
    sg(edges,type=type,pars=par)
}

#
########################################################################################
# default parameter if NULL is given
sg_default_par<-function(pp, type)
{
	lambda<-pp$n/((pp$window$x[2]-pp$window$x[1])*(pp$window$y[2]-pp$window$y[1]))
	defaults<-list(R=1/sqrt(lambda), k=4, none=0, k=0, none=0, none=0, none=0, none=0, 
			c0=c(x0=0,y0=0,z0=0), none=0, type0=factor(1), pars=c(noise=0,alpha=0,beta=0,gamma=0))
	i<-which(SG_SUPPORTED_GRAPHS==type)
	defaults[[i]]
}
########################################################################################
# verify the parameters given to spatgraph
#
sg_verify_parameters<-function(pp,type,par,prepR)
{
	#SUPPORTED_GRAPHS<-c("geometric","mark_geometric","knn","kmnn","markcross","SIG","RST","MST","CCC","gabriel","delauney","STIR","convexnn","RNG")
	
	
	
	if(is.null(pp[["window"]]) || is.null(pp[["x"]]) || is.null(pp[["y"]])) return(simpleError("sg_verify_pars: check the pp requirements: need components x, y and window."))
	if(is.null(par))par<-sg_default_par(pp,type)
	if(length(pp[["x"]])<2 || length(pp[["x"]])!=length(pp[["y"]])) return(simpleError("sg_verify_pars: not a proper point pattern."))
	if(!(type%in% SG_SUPPORTED_GRAPHS)) 
		return(simpleError(paste("sg_verify_pars: sg_type:'",type,"' is not valid graph type. Pick one from:",paste(SG_SUPPORTED_GRAPHS,collapse=" "),".") ))
	if(!is(prepR,"numeric")|| length(prepR)!=1 || prepR<0)
		return(simpleError(paste("sg_verify_pars: sg_preprocessR needs to be a positive number.")) )
	
	
	par_should_be<-unlist(SG_GRAPH_PARS[which(type==SG_SUPPORTED_GRAPHS)])
	if(length(par_should_be)==1)if(par_should_be=="") 	return("")	
	if(length(par)!=length(par_should_be))
		return(simpleError(
				paste("sg_par: '",type,"' graph needs par=",
				     paste("c(",paste(par_should_be,collapse=","),")"),
					 paste(" (",paste(names(par_should_be),collapse=","),")",sep=""),
				".",sep="")
			    ))
	return("")
}
########################################################################################
sg_modify_pp<-function(pp)
{
	n<-length(pp[["x"]])
	if(length(pp[["mass"]]) < n ) # set the masses
	{
		if(length(pp[["marks"]])< n | !is.numeric(pp[["marks"]])) pp$mass<-rep(1.0,n)
		else pp$mass<-pp$marks
	}
	if(length(pp[["types"]]) < n) # set the types
	{
		if( (is.factor(pp$marks) | is.integer(pp$marks)) & length(pp[["marks"]])==n ) pp$types<-pp$marks 
		else pp$types<-rep(1,n)
	}
	pp$mass<-as.numeric(pp$mass)
	pp$types<-as.integer(pp$types)
	
	if(is.null(pp[["z"]]) || length(pp[["z"]])!=length(pp[["x"]])) pp$z<-rep(0.0,n) # if 2D only
	if(is.null(pp[["window"]][["z"]])) pp$window$z<-as.numeric(c(0.0,1.0)) # if 2D only
	pp$marks<-NULL
	pp$window$x<-as.numeric(pp$window$x)
	pp$window$y<-as.numeric(pp$window$y)
	pp$window$z<-as.numeric(pp$window$z)
	pp$x<-as.numeric(pp$x)
	pp$y<-as.numeric(pp$y)
	pp$z<-as.numeric(pp$z)
	pp
}

########################################################################################
#eof
