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
					   "gabriel","delaunay","MST","markcross",
					   "SIG","RST","RNG","CCC","STIR","bgeometric")
SG_GRAPH_PARS<-list(R="numeric>0",k="integer>0","",
					  k="integer>=0","","","",
					  "",list(x0="numeric",y0="numeric",z0="numeric=0"),"",type0="factor",
					   list(noise="numeric>0",alpha="numeric",beta="numeric>0",gamma="numeric>=0"), #STIR
			           R="numeric>0")
########

spatgraph<-function(pp, type="knn", par=NULL, preprocessR=0, dbg=FALSE, 
		            doDists=FALSE, preDists=NULL, preGraph=NULL, toroidal=FALSE,
					include=NULL)
{
	note<-NULL
    if(type=="delauney")type<-"delaunay" # was misspelled
	#check the pattern and parameters
	error<-sg_verify_parameters(pp,type,par,preprocessR)
	if(length(error)>1) return(stop(error))
	if(is.null(par))par<-sg_default_par(pp,type)
	pp<-sg_modify_pp(pp)
	if(toroidal)if(pp[["window"]][["type"]]!="rectangle") stop(simpleError("Toroidal version available only for a rectangular window."))
	npoints<-length(pp[["x"]])
	# if the distance matrix is given, make it upper triangle
	if(!is.null(preDists))
	{
		if(!is.matrix(preDists) | !any(diag(preDists)==0) | !any(dim(preDists)==c(npoints,npoints)) )
			stop("sg: given preDists not a symmetric nxn-matrix with diag=0.")
		preDists<- t(preDists)[!upper.tri(preDists,diag=TRUE)]
	}
	else preDists<--1
	
	# if a precalculation of geometric graph is required
	if(preprocessR>0)
	{
		note<-paste("Precalculated geometric graph with R=",preprocessR,sep="")
	}
	# if a precalculation graph is given
	if(!is.null(preGraph))
	{
		verifyclass(preGraph, "sg")
		note<-paste("Precalculated graph given (", preGraph$type, ", par=",paste(preGraph$parameters, collapse=","),")",sep="")
	}
	# the inclusion vector: which nodes to compute for. Only used for big geometric
	if(!is.null(include)){
		if(length(include)!=length(pp$x)) stop("Include vector's length must match the size of pattern.")
	}
	else include<-rep(TRUE, length(pp$x))
	#all ok!
    if(dbg) cat("Parameter verification ok\n")
	#Some special modifications to the ppp-class object 
    
	
	if(type=="RST")#TODO: put this RST check inside c for modularity
	{
		pp[["x"]]<-c(pp[["x"]],as.numeric(par[1]))
		pp[["y"]]<-c(pp[["y"]],as.numeric(par[2]))
		pp[["z"]]<-c(pp[["z"]],as.numeric(par[3]))
	}
	typei<-pmatch(type, SG_SUPPORTED_GRAPHS)-1
	
	# this is for compatibily with 'spatialsegregation'
	weightMatrix<--1.1 # no type-to-type weights given
	
    #and off we go
	edges<-vector("list",npoints)
	edges<-.External("spatgraph_c", pp, as.integer(typei), as.numeric(par), 
			preprocessR, as.integer(toroidal), as.integer(doDists), as.numeric(preDists), preGraph, 
			as.integer(include), weightMatrix, as.integer(dbg),
			PACKAGE="spatgraphs")
 
    sg(edges, type=type, pars=par, note=note)
}

#
########################################################################################
# default parameter if NULL is given
sg_default_par<-function(pp, type)
{
	lambda<-length(pp$x)/((pp$window$x[2]-pp$window$x[1])*(pp$window$y[2]-pp$window$y[1]))
	defaults<-list(R=1/sqrt(lambda), k=4, none=0, k=0, none=0, none=0, none=0, none=0, 
			c0=c(x0=0,y0=0,z0=0), none=0, type0=factor(1), pars=c(noise=0,alpha=0,beta=0,gamma=0),
			R=1/sqrt(lambda))
	i<-pmatch(type, SG_SUPPORTED_GRAPHS)
	defaults[[i]]
}
########################################################################################
# verify the parameters given to spatgraph
#
sg_verify_parameters<-function(pp,type,par,prepR)
{
	if(is.null(pp[["x"]]) || is.null(pp[["y"]])) return(simpleError("sg_verify_pars: check the pp requirements: need at least components x and y."))
	if(length(pp[["x"]])!=length(pp[["y"]])) return(simpleError("sg_verify_pars: Coordinate vectors x and y are of different length.")) 
	if(!is.null(pp[["z"]]))
		if(length(pp[["z"]])!=length(pp[["x"]]))return(simpleError("sg_verify_pars: z coordinate vector is of different length than x and y."))
	if(is.null(par))par<-sg_default_par(pp,type)
	
	if(is.na(i<-pmatch(type, SG_SUPPORTED_GRAPHS))) 
		return(simpleError(paste("sg_verify_pars: '",type,"' is not valid graph type. Pick one from:",paste(SG_SUPPORTED_GRAPHS,collapse=" "),".") ))
	par_should_be<-unlist(SG_GRAPH_PARS[i])
	if(length(par_should_be)==1)if(par_should_be=="") 	return("")	
	if(length(par)!=length(par_should_be))
		return(simpleError(
				paste("sg_par: '",type,"' graph needs par=",
				     paste("c(",paste(par_should_be,collapse=","),")"),
					 paste(" (",paste(names(par_should_be),collapse=","),")",sep=""),
				".",sep="")
			    ))
	
	if(!is(prepR,"numeric")|| length(prepR)!=1 || prepR<0)
		return(simpleError(paste("sg_verify_pars: preprocessR needs to be a positive number.")) )
	
	return("")
}
########################################################################################
sg_modify_pp<-function(pp)
{
	n<-length(pp[["x"]])
	pp[["n"]]<-n
	
	if(is.null(pp[["window"]])) pp[["window"]]<-list(xrange=range(pp[["x"]]), yrange=range(pp[["y"]]), type="rectangle")
	
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
	
	# area, only rectangle area correct
	if(is.null(pp[["area"]])) 
		pp[["area"]]<-diff(pp$window$x)*diff(pp$window$y)*diff(pp$window$z)
	
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
