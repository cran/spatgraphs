#######################
# weights.R
#
# set weights to edges
#######################


weight.sg<-function(x, pp, f=function(x)exp(-x^2/scale), scale=1, ...) {
	verifyclass(x, "sg")
	D<-as.matrix(dist(cbind(pp$x, pp$y), upper=T, diag=T))
	W<-f(D)
	weights<-list()
	for(i in 1:x$N)	weights[[i]]<-W[i, x$edges[[i]]]
	x$weights<-weights
	x
}
