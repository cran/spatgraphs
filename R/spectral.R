# spectral.R
#
# Functions for spectral clustering
#

spectral.sg<-function(x, pp, m=2, K=3, diagplot=FALSE, ...) {
	verifyclass(x, "sg")
	if(is.null(x$weights)) stop("No weights in x. Run weight.sg-function.")
	
	W<-sg2wadj(x)$matrix
	G<-diag(rowSums(W))
	L<-G-W # Laplacian
	# eigen 
	E<-eigen(L)
	l<-E$values
	chosen<-order(l)[1:m+1] # drop first 0 value
	v<-E$vectors[,chosen]
	labels<-kmeans(v, K)$cluster
	if(diagplot) {
		par(mfrow=c(1,3))
		plot(pp$x, pp$y, col=labels, xlab="x", ylab="y", main="Identified clusters", pch=19)
		plot(1:20, sort(l)[1:20], main="20 smallest eigenvalues", xlab="Number", ylab="eigenvalue", col="darkgreen", pch=19)
		
		abline(h=0, col="gray60", lty=2)
		plot(NA, NA, xlab="index", ylab="", xlim=c(0,x$N), ylim=c(0,m), yaxt="n", main="Smallest eigenvectors (not in scale)")
		axis(2, at=1:m-0.5, paste("eigen",1:m+1, sep=""), tick=FALSE)
		for(i in 1:m){
			vi<-v[,i]-mean(v[,i])
			vi<-vi/(1.1*m*max(abs(vi)))+i-0.5
			points(1:x$N, vi, col=labels)
		}
		abline(h=c(1:(m-1))); abline(h=1:m-.5, col="gray50", lty=2)
	}
	note<-paste("K-means from weighted", x$type, "(",x$par,") note:",x$note)
	list(id=labels, sgc=sgc(split(1:x$N,labels), type="spectral clustering", pars=list(m=m, K=K), note=note), v=v, l=l[chosen])
}