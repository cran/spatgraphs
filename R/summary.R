# summary.R
# 
# summary methods for sg and sgc
#
# Author: Tuomas Rajala <tuomas.a.rajala@jyu.fi>
###############################################################################

summary.sg<-function(object, ...)
{
   args<-list(...)
   print(object)
   degs<-sapply(object$edges, length)
   cat("\nEdge count:",sum(degs),"\n");
   cat("Isolated points:",sum(degs==0),"\n")
   cat("Symmetric:",object$symmetric,"\n")
   cat("Degree stats:\n")
   print(summary(degs))
   if("pp"%in%names(args))
   {
	   l<-sg.edgelengths(object,args$pp)
	   cat("Edge length stats:\n")
	   print(summary(l$d))
   }
}

#####################################################################################
summary.sgc<-function(object, ...)
{
	args<-list(...)
	print(object)
	cls<-sapply(object$clusters, length)
	cat("Isolated points:",sum(cls==1),"\n")
	cat("Cluster size stats:\n")
	print(summary(cls))
}

