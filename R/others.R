shake<-function(pp, a=0.001)
{
	pp$x<-pp$x+runif(pp$n, -a,a)
	pp$y<-pp$y+runif(pp$n, -a,a)
	pp
}

#send out a mail, I use this to notify myself when long remote runs end.
mailer<-function(header, body, address="",prefix="[computation] ")  
	system(paste("echo '",body,"'|mail -s '",prefix,header,"' ",address,sep=""))

#print the used time, exec 't1<-Sys.time()' before run
took<-function(t1)
	paste("\nTook:",format(Sys.time() - t1,units="secs",digits=3),"\n")

