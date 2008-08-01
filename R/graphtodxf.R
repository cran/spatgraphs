# sgtodxf.R
# 
# Write the graph information to a dxf file
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################


sg2dxf<-function(x, pp, file)
{
	if (!inherits(x, "sg"))
		stop("x should be of class sg")
	if (substr(file, nchar(file)-3, nchar(file))!=".dxf")
		file<-paste(file, ".dxf", sep="")
	

	## header of file
	## text<-"  0\nSECTION\n  2\nHEADER\n  9\n$EXTMIN\n 10\n"
	## text<-paste(text, min(x[,2]),"\n", sep="")
	## text<-paste(text, " 20\n", sep="")
	## text<-paste(text, min(x[,3]),"\n", sep="")
	## text<-paste(text, "  9\n$EXTMAX\n 10\n", sep="")
	## text<-paste(text, max(x[,2]),"\n", sep="")
	## text<-paste(text, " 20\n", sep="")
	## text<-paste(text, max(x[,3]),"\n", sep="")
	## text<-paste(text, "  0\nENDSEC\n  0\nSECTION\n", sep="")
	## text<-paste(text, "2\nTABLES\n  0\nENDSEC\n  0\n", sep="")
	## text<-paste(text, "SECTION\n  2\nBLOCKS\n  0\n", sep="")
	##text<-paste(text, "ENDSEC\n  0\nSECTION\n  2\nENTITIES\n", sep="")
	text<-paste("SECTION\n2\nENTITIES\n")
	## The main part of the file
	#The points
	for(i in 1:x$N)
	{
		text<-paste(text,"0\nPOINT\n8\n0\n10\n",pp[["x"]][i],"\n20\n",pp[["y"]][i],"\n")
		if(!is.null(pp[["z"]]))text<-paste(text,"\n30\n",pp[["z"]][i],"\n")
	}
	#The lines
	for(i in 1:x$N)
	{
		for(j in x$edges[[i]])
		{
			text<-paste(text,"0\nLINE\n8\n1\n10\n",pp[["x"]][i],"\n20\n",pp[["y"]][i],"\n")
			if(!is.null(pp[["z"]]))text<-paste(text,"\n30\n",pp[["z"]][i],"\n")
			text<-paste(text,"11\n",pp[["x"]][j],"\n21\n",pp[["y"]][j],"\n")
			if(!is.null(pp[["z"]]))text<-paste(text,"\n31\n",pp[["z"]][j],"\n")
			
		}
	}
	
	text<-paste(text, "0\nENDSEC\n  0\nEOF\n")
	
	## write the file
	cat(text, file=file)
}