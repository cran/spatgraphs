# plotting.R
# 
# Plotting of graphs and clusters
#
# Author: Tuomas Rajala <tarajala@maths.jyu.fi>
###############################################################################


plot_graph<-function(pp,e,col="black",linecolor="black",pointcolor="black",bg="white",add=FALSE,lty=1,pointsize=1,arrows=FALSE,axes=TRUE,main="",labels=TRUE)
# arrows only in 2D
# plot the edges on top of a point pattern
{
    #edges<-edges*(diag(-1,pp$n)+1) #make sure diagonal == 0
    z<-x<-y<-NULL
    s<-NULL

    if(length(pp$z)==length(pp$x))  # if we have a 3D-pattern
    {
#         if(!add)
#         {
#             rgl.open()
#             rgl.bg(color=bg)
#             if(axes) axes3d(col=col,nticks=nticks)
#         }
        plot3d(pp,bg=bg, size=pointsize, col=pointcolor,add=add,axes=axes)
        for(i in 1:pp$n){
            s<-e$matrix[i,]==1
            x<-pp$x[s]
            y<-pp$y[s]
            z<-pp$z[s]
            x<-as.vector( rbind(rep(pp$x[i],sum(s)),x ))
            y<-as.vector( rbind(rep(pp$y[i],sum(s)),y ))
            z<-as.vector( rbind(rep(pp$z[i],sum(s)),z ))
            rgl.lines(x,y,z,col=linecolor,lty=lty)
        }
        title3d(main,col=pointcolor)
    }
    else
    { # 2D pattern
        if(!add){
        	plot(pp,cex=0.01)
	        points(pp,cex=pointsize,pch=19,col=pointcolor)
	    }
        if(arrows==1)
        { 
            w<-length(pp$x)
            for (i in 1:w)
                for (j in 1:w){
                    if (e$matrix[i,j]==1 ) {
                    arrows(pp$x[i],pp$y[i],pp$x[j],pp$y[j],length=0.1,col=col,lty=lty)
                    }
                }
        }    
        else
        {
            w<-length(pp$x)
            for(i in 1:pp$n)
            {
                s<-e$matrix[i,]==1
                x<-pp$x[s]
                y<-pp$y[s]
                x<-as.vector( rbind(rep(pp$x[i],sum(s)),x,rep('NA',sum(s))))
                y<-as.vector( rbind(rep(pp$y[i],sum(s)),y,rep('NA',sum(s))))
                lines(x,y,col=col,lty=lty)
            }
        }
    title(main)
    }
    
}

###########################################################
spin3d<-function(sec=10,rotspeed=1,ang=0,zoom=1,fov=60)
{
    start<-proc.time()[3]
    while( (i<- rotspeed*36*(proc.time()[3]-start))<36*sec*rotspeed){
        rgl.viewpoint(i,ang,zoom=zoom,fov=fov)
    }
}


###################################################################
plot_clusters<-function(pp,ec,pointsize=1,add=T,b=2)
#b= starting color, in colors()
#ec=clustermatrix from rclustermatrix
#d= size of points
{    
  a<-sgclusterlist(ec)
  left<-(1:pp$n)
  z<-rep(0,pp$n)
  for(j in 1:length(a))
  {
    for(i in left)
    {
        for(k in a[[j]])
        {
            if(i == k)
            {
                z[i] = colors()[b+3*j]
            }
        }
    }
  } 
  ec$matrix<-ec$matrix*0
    plot_graph(pp,ec,pointcolor=z,pointsize=pointsize,add=add)
}

## EO plotting etc.
