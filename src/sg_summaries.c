#include <math.h>
#include "spatgraphs.h"
#include "sg_dists.h"
//######################################################################
// Distance matrix computation for spatgraphs
// By Tuomas Rajala <tarajala@maths.jyu.fi>
//######################################################################
//#####################################################

void sg_clusfun(double *x,double *y, double *z,int *n, double *Rvec, int *nR, double *xlim, double *ylim, double *zlim, double *values, int *tor)
{
    int m = *n,iR,i,j,k,ni,ti,mvalid;
    double ci, *Rp;
    int A[m*m];
    double dists[m*m];
    for(iR=0;iR<*nR;iR++)
    {   
        Rp = &Rvec[iR];
        if(*tor>0) sg_dists3d_tor(x,y,z,n,xlim,ylim,zlim,dists);
        else sg_dists3d(x,y,z,n,xlim,ylim,zlim,dists);
        j=0;
        sg_geometric(x,y,z,x,n,dists,Rp,A,&j);
        values[iR]=0.0;
        mvalid=0;
        for(i=0;i<m;i++) //look at each point...
        {
            ti=0;
            for(j=0;j<(m-1);j++) //count connected neigh.pairs of i
                if(i!=j) for(k=j+1;k<m;k++){ ti = ti + A[i*m+j]*A[i*m+k]*A[j*m+k]; };
            ni=0;
            ci=0.0;
            for(j=0;j<m;j++) ni= ni + A[i*m+j]; //neighs(i)
            if(ni>1)
            {
                mvalid=mvalid+1;
                ci = (double) ti/ (0.5* ni*(ni-1)); 
                values[iR]= values[iR] + ci;//sum(ci)
            }           
        }
        if(mvalid>0) values[iR] = values[iR]/(double) mvalid; // = sum(ci)/n
        else values[iR] = 99999;
    }
}
//################################################################3
double sg_kernel(double r,double H,int *ker){
//Checked 310707
//     Kernel function, *ker: 0 => box, 1=>Epanechnikov
    float value=0.0;
    if(*ker==0){
        if(r<H) value=1.0/(2*(H));
    }
    else if(*ker==1){
        if(r<H) value= (0.75/H)*(1-r*r/(H*H));
    }
    return value;
}


// #####################################################
void sg_confun(int *e,int *m, double *h, int *ker, double *r,int *rn, double *v,double *dists,double *weights){
 /* Checked 310707
    e = clustermatrix as a list
    m = size of the pattern
    h = kernel parameter
    ker= kernel type, 0=unif,1=epanechnicov
    r = vector of r-values for the function to be calculated in
    rn= size of r
    v = size rn vector for the return values
    dists = distances of the points as vector
    weights = edgecorrection weights as a vector
    */
    int n=*m;
    double H=*h,w;
    int i,j,l;
    double k,ylaosa,alaosa;
    for(l=0;l<*rn;l++){
        alaosa=0.0;
        ylaosa=0.0;
        for(i=0;i<(n-1);i++){
            for(j=i+1;j<n;j++){
                k=sg_kernel(fabs(r[l]-dists[i*n+j]),H,ker);
                w = weights[i*n+j];
                if(w>0){
	                ylaosa = ylaosa + e[i*n+j]*k/w;
	                alaosa = alaosa + k/w;
                }
            }
        }
        v[l]=ylaosa/alaosa;
    }
}
// #####################################################
void sg_confun_weights(double *x,double *y, double *z, double *w,int *n,double *xlim,double *ylim, double *zlim){
// For a rect window xlim[] x ylim[] x zlim[]
// Checked: 191007
    int i,j;
    for(i=0;i<*n;i++){
        for(j=0;j<*n;j++){
            w[i*(*n)+j]= (double) (xlim[1]-xlim[0] - fabs(x[i]-x[j]))* (ylim[1]-ylim[0] - fabs(y[i]-y[j]))*(zlim[1]-zlim[0] - fabs(z[i]-z[j]));
        }
    }
}    

// #####################################################
int ind(double r,double d)
{
    if(0<d && d<r) return(1);
    return(0);
}
// #####################################################
void sg_cumconfun(int *e,int *m, double *r,int *rn, double *v,double *dists,double *weights, double *kest, double *ratio){
    /*
    e = clustermatrix as a list
    m = size of the pattern
    r = vector of r-values for the function to be calculated in
    rn= size of r
    v = size rn vector for the return values
    dists = distances of the points as vector
    weights = edgecorrection weights as a vector
    */
    int n=*m;
    int i,j,l;
    double k,apu,ypu;
    for(l=0;l<*rn;l++){
        ypu=0.0;    
        apu=0.0;
        for(i=0;i<(n-1);i++){
            for(j=(i+1);j<n;j++){
                  k = (double) ind(r[l],dists[i*n+j]);
                ypu = ypu + e[i*n+j] * k/weights[i*n+j];
                apu = apu + k/weights[i*n+j];
            }
        }
        v[l] = ypu;
        ratio[l] = ypu/apu;
        kest[l] = 2.0*apu;
    }
}

// #####################################################
//EOF
