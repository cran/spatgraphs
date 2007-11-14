// Distance matrix computation for spatgraphs
// By Tuomas Rajala <tarajala@maths.jyu.fi>
//######################################################################

#include <math.h>

int compare_doubles (const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;
     
  return (*da > *db) - (*da < *db);
}
//######################################################################
void sg_dists(double *x, double *y, double *z, int *n, double *xlim, double *ylim, double *zlim, double *dists)
{
  int i,j; 
  double d;
  for(i=0;i<(*n-1);i++)
    for(j=i+1;j<*n;j++)
    {
       d = sqrtf( pow(x[i]-x[j],2.0)+pow(y[i]-y[j],2.0) ) ;
       dists[i*(*n)+j]=dists[j*(*n)+i]=d;
    }
    //done
}
//######################################################################
void sg_dists3d(double *x, double *y, double *z, int *n, double *xlim, double *ylim, double *zlim, double *dists)
{
  int i,j; 
  double d;
  for(i=0;i<(*n-1);i++)
    for(j=i+1;j<*n;j++)
    {
       d = sqrtf( pow(x[i]-x[j],2.0)+pow(y[i]-y[j],2.0) +pow(z[i]-z[j],2.0) ) ;
       dists[i*(*n)+j]=dists[j*(*n)+i]=d;
    }
    //done
}
//######################################################################
void sg_dists_tor(double *x, double *y, double *z, int *n, double *xlim, double *ylim, double *zlim, double *dists)
{
  int i,j; 
  double d;
  for(i=0;i<(*n-1);i++)
    for(j=i+1;j<*n;j++)
    {
       d = sqrtf(pow( fminf( xlim[1]-xlim[0]-fabs(x[i]-x[j]) , fabs(x[i]-x[j]) ) ,2.0)+pow( fminf( ylim[1]-ylim[0]-fabs(y[i]-y[j]) , fabs(y[i]-y[j]) ) ,2.0) );
       dists[i*(*n)+j]=dists[j*(*n)+i]=d;
    }
    //done
}
//######################################################################
void sg_dists3d_tor(double *x, double *y, double *z, int *n, double *xlim, double *ylim, double *zlim, double *dists)
{
  int i,j; 
  double d;
  for(i=0;i<(*n-1);i++)
    for(j=i+1;j<*n;j++)
    {
       d = sqrtf( pow( fminf( xlim[1]-xlim[0]-fabs(x[i]-x[j]) , fabs(x[i]-x[j]) ) ,2.0)+pow( fminf( ylim[1]-ylim[0]-fabs(y[i]-y[j]) , fabs(y[i]-y[j]) ) ,2.0)+pow( fminf( zlim[1]-zlim[0]-fabs(z[i]-z[j]) , fabs(z[i]-z[j]) ) ,2.0) );
       dists[i*(*n)+j]=dists[j*(*n)+i]=d;
    }
    //done
}
