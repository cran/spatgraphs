/*
    Collection of C-functions for the spatgraphs-package, to be called from R.
    
    By Tuomas Rajala
    
    general:
        the functions return the matrix in their parameter as a list int *e, hence
        all calls include at least two elements: int *n  and  int *e[n*n]

    Implemented graphs:
    geometric
    k-nearest neighbours
    k-mutual nearest neighbours
    SIG
    Mark SIG graph
    RST
    MST
*/

#include <R.h>
#include <time.h>
#include <Rdefines.h>
#include <math.h>
#include <stdlib.h>
#include "sg_dists.h"
#include "sg_summaries.h"
#include "sg_clustering.h"

#ifndef MAX_DOUBLE
const double MAX_DOUBLE=999999;
#endif

// ##################################################### 
void sg_geometric(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
  if(*print_sg) printf("Computing geometric graph... ");
  int i,j;
  for(i=0;i<(*n*(*n));i++) e[i]=0;
  
  for(i=0;i<(*n-1);i++)
  {
    for(j=i+1;j<*n;j++){
      if( dists[i*(*n)+j]<pars[0] ){ e[i*(*n)+j]=e[j*(*n)+i]=1;}
      else{ e[i*(*n)+j]=e[j*(*n)+i]=0;}
    }
  }
  if(*print_sg) printf("Done.\n");
}
// ##################################################### 
void sg_mark_geometric(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
  if(*print_sg) printf("Computing mark geometric graph... ");
  int i,j;
  for(i=0;i<(*n*(*n));i++) e[i]=0;
  
  for(i=0;i<(*n-1);i++)
  {
    for(j=i+1;j<*n;j++){
      if( dists[i*(*n)+j]< m[i] ){ e[i*(*n)+j]=1;}
      else{ e[i*(*n)+j]=e[j*(*n)+i]=0;}
    }
  }
  if(*print_sg) printf("Done.\n");
}

// ##################################################### 
void sg_knn(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
  if(*print_sg) printf("Computing %inn graph... ",(int)pars[0]);
  int i,j,l;
  double dists_i[*n];
  for(i=0;i<*n;i++)
  {
    for(j=0;j<*n;j++) dists_i[j]=dists[i*(*n)+j];
    qsort( dists_i, *n, sizeof(double),compare_doubles);
    for(j=1;j<=pars[0];j++)
        for(l=0;l<*n;l++)
            if( dists_i[j] == dists[i*(*n)+l] ) e[i*(*n)+l] = 1;
  }
  if(*print_sg) printf("Done.\n");
}
// ##################################################### 
void sg_kmnn(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
    int *print_sg2,i=0,j;
    if(*print_sg) printf("Computing %imnn graph... ",(int)pars[0]);
    sg_knn(x,y,z,m,n,dists,pars,e,print_sg2);
    for(i=0;i<(*n-1);i++)
        for(j=i+1;j<*n;j++)
            e[i*(*n)+j]=e[j*(*n)+i]=e[i*(*n)+j]*e[j*(*n)+i];
    if(*print_sg) printf("Done.\n");
}

// ##################################################### 
void sg_markcross(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
    int i,j;
    double nndist_i;
    if(*print_sg) printf("Computing mark cross graph... ",(int)pars[0]);
    
    for(i=0;i<(*n-1);i++)
        for(j=i+1;j<*n;j++)
            if( dists[i*(*n)+j] < m[i]+m[j] ) e[i*(*n)+j]=e[j*(*n)+i]=1;
    if(*print_sg) printf("Done.\n");
}

// ##################################################### 
void sg_SIG(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
    int i=0,j,*print_sg2;
    double nn_i;
    if(*print_sg) printf("Computing Spheres of influence graph... ",(int)pars[0]);
    for(i=0;i<*n;i++)
    {
        nn_i=MAX_DOUBLE;
        for(j=0;j<*n;j++)
            if(i!=j) nn_i = fminf(nn_i,dists[i*(*n)+j]);
        m[i] = nn_i;
    }
    i=0;
    print_sg2=&i;
    sg_markcross(x,y,z,m,n,dists,pars,e,print_sg2);
    if(*print_sg) printf("Done.\n");
}

// ##################################################### 
void sg_RST(double *x, double *y, double *z, double *m, int *n, double *dists, double *pars, int *e,int *print_sg)
{
  if(*print_sg) printf("Computing RST graph... ");
  int i,j,k;
  double apu0,apu1,apu2,apu3;
  for(i=0;i<*n;i++)
  {
    apu0 = dists[i*(*n+1)+*n];
    apu3=MAX_DOUBLE;
    k=-1;
    for(j=0;j<*n;j++)
    {
      if(j!=i)
      {
        apu1 = dists[j*(*n+1)+*n];
        if(apu1 < apu0 )
        {
          apu2 = dists[i*(*n+1)+j];
          if( apu2 < apu3 )
          {
            apu3 = apu2;
            k = j;
          }
        }
      }
    }
    if(k>-1) e[k*(*n)+i] = 1;
  }
  if(*print_sg) printf("Done.\n");
}

// ##################################################### 
void sg_MST(double *x, double *y, double *zz, double *marks, int *n, double *dists, double *pars, int *e,int *print_sg)
{
    if(*print_sg) printf("Computing MST graph... ");
  int i,j,k,l,m,z,k0,l0;
  int done[*n],dn;
  double apu0,apu1,apu2;
  done[0] = 0;
  dn = 1;
  int left=*n-dn;
//   double sum;
  while( left > 0 )
  {
    if(*print_sg) if((left+1)%100==0) printf("MST: %i/%i         \r",left,*n);
    apu2 = MAX_DOUBLE;
    for(i=1; i<*n;i++){
      z = 1;
      apu0=apu2;
      for(j=0; j<dn;j++){
        if(i == done[j] ) {
          z=0;
          break;
        }
        apu1 = dists[i*(*n)+done[j]];
        if( apu1<apu0 ){ 
          apu0=apu1;
          k0=i;
          l0=done[j];
        }
      }
      if(z){
        if(apu0<apu2){
          apu2=apu0;
          k=k0;
          l=l0;
        }
      }
    }
//     sum+=apu2;
    done[dn] = k;
    dn++;
    left--;
    e[l*(*n)+k] = 1;
//     printf("\r");
  }
  if(*print_sg) printf("                         \nDone.\n");
}

// ##################################################### 
// EOF
