/*
    Collection of C-functions for calculation of adjacency matrices of 
    different types of random graphs for a point pattern. To be called within R
    by Tuomas Rajala
    
    general:
        the functions return the matrix in their parameter as a list int *e, hence
        all calls include at least two elements: int *n  and  int *e[n*n]

    Implemented graphs:
    geometric random graph  =  cgeom_graph
    nearest neighbour graph  = cnn_graph
    markballs cross graph = ccross_graph
    mark as geometric random graph parameter = cmark_graph
    k-nearest neighbours graph = cknn_graph
    k-mutual nearest neighbours graph = cmknn_graph
    
*/

#include <R.h>
#include <time.h>
#include <Rdefines.h>
#include <math.h>
#include <stdlib.h>
#include <stdbool.h>

int compare_doubles (const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;
     
  return (*da > *db) - (*da < *db);
}
    
    
void perkle()
{
  bool z;
  unsigned int j;
  double d=1000.0;
  char c[]="1";
  printf("j=%i ,d=%i, c=%i\n",sizeof(j),sizeof(d),sizeof(z));
}



void cgeom_graph(double *x, double *y, double *R, int *n ,int *e)
{
  int i,j;
  for(i=0;i<*n;i++)
  {
    for(j=i;j<*n;j++){
      if( (pow(x[i]-x[j],2.0)+pow(y[i]-y[j],2.0))<pow(*R,2.0) ){ e[i*(*n)+j]=e[j*(*n)+i]=1;}
      else{ e[i*(*n)+j]=e[j*(*n)+i]=0;}
    }
  }
}

void cnn_graph(double *x, double *y, int *n,int *e)
{
  int i,j,mini;
  double min,apu;
  for(i=0; i<*n;i++){
    min=99999;
    for(j=0;j<*n;j++)
    {
      if(j!=i) {apu = sqrt(pow(x[i]-x[j],2) + pow(y[i]-y[j],2));}
      else {apu = 99999;}
      e[j*(*n)+i] = 0;
      if(apu < min){ min = apu; mini=j;};
    }
    e[mini*(*n)+i] = 1;
  }
}

void cknn_graph(double *x, double *y, int *n, int *k, int *e)
{
  int i,j,t,z;
  double apu0,apu[*n],apu2[*n];
  for(i=0;i<*n;i++){
    for(j=0;j<*n;j++){
      apu[j]=999999;
      apu2[j]=apu[j];
      if(i!=j){
        apu[j] = sqrt(pow(x[i]-x[j],2) + pow(y[i]-y[j],2));
        apu2[j]= apu[j];
      }
    }
        
    qsort( apu2, *n, sizeof(double),compare_doubles);
    for(t=0; t<*k;t++) {
      for(j=0;j<*n;j++){
        if( apu2[t] == apu[j] ) e[j*(*n)+i] = 1;
      }
    }
  }
}


void cmknn_graph(double *x, double *y, int *n, int *k, int *e)
{
  cknn_graph(x,y,n,k,e);
  int i,j;
  for(i=0;i<*n;i++)
    for(j=0;j<*n;j++)
      e[j*(*n)+i] *= e[i*(*n)+j];
}


void cmark_graph(double *x, double *y, double *m, int *n, int *e)
{
  int i,j;
  for(i=0;i<*n;i++)
  {
    for(j=i;j<*n;j++){
      if( (pow(x[i]-x[j],2.0)+pow(y[i]-y[j],2.0))<pow(m[i],2.0) ){ e[i*(*n)+j]=e[j*(*n)+i]=1;}
      else{ e[i*(*n)+j]=e[j*(*n)+i]=0;}
    }
  }
}

void ccross_graph(double *x, double *y, double *m, int *n, int *e)
{
  int i,j;
  for(i=0;i<*n;i++)
  {
    for(j=i;j<*n;j++){
      if( (pow(x[i]-x[j],2.0)+pow(y[i]-y[j],2.0))<pow(m[i]+m[j],2.0) ){ e[i*(*n)+j]=e[j*(*n)+i]=1;}
      else{ e[i*(*n)+j]=e[j*(*n)+i]=0;}
    }
  }
}

void crst_graph(double *x, double *y, double *x0, double *y0, int *n, int *e)
{
  int i,j,z;
  double apu0,apu1,apu2,apu3;
  for(i=0;i<*n;i++){
    apu0 = sqrt(pow(x[i]-*x0,2) + pow(y[i]-*y0,2));
    apu3=999999;
    z=i;
    for(j=0;j<*n;j++){
      if(j!=i){
        apu1 = sqrt(pow(*x0-x[j],2) + pow(*y0-y[j],2));
        if(apu1 < apu0 ){
          apu2 = sqrt(pow(x[i]-x[j],2) + pow(y[i]-y[j],2));
//                 printf("0:%f 1:%f 2:%f\n",apu0,apu1,apu2);
          if( apu2 < apu3 ){
            apu3 = apu2;
            z = j;
          }
        }
      }
    }
    e[z*(*n)+i] = 1;
  }
}


void cmst_graph(double *x, double *y, int *n, int *e)
{
  int i,j,k,l,m,z,k0,l0;
  int done[*n],dn;
  double apu0,apu1,apu2;
  done[0] = 0;
  dn = 1;
  int left=*n-dn;
//   double sum;
  while( left > 0 ){
      if((left+1)%100==0) printf("MST: %i/%i         \r",left,*n);
    apu2 = 999999;
    for(i=1; i<*n;i++){
      z = 1;
      apu0=apu2;
      for(j=0; j<dn;j++){
        if(i == done[j] ) {
          z=0;
          break;
        }
        apu1 = sqrt( pow(x[i]-x[done[j]],2)+pow(y[done[j]]-y[i],2) );
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
  printf("                        \r");
  
}

void cgabriel_graph(double *x, double *y, int *n, int *e)
{
  printf("NOT IMPLEMENTED IN C YET!!\n");
}




/*
    Cluster calculation functions,
    to be used with R
    
     29.11.2006  Tuomas Rajala
     
    void compute_clusters:
        Takes edge matrix in *e, returns clustermatrix in *e. *A is used for grouping.
    
    
    SLOW BUT GUARANTEED RIGHT:
    void matmul_10:
        subfunction for compute_clustermatrix
    void compute_clustermatrix:
        computes cluster matrix (ref. ?), result returned in *A
    
*/


void compute_clusters(int *e,int *A, int *m){
  int n=*m;
  int koot[n];
  int i,j,k;
  for(i=0;i<n;i++) { koot[i]=0;};
 
  for(i=0;i<n;i++){   //group numbers=indices of neighbours of i to col(i)
    for(j=0;j<n;j++){
      A[j*n+i]=-1;
      if(e[j*n+i] == 1){
        A[koot[i]*n+i] = j;
        koot[i]++;
      }
    }
  }        
  printf("Clustering...\r");
  int loop=1,done=0;
  i=0;
  int d=0;
  int t=1,l,s;
  int g,koko,h;
    
  while(loop){
    if((koot[i]-d)>0){ //any unvisited neigh's left
            
      for(k=d;k<koot[i];k++){//look through unvisited neigh's in column i
//                 printf(",");
        g=A[k*n+i]; //number=index of the neighbour
        for(j=0;j<koot[g];j++){//starting union of col(i) U col(g) = col(i)
          s=1;
          h=A[j*n+g];
          if(h != i ){
            for(l=0;l<koot[i];l++){//is it new?
//                             printf(".");
              if(A[l*n+i] == h){s=0; }
            }
            if(s>0){ //if new add to col(i)
              A[koot[i]*n+i]=h;
              koot[i]++;
//                             printf("from %i add %i->%i\n",g,h,i);
            }
          }
          A[j*n+g]=-1;
        }//end of union
        koot[g]=-1; //mark the emptied column
        d++;
      }
    }
    else {
      d=0;
      if(koot[i]>=0){ A[koot[i]*n+i]=i;koot[i]++;}
      i++;
    }
    if(i>=n)
      loop=0;
  }//eo clustering loop
    
  for(i=0;i<n;i++)
    for(j=0;j<koot[i];j++){
    for(l=j;l<koot[i];l++){
      e[A[j*n+i]*n+A[l*n+i]]=1;
      e[A[j*n+i]+A[l*n+i]*n]=1;
    }
    }
    printf("                     \r");
}




void matmul_10(int *A, int *B, int *C,int *n){
  int i,j,k;
  int m=*n;
  int apu=0;
//     printf("%i\n",);
  for(i=0;i<m;i++){
    for(j=0;j<m;j++){
      apu=0;
      for(k=0;k<m;k++){
        if( A[i*m+k]*B[k*m+j]>0){ apu=1;break;}
      }
      if(apu>0) {C[j*m+i]=1;}
      else{ C[j*m+i]=0;}
//             C[i+m*j]=apu;
    }
  }
  for(i=0;i<(m*m);i++){ A[i]=C[i];}
}

void compute_clustermatrix(int *e, int *A, int *n,int *m){
  int i,j,k;
  int C[(*n)*(*n)];
  for(i=0;i<m[0];i++){
    matmul_10(A,e,C,n);
  }
}


double K(double r,double H,int *ker){
    float value=0.0;
    if(*ker==0){
        if(r<H) value=1.0/(2*(H));
    }
    else if(*ker==1){
        if(r<H) value= (0.75/H)*(1-r*r/(H*H));
    }
    else if(*ker==2){
        printf("Kernel 2 not yet implemented!!!\n");
        return 0;
    }
    return value;
}

void confunc(int *e,int *m, double *h, int *ker, double *r,int *rn, double *v,double *dists,double *weights){
    /*
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
    double H=*h;
    int i,j,l;
    double k,apu;
//     printf("%f\n",K(2.0,3));
    for(l=0;l<*rn;l++){
        apu=0.0;
        for(i=0;i<n;i++){
            for(j=i;j<n;j++){
                k=K(fabs(r[l]-dists[i*n+j]),H,ker);
                v[l] = v[l]+e[i*n+j]*k/weights[i*n+j];
                apu = apu + k/weights[i*n+j];
            }
        }
        v[l]=v[l]/apu;
    }
}

    
void weights(double *x,double *y,double *w,int *n,double *xlim,double *ylim){
    int i,j;
    int m=*n;
    for(i=0;i<m;i++){
        for(j=0;j<m;j++){
            w[i*m+j]= (double) (xlim[1]-xlim[0] - fabs(x[i]-x[j]))* (ylim[1]-ylim[0] - fabs(y[i]-y[j]));
        }
    }
}    

    
void weights2(double *x, double *y, double *w, int *n, double *xlim, double *ylim){
    int i,j;
    int m=*n;
    for(i=0;i<m;i++){
        for(j=0;j<m;j++){
            w[i*m+j]= (double) (xlim[1]-xlim[0] - fabs(x[i]-x[j]))* (ylim[1]-ylim[0] - fabs(y[i]-y[j]));
        }
    }  
}    
    

int ind(double r,double d)
{
    if(0<d && d<r) return(1);
    return(0);
}

void confuncK(int *e,int *m, double *r,int *rn, double *v,double *dists,double *weights, double *kest){
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
//     printf("%f\n",K(2.0,3));
    for(l=0;l<*rn;l++){
        ypu=0.0;    
        apu=0.0;
        for(i=0;i<n;i++){
            for(j=i;j<n;j++){
                k=(double) ind(r[l],dists[i*n+j]);
                ypu = ypu +e[i*n+j]*k/weights[i*n+j];
                apu = apu + k/weights[i*n+j];
            }
        }
        v[l]=ypu/apu;
        kest[l] = apu;
    }
}

