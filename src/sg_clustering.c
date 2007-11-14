// Clustering functions for the spatgraph-package 
// by Tuomas Rajala
// Last mod: 14.10.07
#include <R.h>
#include <time.h>
#include <Rdefines.h>
#include <math.h>
#include <stdlib.h>
#include "spatgraphs.h"

void sg_cluster(int *e,int *A, int *m, int *clusn, int *print_sg){
  int n=*m;
  int koot[n];
  int i,j,k;
  if(*print_sg) printf("Clustering:%i ",*print_sg);
  for(i=0;i<n;i++) { koot[i]=0;};
  if(*print_sg) printf("grouping... ");
  for(i=0;i<n;i++){   //group numbers=indices of neighbours of i to col(i)
    for(j=0;j<n;j++){
      A[j*n+i]=-1;
      if(e[j*n+i] == 1){
        A[koot[i]*n+i] = j;
        koot[i]++;
      }
    }
  }        
  
  int loop=1,done=0;
  i=0;
  int d=0;
  int t=1,l,s;
  int g,koko,h;
  if(*print_sg) printf("sorting... ");  
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
    if(koot[i]>0){
     *clusn=*clusn+1;
	 for(j=0;j<koot[i];j++){
       for(l=j;l<koot[i];l++){
         e[A[j*n+i]*n+A[l*n+i]]=1;
         e[A[j*n+i]+A[l*n+i]*n]=1;
       }
     }
    }
    if(*print_sg) printf("Done.\n");
}
