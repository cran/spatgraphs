// Clustering functions for the spatgraph-package
// by Tuomas Rajala
// Last mod: 06.06.08
#include <R.h>
#include <Rdefines.h>
#include <math.h>
#include <R.h>
#include "Rextras.h"

extern "C" {
	//void sg_cluster(int *e,int *A, int *m, int *clusn, int *dbg)
SEXP sg_cluster(SEXP Args)
{
    int *dbg;
    std::vector<std::vector<int> > nodelist, clustlist;

    Args = CDR(Args);
	VectsxpToVector(CAR(Args),nodelist);
	Args = CDR(Args);
	dbg = INTEGER(CAR(Args));

	clustlist.resize(nodelist.size());
	int n=nodelist.size();
	int koot[n];
	int i,j,k;
	if(*dbg) printf("Clustering: ");
	for(i=0;i<n;i++) { koot[i]=0;};
	if(*dbg) printf("grouping... ");

	for(i=0;i<n;i++)
	{
		clustlist.at(i).clear();
		for(j=0;j<nodelist.at(i).size();j++)//group numbers=indices of neighbours of i //to col(i)
		{
			clustlist.at(i).push_back(nodelist.at(i).at(j)-1);//A[koot[i]*n+i] = j;
		}
	}



	int loop=1;
	i=0;
	int d=0,sizei;
	int l,s;
	int g,sizeg,h;
	if(*dbg) printf("sorting... ");
	while(loop)
	{
		sizei = clustlist.at(i).size();
		if((sizei-d)>0)//any unvisited neigh's left
		{
//			printf("\n %i:%i,%i",i,sizei,d);
			for(k=d;k<sizei;k++)//look through unvisited neigh's in column i
			{
				g=clustlist.at(i).at(k);//A[k*n+i]; //number=index of the neighbour
				//printf("\n  %i:%i,%i",i, k, g);
				sizeg=clustlist.at(g).size();
				for(j=sizeg-1; j >=0 ;j--)//starting union of col(i) U col(g) = col(i)
				{
					s=1;
					h=clustlist.at(g).at(j);//A[j*n+g]; //from rear, for .pop_back()
					if(h != i )
					{
						for(l=0;l<clustlist.at(i).size();l++)//is it new?
						{
							//                             printf(".");
							if(clustlist.at(i).at(l) == h){ s=0; }
						}
						if(s>0) //if new add to col(i)
						{
							clustlist.at(i).push_back(h);
							//printf("(%i)",h);
							//A[koot[i]*n+i]=h;
							//koot[i]++;
							//                             printf("from %i add %i->%i\n",g,h,i);
						}
					}
					clustlist.at(g).pop_back();//A[j*n+g]=-1;
				}//end of union
				clustlist.at(g).clear();//koot[g]=-1; //mark the emptied column
				d++;
			}
		}
		else {
			d=0;
			//if(clustlist.at(i).size()>0)
			clustlist.at(i).push_back(i);
			i++;
		}
		if(i>=n) loop=0;
	}//eo clustering loop

	//  for(i=0;i<n;i++)
	//    if(koot[i]>0){
	//     *clusn=*clusn+1;
	//	 for(j=0;j<koot[i];j++){
	//       for(l=j;l<koot[i];l++){
	//         e[A[j*n+i]*n+A[l*n+i]]=1;
	//         e[A[j*n+i]+A[l*n+i]*n]=1;
	//       }
	//     }
	//    }
	std::vector<std::vector<int> > reslist;
	std::vector<int>  *p;
	for(i=0;i<clustlist.size();i++)//fix the damn index shift
	{
		p = new std::vector<int> ;
		p->resize(0);
		if(clustlist.at(i).size()>0)
		{
			for(j=0;j<clustlist.at(i).size();j++)
			{
				p->push_back(clustlist.at(i).at(j)+1);
				clustlist.at(clustlist.at(i).at(j)).clear();
			}
			reslist.push_back(*p);
		}
	}
	if(*dbg) printf("done.\n");
	return vectorToSEXP(reslist);
}


} //extern
