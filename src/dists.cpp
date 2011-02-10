/********************************************************
 *  Distance calculation functions for spatgraphs
 *
 *
 * upper triangle matrix for distances, i=0,...,n-1, j=0,...,n-i
 * [i,j] element is in vector( n(n-1)/2) A at position
 *  A[ ( ni - i(i-1)/2 ) + j]
 *
 * Tuomas Rajala  <tuomas@sokkelo.net>
 * 150408
 *
 * ****************/

#include "dists.h"

//double getDist0(double *x, double *y, double *z, int *n, int *i, int *j, std::vector<double> *dist)
//{
//	if(*i==*j) return 0.0;
//	if(*i>*j) return getDist0(x, y, z, n, j, i, dist);
//	if(dist->size()>0)
//		return dist->at( *j-*i -1 + (int)(*i*(*n)-*i*(*i+1)/2) ) ;
//	else
//		return sqrt(pow(x[*i]-x[*j],2) + pow(y[*i]-y[*j],2)+pow(z[*i]-z[*j],2) );
//}
double fmin2(double a, double b)
{
	if(a < b) return a;
	else return b;
}

/**********************************************************************************/

double getDist(Pp *pp, int *i, int *j, int *toroidal)
{
	if(*i==*j) return 0.0;
	if(*i>*j) return getDist(pp, j, i, toroidal);
	if(*toroidal)
		return	sqrt(
					pow( fminf( pp->xlim[1]-pp->xlim[0]-fabs(pp->getX(i)-pp->getX(j)) , fabs(pp->getX(i)-pp->getX(j)) ) ,2.0F) +
					pow( fminf( pp->ylim[1]-pp->ylim[0]-fabs(pp->getY(i)-pp->getY(j)) , fabs(pp->getY(i)-pp->getY(j)) ) ,2.0F) +
					pow( fminf( pp->zlim[1]-pp->zlim[0]-fabs(pp->getZ(i)-pp->getZ(j)) , fabs(pp->getZ(i)-pp->getZ(j)) ) ,2.0F)   );
	else
		return 	sqrt(
				pow( pp->getX(i)- pp->getX(j)  ,2.0) +
				pow( pp->getY(i)- pp->getY(j)  ,2.0) +
				pow( pp->getZ(i)- pp->getZ(j)  ,2.0)   );

}
/**********************************************************************************/


double getDist(int *i, int *j, int *n, std::vector<double> *dist)
{
	if(*i==*j) return 0.0;
	if(*i>*j) return getDist(j, i, n, dist);
	return dist->at( *j-*i -1 + (int)((*i)*(*n)-(*i)*(*i+1)/2) ) ;
}
/**********************************************************************************/


void calcDists(Pp *pp, std::vector<double> *dist, int *toroidal)
{
	int i,j, n;
	double d;
	n = pp->size();
	for(i=0;i<n-1;i++)
	{
//		k = (int) i*(*n)-i*(i+1)/2;
		for(j=i+1;j<n;j++)
		{
			d = getDist(pp, &i, &j, toroidal);
			dist->push_back(d);
		}
	}
}

/**********************************************************************************/
void setDists(Pp *pp, std::vector<double> *dist, double *preDists)
{
	int i,j, n;
	double d;
	n = pp->size();
	for(i=0;i<n-1;i++)
	{
//		k = (int) i*(*n)-i*(i+1)/2;
		for(j=i+1;j<n;j++)
		{
			d = preDists[j-i -1 + (int)(i*n-i*(i+1)/2)];
			dist->push_back(d);
		}
	}
}

/**********************************************************************************/
//int compare_doubles(const void *a, const void *b)
//{
//  const double *da = (const double *) a;
//  const double *db = (const double *) b;
//
//  return (*da > *db) - (*da < *db);
//}

/**********************************************************************************/

int Empty(double *x, double *y, int *n, int i, int j, int k, double *x00, double *y00, double *R20)
// check if the circumcircle of three point triangle is empty of other points.
// puts to (x00,y00,R20) the circumcircle
// See: http://mathworld.wolfram.com/Circumcircle.html
{
	int m;
	double x0,y0,R2,bx,by,a,c,d2,xxyy1,xxyy2,xxyy3,x13,x23,x21,y13,y21,y23;
	xxyy1 = x[i]*x[i]+y[i]*y[i];
	xxyy2 = x[j]*x[j]+y[j]*y[j];
	xxyy3 =	x[k]*x[k]+y[k]*y[k];
	y23 = y[j]-y[k];
	y13 = y[i]-y[k];
	y21 = y[j]-y[i];
	x23 = x[j]-x[k];
	x13 = x[i]-x[k];
	x21 = x[j]-x[i];
	bx = -( xxyy1*y23-xxyy2*y13-xxyy3*y21 );
	by =  ( xxyy1*x23-xxyy2*x13-xxyy3*x21 );
	a  = x[i]*y23-x[j]*y13-x[k]*y21;
	c  = -(xxyy1*(x[j]*y[k]-y[j]*x[k])-xxyy2*(x[i]*y[k]-y[i]*x[k])-xxyy3*(x[j]*y[i]-x[i]*y[j]));
	R2 = (bx*bx+by*by-4.0*a*c)/(4.0*a*a);
	x0 = -bx/(2.0*a);
	y0 = -by/(2.0*a);
	x00[0] = x0;
	y00[0] = y0;
	R20[0] = R2;
	for(m=0;m<*n;m++)
	{
		if( (m!=i) & (m!=j) & (m!=k))
		{
			d2 = pow(x0-x[m],2)+pow(y0-y[m],2);
			if(d2<R2) return 0;
		}
	}
	return 1;
}
/**********************************************************************************/
