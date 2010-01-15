#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <vector>
#include "Pp.h"

//double getDist0(double *x, double *y, double *z, int *n, int *i, int *j, std::vector<double> *dist);
double getDist(Pp *pp, int *i, int *j, int *toroidal);
double getDist(int *i, int *j, int *n, std::vector<double> *dist);
void calcDists(Pp *pp, std::vector<double> *dist, int *toroidal);
void setDists(Pp *pp, std::vector<double> *dist, double *preDists);
int compare_doubles(const void *a, const void *b);
int Empty(double *x, double *y, int *n, int i, int j, int k, double *x00, double *y00, double *R20);
