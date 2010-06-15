/* Cut and Prune for spatgraph-objects
 *
 * Tuomas Rajala <tarajala@maths.jyu.fi>
 * 300708
 */


#include <R.h>
#include "Graph.h"
#include "Pp.h"


extern "C" {
//	edges<-.External("sg_cutprune_c", pp,x,as.numeric(R), as.integer(doDists), as.integer(toroidal), as.integer(dbg),as.integer(1))
SEXP sg_cutprune_c(SEXP Args)
{
	Pp pp;
	int *doDists, *dbg, *toroidal, *cut, i0=0;
	Graph graph;
	SEXP prepGraph;
	double *par, d0=0.0, *preDists, d1=-1.0;

	preDists = &d1;

	Args = CDR(Args);
	pp.Init(CAR(Args)); // init pp

	Args = CDR(Args);
	prepGraph = CAR(Args);

	Args = CDR(Args);
	par = REAL(CAR(Args));

	Args = CDR(Args);
	doDists = INTEGER(CAR(Args));

	Args = CDR(Args);
	toroidal = INTEGER(CAR(Args));

	Args = CDR(Args);
	dbg = INTEGER(CAR(Args));

	Args = CDR(Args);
	cut = INTEGER(CAR(Args));



	graph.Init(&pp, &i0, par, &d0, doDists, preDists, toroidal, dbg);

	if(*dbg)printf("Setting precalculated edges...");

	std::vector<std::vector<int> > prepNodelist;
	VectsxpToVector(getListElement(prepGraph,"edges"), prepNodelist);
	graph.setNodelist(&prepNodelist);

	if(*dbg)printf(" ok.\n");
	if(*cut)
		graph.sg_cut(par);
	else
		graph.sg_prune(par);

	if(*dbg)printf("\n");
	return graph.toSEXP();
}

}
