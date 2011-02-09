#include <R.h>
#include "Graph.h"
#include "Pp.h"


extern "C" {

SEXP spatgraph_c(SEXP Args)
{
//.External("spatgraph_c", pp, as.integer(typei), as.numeric(par), preprocessR, as.integer(toroidal), as.integer(doDists), as.integer(dbg))

	Pp pp;
	double *prepR, *par, *preDists, *weightMatrix;
	int *gtype, *doDists, *toroidal, *dbg, *incl;
	Graph graph;
	SEXP preGraph;

//start parsing the args
	Args = CDR(Args);
	pp.Init(CAR(Args)); // init pp

	Args = CDR(Args);
	gtype = INTEGER(CAR(Args)); //what type of graph

	Args = CDR(Args);
	par = REAL(CAR(Args)); // graph par

	Args = CDR(Args);
	prepR = REAL(CAR(Args)); // if preprocessing

	Args = CDR(Args);
	toroidal = INTEGER(CAR(Args)); // if toroidal correction

	Args = CDR(Args);
	doDists = INTEGER(CAR(Args)); // if the distances are precalculated and stored

	Args = CDR(Args);
 	preDists = REAL(CAR(Args)); //possible precalculated distances

 	Args = CDR(Args);
 	preGraph = CAR(Args); //possibly precalculated edges

 	Args = CDR(Args);
	incl = INTEGER(CAR(Args)); // the point inclusion vector

	Args = CDR(Args);
	weightMatrix = REAL(CAR(Args)); // weightMatrix

	Args = CDR(Args);
	dbg = INTEGER(CAR(Args)); // if debug messages



	graph.Init(&pp, gtype, par, prepR, doDists, preDists, toroidal, incl, weightMatrix, dbg);

	if(!isNull(preGraph))
	{
		graph.setNodelist(preGraph);
	}

	graph.sg_calc();

	if(*dbg)printf("\n");
	return graph.toSEXP();
}



}
