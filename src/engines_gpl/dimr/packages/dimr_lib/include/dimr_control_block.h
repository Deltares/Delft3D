#pragma once
#include "dimr_unit.h"
// The control structure in the config.xml file is modelled as a collection of linked controlBlocks
// <control>    : 1 or more subBlocks, unit=NULL,                                 timeVars=NULL   , type=CT_SEQUENTIAL
// <parallel>   : 1 or more subBlocks, unit=NULL,                                 timeVars=NULL   , type=CT_PARALLEL
//                ASSUMPTION: first subBlock is the "master"/timeintegrator component
// <start>      : 0         subBlocks, unit.component=defined, unit.coupler=NULL, timeVars=NULL   , type=CT_START
// <startGroup> : 1 or more subBlocks, unit=NULL,                                 timeVars=defined, type=CT_STARTGROUP
// <coupler>    : 0         subBlocks, unit.component=NULL, unit.coupler=defined, timeVars=NULL   , type=CT_COUPLER
struct dimr_control_block {
	int                numSubBlocks;     // total number of sub blocks
	dimr_control_block * subBlocks;
	int                masterSubBlockId; // Identifying the unique subBlock of this controlBlock acting as the master
	int                type;
	dimr_unit          unit;             // pointer to the actual units (in case of no further subblocks)
	double             tStart;
	double             tStep;
	double             tEnd;
	double             tNext;
	double             tCur;
	vector<double>*    computeTimes;      // Ideally this should not be a pointer. But dimr_control_block is allocated using malloc.
	int                computeTimesCurrent;
}
;
