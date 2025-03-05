// Copyright (C) 2014 Deltares
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

/**
 * @file
 * @brief xxx
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2014
 */

#include "scenarioTree_binary.h"

#include <piDiagInterface.h>
#include <utilities/utils.h>
//#include <cmath>
#include <math.h>
#include <algorithm> 

using namespace rtctools::timeseries;
using namespace rtctools::utilities;
using namespace std;

scenarioTree_binary::scenarioTree_binary(int iReference, vector<int> iInput, vector<int> iOutput, int iProbability, scenarioTree_binary::PARAMETER par) 
	: scenarioTreeGenerator(iReference, iInput, iOutput, iProbability), par(par)
{ }

scenarioTreeGenerator::scenarioTree scenarioTree_binary::generate(timeSeriesTensorInterface* tsTensor)
{
	// get the reference ensemble for the tree generation
	vector<vector<double> > ensemble = getEnsemble(tsTensor, iReference);
	int nEnsemble = (int)ensemble.size();
	int nStep = (int)ensemble[0].size();

	// initialize p (probability of each branch) and direct (info to which branch a node is reduced to)
	vector<double> p = initP(nEnsemble);
	vector<vector<int> > direct = initDirect(nEnsemble, nStep);

	// reduction to pre-defined number of final branches, nStep is equal to the number of total time steps
	vector<double> p1 = p;
	reduction(nStep, ensemble, p1, direct, this->par.nBranch);

	// fill the root set
	branchSet root;
	root.indices = vector<int>();
	for (int i=0; i<nEnsemble; i++) {
		if (p1[i]>0.0) root.indices.push_back(i);
	}

	// additional reduction steps
	int nReduction = (int)floor(log((double)par.nBranch)/log(2.0)+0.5);
	root.split(nReduction, ensemble, direct, 0);

	// redirect all branches which are NOT in the set
	for (int i=0; i<nEnsemble; i++) {
		if (find(root.indices.begin(), root.indices.end(), i)==root.indices.end()) {
			for (int j=0; j<nStep; j++) {
				direct[i][j] = direct[direct[i][j]][j];
			}
		}
	}

	// average the ensembles over nodes which are common in all tree branches
	for (int i=0; i<(int)iInput.size(); i++) {
		average(tsTensor, direct, p, this->iInput[i], this->iOutput[i]);
	}

	// generate tree info for the optimizer
	scenarioTreeGenerator::scenarioTree st = generateScenarioTree(p1, direct);

	return st;
}

void scenarioTree_binary::branchSet::split(int nReduction, 
	vector<vector<double> >& ensemble, vector<vector<int> >& direct, int iStart)
{
	int nEns = (int)ensemble.size();
	int nStep = (int)ensemble[0].size();

	// sort and direct indices
	sort(indices.begin(), indices.end());
	for (int i=0; i<(int)indices.size(); i++) {
		for (int j=iStart; j<nStep; j++) {
			direct[indices[i]][j] = indices[0];
		}
	}

	// no further split for nReduction = 0, other continue with the split up
	if (nReduction==0) return;

	// compute averages
	vector<double> average(nEns);
	for (int i=0; i<nEns; i++) {
		average[i] = 0.0;
		for (int j=iStart; j<nStep; j++) {
			average[i] += ensemble[i][j];
		}
	}

	set1 = new branchSet;
	set1->indices = vector<int>();
	set2 = new branchSet;
	set2->indices = vector<int>();
	for (int i=0; i<(int)indices.size()/2; i++) {
		// pick half of the indices with the highest averages
		int indx = indices[0];
		for (int j=1; j<(int)indices.size(); j++) {
			if (average[indices[j]]>average[indx]) indx = indices[j];
		}
		set1->indices.push_back(indx);
		average[indx] = -1e20; // avoid that this one is picked again
	}
	// remaining ones are set2
	for (int i=0; i<(int)indices.size(); i++) {
		if (average[indices[i]]>-1e20) set2->indices.push_back(indices[i]);
	}

	set1->split(nReduction-1, ensemble, direct, (int)(iStart + (nStep-iStart)/(nReduction+1)));
	set2->split(nReduction-1, ensemble, direct, (int)(iStart + (nStep-iStart)/(nReduction+1)));
}
