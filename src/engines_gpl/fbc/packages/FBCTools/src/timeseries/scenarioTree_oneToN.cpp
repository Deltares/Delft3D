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

#include "scenarioTree_oneToN.h"

#include <piDiagInterface.h>
#include <utilities/utils.h>

using namespace rtctools::timeseries;
using namespace rtctools::utilities;
using namespace std;

scenarioTree_oneToN::scenarioTree_oneToN(int iReference, vector<int> iInput, vector<int> iOutput, int iProbability, scenarioTree_oneToN::PARAMETER par) 
	: scenarioTreeGenerator(iReference, iInput, iOutput, iProbability), par(par)
{ }

scenarioTreeGenerator::scenarioTree scenarioTree_oneToN::generate(timeSeriesTensorInterface* tsTensor)
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

	// reduction of the first par.nStep (defined as a parameter of the oneToN option) to a single branch
	vector<double> p2 = p1;
	reduction(par.nStep, ensemble, p2, direct, 1);

	// average the ensembles over nodes which are common in all tree branches
	for (int i=0; i<(int)iInput.size(); i++) {
		average(tsTensor, direct, p, this->iInput[i], this->iOutput[i]);
	}

	// generate tree info for the optimizer
	scenarioTreeGenerator::scenarioTree st = generateScenarioTree(p1, direct);

	return st;
}

