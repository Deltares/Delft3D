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

#include "scenarioTreeGenerator.h"

#include <piDiagInterface.h>
#include <utilities/utils.h>
#include <cmath>
#include <algorithm> 
#include <stdexcept>

using namespace rtctools::timeseries;
using namespace rtctools::utilities;
using namespace std;

scenarioTreeGenerator::scenarioTreeGenerator(int iReference, vector<int> iInput, vector<int> iOutput, int iProbability) 
	: iReference(iReference), iInput(iInput), iOutput(iOutput), iProbability(iProbability)
{ }

vector<vector<double> > scenarioTreeGenerator::getEnsemble(timeSeriesTensorInterface *tsTensor, int indx)
{
	vector<vector<double> > ensemble = vector<vector<double> >();

	for (int i=0; i<tsTensor->getNEnsemble(); i++) {
		vector<double> member = vector<double>();
		for (int j=1; j<tsTensor->getNTimeStep(); j++) {
			double value = tsTensor->getValue(i, j, indx);
			if (value==value) {
				member.push_back(tsTensor->getValue(i, j, indx));
			} else {
				throw runtime_error("vector<vector<double> > scenarioTreeGenerator::getEnsemble(...) - error - NaN value detected");
			}
		}
		ensemble.push_back(member);
	}

	return ensemble;
}

void scenarioTreeGenerator::setEnsemble(timeSeriesTensorInterface *tsTensor, int indx, vector<vector<double> > ensemble)
{
	for (int i=0; i<tsTensor->getNEnsemble(); i++) {
		for (int j=1; j<tsTensor->getNTimeStep(); j++) {
			tsTensor->setValue(i, j, indx, ensemble[i][j-1]);
		}
	}
}

vector<double> scenarioTreeGenerator::initP(int nEnsemble)
{
	vector<double> p(nEnsemble);

	// assume equal likelihood of all ensemble members
	for (int i=0; i<nEnsemble; i++) {
		p[i] = 1.0/(double)nEnsemble;
	}

	return p;
}

vector<vector<int> > scenarioTreeGenerator::initDirect(int nEnsemble, int nStep)
{
	// direct provides information, if the value in a specific branch at a time step
	// originates at this location (direct[branch][time step] = branch) or
	// comes from a different branch (direct[branch][time step] != branch)
	vector<vector<int> > direct = vector<vector<int> >(nEnsemble);

	// direct all members to itself
	for (int i=0; i<nEnsemble; i++) {
		direct[i] = vector<int>(nStep);
		for (int j=0; j<nStep; j++) {
			direct[i][j] = i;
		}
	}

	return direct;
}

vector<vector<double> > scenarioTreeGenerator::computeDistanceMatrix(vector<vector<double> > ensemble, int k0, int k1)
{
	// get the number of ensemble members and allocate the distance matrix
	int nMember = (int)ensemble.size();
	vector<vector<double> > distance = vector<vector<double> >();
	for (int i=0; i<nMember; i++) {
		distance.push_back(vector<double>(nMember));
	}

	// compute the distance matrix
	for (int i=0; i<nMember; i++) {
		for (int j=0; j<nMember; j++) {
			double sum = 0.0;
			for (int k=k0; k<=k1; k++) {
				// mean average disctance between the two branches,
				// in the future we will support different, configurable metrics here
				sum += fabs(ensemble[i][k]-ensemble[j][k]);
			}
			distance[i][j] = sum;
		}
	}

	return distance;
}

void scenarioTreeGenerator::reduction(int nStep, vector<vector<double> > ensemble, vector<double>& p, vector<vector<int> >& direct, int s)
{
	int nTree = 0;
	for (int i=0; i<(int)p.size(); i++) {
		// count the remaining tree branches (ensemble branches with p>0)
		if (p[i]>0.0) nTree++;
	}
	int nEns = (int)ensemble.size();
	vector<vector<double> > distance = computeDistanceMatrix(ensemble, 0, nStep-1);
	vector<double> z = vector<double>(nEns);

	// index set of deleted scenarios
	vector<int> J = vector<int>(nTree-s);
	for (int i=0; i<nTree-s; i++) {
		J[i] = -1;
	}
	 
	// reduction steps
	for (int step=0; step<nTree-s; step++) {
		// compute the z vector
		for (int l=0; l<nEns; l++) {
			z[l] = 1e20;
			for (int j=0; j<nEns; j++) {
				// don't use the distance with itself, the ones from the set J and if p=0
				if (p[l]>0.0 && j!=l && find(J.begin(), J.end(), j)==J.end()) z[l] = min(z[l], p[l]*distance[l][j]);
			}
		}
		// select the smallest z element and add the related branch to J for deletion
		double zMin = 1e20;
		for (int l=0; l<nEns; l++) {
			if (z[l]<zMin && find(J.begin(), J.end(), l)==J.end()) {
				// replace by other index if z is smaller and not included in J
				zMin = z[l];
				J[step] = l;
			}
		}
	}

	// distribute deleted scenarios to remaining ones
	for (int i=0; i<(int)J.size(); i++) {
		// search the branch to whom we redistribute, the one with the lowest distance
		double dMin = 1e20;
		int indx = -1;
		for (int j=0; j<nEns; j++) {
			if (distance[J[i]][j]<dMin                       // smallest distance so far
				&& J[i]!=j                                   // not look at itself
				&& find(J.begin(), J.end(), j)==J.end()      // not present in J, a remaining scenario
				&& p[j]>0.0) {                               // still active, 0 if already reduced
				dMin = distance[J[i]][j];                                 
				indx = j;
			}
		}
		if (indx==-1) throw runtime_error("no branch found for redistribution");
		// redistribute probability
		p[indx] += p[J[i]];
		p[J[i]] = 0.0;
		// keep track where the branch is directed to
		for (int j=0; j<nStep; j++) {
			int toReplace = J[i];
			for (int k=0; k<nEns; k++) if (direct[k][j]==toReplace) direct[k][j] = indx;
		}
	}
}

void scenarioTreeGenerator::average(timeSeriesTensorInterface* tsTensor, vector<vector<int> >& direct, vector<double>& p, int in, int out)
{
	vector<vector<double> > x = getEnsemble(tsTensor, in);

	for (int i=0; i<(int)direct[0].size(); i++) {
		for (int j=0; j<(int)direct.size(); j++) {
			if (direct[j][i]==j) {
				// element is directed to itself, check for other elements referring to it
				double value = 0.0;
				double pSum = 0.0;
				for (int k=0; k<(int)direct.size(); k++) {
					if (direct[k][i]==j) {
						value += p[k]*x[k][i];
						pSum += p[k];
					}
				}
				value /= pSum;
				// distribute average to all elements directing to it
				for (int k=0; k<(int)direct.size(); k++) {
					if (direct[k][i]==j) {
						x[k][i] = value;
					}
				}
			}
		}
	}

	setEnsemble(tsTensor, out, x);
}

scenarioTreeGenerator::scenarioTree scenarioTreeGenerator::generateScenarioTree(vector<double> p, vector<vector<int> > direct)
{
	scenarioTreeGenerator::scenarioTree st;

	st.nBranch = 0;
	st.nEnsemble = (int)p.size();
	st.nStepBranch = (int)direct[0].size();
	st.nStepTotal = 0;
	st.indexArray = vector<vector<int> >(st.nBranch);
	st.iVec = vector<int>();
	st.pVec = vector<double>();

	map<int,int> indexMap = map<int,int>();
	for (int i=0; i<(int)p.size(); i++) {
		if (p[i]>0.0) {
			// index map
			indexMap.insert(pair<int,int>(i, st.nBranch));

			// member index and probability
			st.iVec.push_back(i);
			st.pVec.push_back(p[i]);
			st.nBranch++;

			// index array directing to itself requires a new variable
			vector<int> indexArray(st.nStepBranch);
			for (int j=0; j<st.nStepBranch; j++) {
				if (direct[i][j]==i) {
					indexArray[j] = st.nStepTotal;
					st.nStepTotal++;
				} 
			}
			st.indexArray.push_back(indexArray);
		}
	}

	// index directing to other, already existing indices
	int nBranch = 0;
	for (int i=0; i<(int)p.size(); i++) {
		if (p[i]>0.0) {
			for (int j=0; j<st.nStepBranch; j++) {
				if (direct[i][j]!=i) {
					st.indexArray[nBranch][j] = st.indexArray[indexMap[direct[i][j]]][j];
				} 
			}
			nBranch++;
		}
	}

	return st;
}


