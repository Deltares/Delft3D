// Copyright (C) 2010 Deltares
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
 * @date 2010
 */


#include "unitDelay.h"
#include <assert.h> 
#include <stdexcept>

using namespace rtctools::schematization;

unitDelay::unitDelay(string id, string name, double nStep, int iXIn, OUTPUT iOutput)
	: component(id, name), rule(id, name)
{
	this->iXIn = iXIn;
	this->iOutput = iOutput;

	// check nStep
	if ((nStep<0.0) || (nStep>iOutput.iY.size())) {
		throw runtime_error(string("unitDelay::unitDelay(string id, string name, double nStep, int iXIn, OUTPUT iOutput) - nStep beyond output vector range, component = " + id).c_str());
	}

	if (nStep==(int)nStep) {
		// nStep is integer
		this->nStep = (int)nStep;
	} else {
		this->nStep = -1;

		if (nStep<1.0) {
			// interpolation between input x and first element of delay vector
			i1 = iXIn;
			i2 = iOutput.iY[0];
			w1 = 1.0 - nStep;
			w2 = nStep;
		} else {
			// interpolation between two elements of delay vector
			i1 = iOutput.iY[(int)nStep - 1];
			i2 = iOutput.iY[(int)nStep];
			w1 = 1.0 - (nStep - (int)nStep);
			w2 = nStep - (int)nStep;
		}
	}
}

void unitDelay::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	stateNew[iOutput.iY[0]] = stateOld[iXIn];
	for (int i=1; i<(int)iOutput.iY.size(); i++) {
		stateNew[iOutput.iY[i]] = stateOld[iOutput.iY[i-1]];
	}

	if (iOutput.iYFinal>-1) {
		if (nStep==0) {
			stateNew[iOutput.iYFinal] = stateNew[iXIn];
		} else if (nStep>0) {
			stateNew[iOutput.iYFinal] = stateNew[iOutput.iY[nStep-1]];
		} else {
			stateNew[iOutput.iYFinal] = w1*stateNew[i1] + w2*stateNew[i2];
		}
	}

	if (iOutput.iYMin>-1) {
		double yMin = stateNew[iXIn];
		for (int i=0; i<(int)iOutput.iY.size(); i++) {
			yMin = min(yMin, stateNew[iOutput.iY[i]]); 
		}
		stateNew[iOutput.iYMin] = yMin;
	}

	if (iOutput.iYMean>-1) {
		double yMean = stateNew[iXIn];
		for (int i=0; i<(int)iOutput.iY.size(); i++) {
			yMean += stateNew[iOutput.iY[i]]; 
		}
		stateNew[iOutput.iYMean] = yMean/(double)(iOutput.iY.size() + 1.0);
	}

	if (iOutput.iYMax>-1) {
		double yMax = stateNew[iXIn];
		for (int i=0; i<(int)iOutput.iY.size(); i++) {
			yMax = max(yMax, stateNew[iOutput.iY[i]]); 
		}
		stateNew[iOutput.iYMax] = yMax;
	}

	if (iOutput.iYSum>-1) {
		double ySum = stateNew[iXIn];
		for (int i=0; i<(int)iOutput.iY.size(); i++) {
			ySum += stateNew[iOutput.iY[i]]; 
		}
		stateNew[iOutput.iYSum] = ySum;
	}
}

void unitDelay::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	// TODO min, max

	if (iOutput.iYSum>-1) {
		// stateNew[iOutput.iYSum] = ySum;
		double dYSum = dStateNew[iOutput.iYSum];
		for (int i=0; i<(int)iOutput.iY.size(); i++) {
			// ySum += stateNew[iOutput.iY[i]]; 
			dStateNew[iOutput.iY[i]] += dYSum; 
		}
		// double ySum = stateNew[iXIn];
		dStateNew[iXIn] += dYSum; 
	}

	if (iOutput.iYMean>-1) {
		// stateNew[iOutput.iYMean] = yMean/(double)(iOutput.iY.size()+1.0);
		double dYMean = dStateNew[iOutput.iYMean]/(double)(iOutput.iY.size()+1.0);
		for (int i=0; i<(int)iOutput.iY.size(); i++) {
			// yMean += stateNew[iYOut[i]]; 
			dStateNew[iOutput.iY[i]] += dYMean; 
		}
		// double yMean = stateNew[iXIn];
		dStateNew[iXIn] += dYMean;
	}

	if (iOutput.iYFinal>-1) {
		if (nStep==0) {
			// stateNew[iOutput.iYFinal] = stateNew[iXIn];
			dStateNew[iXIn] += dStateNew[iOutput.iYFinal];
		} else if (nStep>0) {
			// stateNew[iOutput.iYFinal] = stateNew[iOutput.iY[nStep-1]];
			dStateNew[iOutput.iY[nStep-1]] += dStateNew[iOutput.iYFinal];
		} else {
			// stateNew[iOutput.iYFinal] = w1*stateNew[i1] + w2*stateNew[i2];
			dStateNew[i1] += dStateNew[iOutput.iYFinal] * w1;
			dStateNew[i2] += dStateNew[iOutput.iYFinal] * w2;
		}
	}

	for (int i=(int)iOutput.iY.size()-1; i>0; i--) {
		// stateNew[iOutput.iY[i]] = stateOld[iOutput.iY[i-1]];
		dStateOld[iOutput.iY[i-1]] += dStateNew[iOutput.iY[i]];
	}
	// stateNew[iOutput.iY[0]] = stateOld[iXIn];
	dStateOld[iXIn] += dStateNew[iOutput.iY[0]];
}
