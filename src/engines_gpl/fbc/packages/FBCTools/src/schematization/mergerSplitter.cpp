// Copyright (C) 2013 Deltares, University of Duisburg-Essen
// Institute of Hydraulic Engineering and Water Resources Management
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
 * @date 2013
 */

#include <schematization/mergerSplitter.h>

#include <limits>
#include <stdexcept>

using namespace rtctools::schematization::components;

mergerSplitter::mergerSplitter(
	string id,
	string name,
	modeEnum mode,
	INPUT iInput,
	OUTPUT iOutput) : component(id, name), rule(id, name)
{
	this->mode = mode;
	this->iInput = iInput;
	this->iOutput = iOutput;

	nInput = (int)iInput.iX.size();
	nOutput = (int)iOutput.iY.size();

	yFactorSum = 0.0;
	for (int i=0; i<nOutput; i++) {
		yFactorSum += iOutput.yFactor[i];
	}
}

void mergerSplitter::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double total = numeric_limits<double>::quiet_NaN();

	// optional nStep series
	int iStart = 0;
	if (iInput.iNStepStart>-1 && stateNew[iInput.iNStepStart]==stateNew[iInput.iNStepStart]) {
		// time series exists with valid data
		iStart = (int)stateNew[iInput.iNStepStart];
		if (iStart<0 || iStart>nInput-1) throw runtime_error("void mergerSplitter::solve(...) - error in nStep value");
	}
	int iEnd = nInput-1;
	if (iInput.iNStepEnd>-1 && stateNew[iInput.iNStepEnd]==stateNew[iInput.iNStepEnd]) {
		iEnd = (int)stateNew[iInput.iNStepEnd];
		if (iEnd<0 || iEnd>nInput-1 || iEnd<iStart) throw runtime_error("void mergerSplitter::solve(...) - error in nStep value");
	}

	double xFactorSum = 0.0;
	for (int i=iStart; i<=iEnd; i++) {
		xFactorSum += iInput.xFactor[i];
	}

	// merge inputs
	if (mode==AVERAGE) {
		total = 0.0;
		for (int i=iStart; i<=iEnd; i++) {
			total += iInput.xFactor[i] * stateOld[iInput.iX[i]] / xFactorSum;
		}
	} else if (mode==DATAHIERARCHY) {
		for (int i=iStart; i<=iEnd; i++) {
			double x = iInput.xFactor[i] * stateOld[iInput.iX[i]];
			if (x==x) {
				total = x;
				break;
			}
		}
	} else if (mode==SUM) {
		total = 0.0;
		for (int i=iStart; i<=iEnd; i++) {
			total += iInput.xFactor[i] * stateOld[iInput.iX[i]];
		}
	}

	// distribute to outputs
	for (int i=0; i<nOutput; i++) {
		stateNew[iOutput.iY[i]] = total * iOutput.yFactor[i] / yFactorSum;
	}
}

void mergerSplitter::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	// optional nStep series
	int iStart = 0;
	if (iInput.iNStepStart>-1 && stateNew[iInput.iNStepStart]==stateNew[iInput.iNStepStart]) {
		// time series exists with valid data
		iStart = (int)stateNew[iInput.iNStepStart];
		if (iStart<0 || iStart>nInput-1) throw runtime_error("void mergerSplitter::solveDer(...) - error in nStep value");
	}
	int iEnd = nInput-1;
	if (iInput.iNStepEnd>-1 && stateNew[iInput.iNStepEnd]==stateNew[iInput.iNStepEnd]) {
		iEnd = (int)stateNew[iInput.iNStepEnd];
		if (iEnd<0 || iEnd>nInput-1 || iEnd<iStart) throw runtime_error("void mergerSplitter::solveDer(...) - error in nStep value");
	}

	double xFactorSum = 0.0;
	for (int i=iStart; i<=iEnd; i++) {
		xFactorSum += iInput.xFactor[i];
	}

	// recompute total
	double total = 0.0;
	for (int i=0; i<nOutput; i++) {
		total += stateNew[iOutput.iY[i]];
	}

	// distribute to outputs
	double dTotal = 0.0;
	for (int i=0; i<nOutput; i++) {
		// stateNew[iOutput.iY[i]] = total * iOutput.yFactor[i]/yFactorSum;
		dTotal += dStateNew[iOutput.iY[i]] * iOutput.yFactor[i]/yFactorSum;
	}

	// merge inputs
	if (mode==AVERAGE) {
		for (int i=iStart; i<=iEnd; i++) {
			// total += iInput.xFactor[i] * stateOld[iInput.iX[i]] / xFactorSum;
			dStateOld[iInput.iX[i]] += dTotal * iInput.xFactor[i]/xFactorSum;
		}
	} else if (mode==DATAHIERARCHY) {
		for (int i=iStart; i<=iEnd; i++) {
			double x = stateOld[iInput.iX[i]];
			if (x==x) {
				dStateOld[iInput.iX[i]] += dTotal * iInput.xFactor[i];
				break;
			}
		}
	} else if (mode==SUM) {
		for (int i=iStart; i<=iEnd; i++) {
			// total += iInput.xFactor[i] * stateOld[iInput.iX[i]];
			dStateOld[iInput.iX[i]] += dTotal * iInput.xFactor[i];
		}
	}
}
