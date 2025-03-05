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

#ifndef TIMESERIESMATRIX_H
#define TIMESERIESMATRIX_H

#include <string>
#include <cmath>
#include <limits>
#include <ctime>
#include <iostream>

#include "timeSeriesMatrixInterface.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{

class timeSeriesMatrix : public timeSeriesMatrixInterface
{
private:
	double **valueMatrix;
	double **objMatrix;
	int iState;
	double *state;

public:
	timeSeriesMatrix(
			int nTimeStep, vector<long long> time,
			int nSeries, vector<string> seriesID, vector<validationEnum> seriesValidation,
			map<string,int> scalarIDMap, map<string,pair<int,int> > vectorIDMap,
			double **valueMatrix, double **objMatrix);
	virtual ~timeSeriesMatrix(void);

	double* getState(int tIndex);
	double* getStateObj(int tIndex);
	double** getValueMatrix();
	double** getObjMatrix();
	double getValue(int tIndex, int sIndex);
	bool setValue(int tIndex, int sIndex, double value);
	void initializeValue(double val);
	void initializeValue(int startIndex, int endIndex, double val);
	void initializeObj(double val);
	void initializeObj(int startIndex, int endIndex, double val);
	void incrementTimeStep();
	void validate(int nSeriesEnd, int nTimeStepT0);
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESMATRIX_H */
