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

#include "timeSeriesMatrix.h"
#include <assert.h>
#include <sstream> 
#include <stdexcept>

using namespace rtctools::timeseries;
using namespace std;


timeSeriesMatrix::timeSeriesMatrix(int nTimeStep, vector<long long> time,
							       int nSeries, vector<string> seriesID, vector<validationEnum> seriesValidation,
								   map<string,int> scalarIDMap, map<string,pair<int,int> > vectorIDMap,
								   double **valueMatrix, double **objMatrix)
	: timeSeriesBasics(time, seriesID, seriesValidation, scalarIDMap, vectorIDMap)
{
	this->valueMatrix = valueMatrix;
	this->objMatrix = objMatrix;

	iState = 1;
	state = new double[4*nSeries];
	for (int i=0; i<nSeries; i++) 
    {
      state[i              ]   = valueMatrix[0][i]; 
      state[i + nSeries    ]   = valueMatrix[1][i];
      state[i + 2 * nSeries]   = valueMatrix[2][i]; 
      state[i + 3 * nSeries]   = valueMatrix[3][i]; //present state
    }
}

timeSeriesMatrix::~timeSeriesMatrix(void)
{
	delete []state;
}

double* timeSeriesMatrix::getState(int tIndex) { 
	assert(tIndex>=0);
	assert(tIndex<nTimeStep);
	// we access memory contiguously in pid controller we need to refer to state variable 
	if (tIndex == (iState - 3))
	{
		return state;
	}
    if (tIndex == (iState - 2))
    {
        return state + nSeries;
    }
    if (tIndex == (iState - 1))
    {
        return state + 2 * nSeries;
    }
    if (tIndex == iState)
    {
        return state + 3 * nSeries;
    }

    throw runtime_error("timeSeriesMatrix::getState(int tIndex) - time series index " + std::to_string(tIndex) + " is invalid. Legal indexes values are " +
        std::to_string(iState - 3) + " " +
        std::to_string(iState - 2) + " " +
        std::to_string(iState - 1)
    );
}

double* timeSeriesMatrix::getStateObj(int tIndex) { 
	assert(tIndex>=0);
	assert(tIndex<nTimeStep);
	return objMatrix[tIndex]; 
}

double** timeSeriesMatrix::getValueMatrix() { return valueMatrix; }
double** timeSeriesMatrix::getObjMatrix() { return objMatrix; }

double timeSeriesMatrix::getValue(int tIndex, int sIndex) { 
	assert(sIndex>=0);
	assert(sIndex<2*nSeries);
	assert(tIndex>=0);
	assert(tIndex<nTimeStep);
	return valueMatrix[tIndex][sIndex]; 
}

bool timeSeriesMatrix::setValue(int tIndex, int sIndex, double value) { 
	
   assert(sIndex >= 0);
   // set value matrix
   if (tIndex <= 0) // when restarting
   {
      if (tIndex == (iState - 3))
      {
         valueMatrix[0][sIndex] = value;
      }
      else if (tIndex == (iState - 2))
      {
         valueMatrix[1][sIndex] = value;
      }
      else if (tIndex == (iState - 1))
      {
         valueMatrix[2][sIndex] = value;
      }
      else if (tIndex == iState)
      {
         valueMatrix[3][sIndex] = value;
      }
   }
   else
   {
      valueMatrix[tIndex][sIndex] = value;
   }

   // set the state. This is the master variable used in pid controller, valueMatrix could be removed.
   if (tIndex == (iState - 3))
   {
	   state[sIndex] = value;
   }
   else if (tIndex == (iState - 2))
   {
	   state[sIndex + nSeries] = value;
   }
   else if (tIndex == (iState - 1))
   {
	   state[sIndex + 2 * nSeries] = value;
   }
   else if (tIndex == iState)
   {
	   state[sIndex + 3 * nSeries] = value;
   }

   return true;
}

void timeSeriesMatrix::initializeValue(double val) {
	for (int i=0; i<nTimeStep; i++) {
		for (int j=0; j<nSeries; j++) {
			valueMatrix[i][j] = val;
		}
	}
}

void timeSeriesMatrix::initializeValue(int iStart, int iEnd, double val) {
	for (int i=iStart; i<=iEnd; i++) {
		for (int j=0; j<nSeries; j++) {
			valueMatrix[i][j] = val;
		}
	}
}

void timeSeriesMatrix::initializeObj(double val) {
	for (int i=0; i<nTimeStep; i++) {
		for (int j=0; j<nSeries; j++) {
			objMatrix[i][j] = val;
		}
	}
}

void timeSeriesMatrix::initializeObj(int iStart, int iEnd, double val) {
	for (int i=iStart; i<=iEnd; i++) {
		for (int j=0; j<nSeries; j++) {
			objMatrix[i][j] = val;
		}
	}
}

void timeSeriesMatrix::incrementTimeStep()
{
	iState++;
	assert(iState>1);
	assert(iState<nTimeStep);

	// copy of new state into old state and initialization of new state
	for (int i=0; i<nSeries; i++) {
		// -3
		state[i] = state[i+nSeries];
		// -2
		state[i+nSeries] = state[i + 2 * nSeries];
		// -1
		state[i + 2 * nSeries] = state[i + 3 * nSeries];
		// get value from matrix for inputs, 0 (iState)
		state[i + 3 * nSeries] = valueMatrix[iState][i];
	}
}

void timeSeriesMatrix::validate(int nSeriesEnd, int nTimeStepT0) 
{
	stringstream ss;

	for (int i=0; i<nSeriesEnd; i++) {
		if (seriesValidation[i]==VALIDATION_STATE) {
			if (valueMatrix[0][i]!=valueMatrix[0][i]) {
				ss << "void timeSeriesMatrix::validate() - error in initial state of time series with ID = '" 
				   << seriesID[i] << "' for VALIDATION_STATE";
				throw runtime_error(ss.str().c_str());
			}
		} else if (seriesValidation[i]==VALIDATION_UPDATE) {
			for (int j=0; j<=nTimeStepT0; j++) {
				if (valueMatrix[j][i]!=valueMatrix[j][i]) {
					ss << "void timeSeriesMatrix::validate() - error in time series with ID = '"
					   << seriesID[i] << "' at time index " << j << " for VALIDATION_UPDATE";
					throw runtime_error(ss.str().c_str());
				}
			}
		} else if (seriesValidation[i]==VALIDATION_UPDATE_EXCEPT_STATE) {
			for (int j=1; j<=nTimeStepT0; j++) {
				if (valueMatrix[j][i]!=valueMatrix[j][i]) {
					ss << "void timeSeriesMatrix::validate() - error in time series with ID = '"
					   << seriesID[i] << "' at time index " << j << " for VALIDATION_UPDATE_EXCEPT_STATE";
					throw runtime_error(ss.str().c_str());
				}
			}
		} else if (seriesValidation[i]==VALIDATION_FORECAST) {
			for (int j=nTimeStepT0; j<nTimeStep; j++) {
				if (valueMatrix[j][i]!=valueMatrix[j][i]) {
					ss << "void timeSeriesMatrix::validate() - error in time series with ID = '"
					   << seriesID[i] << "' at time index " << j << " for VALIDATION_FORECAST";
					throw runtime_error(ss.str().c_str());
				}
			}
		} else if (seriesValidation[i]==VALIDATION_FORECAST_EXCEPT_T0) {
			for (int j=nTimeStepT0+1; j<nTimeStep; j++) {
				if (valueMatrix[j][i]!=valueMatrix[j][i]) {
					ss << "void timeSeriesMatrix::validate() - error in time series with ID = '"
					   << seriesID[i] << "' at time index " << j << " for VALIDATION_FORECAST_EXCEPT_T0";
					throw runtime_error(ss.str().c_str());
				}
			}
		} else if (seriesValidation[i]==VALIDATION_ALL) {
			for (int j=0; j<nTimeStep; j++) {
				if (valueMatrix[j][i]!=valueMatrix[j][i]) {
					ss << "void timeSeriesMatrix::validate() - error in time series with ID = '"
					   << seriesID[i] << "' at time index " << j << " for VALIDATION_ALL";
					throw runtime_error(ss.str().c_str());
				}
			}
		} else if (seriesValidation[i]==VALIDATION_ALL_EXCEPT_STATE) {
			for (int j=1; j<nTimeStep; j++) {
				if (valueMatrix[j][i]!=valueMatrix[j][i]) {
					ss << "void timeSeriesMatrix::validate() - error in time series with ID = '"
					   << seriesID[i] << "' at time index " << j << " for VALIDATION_ALL_EXCEPT_STATE";
					throw runtime_error(ss.str().c_str());
				}
			}
		}
	}
}
