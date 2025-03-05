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

#if _MSC_VER && _MSC_VER < 1900 
#define snprintf sprintf_s
#endif

#include "timeSeriesSparseMatrix.h"
#include "piDiagInterface.h" 
#include <stdexcept>

using namespace rtctools::timeseries;

timeSeriesSparseMatrix::timeSeriesSparseMatrix(int nTimeStep, vector<long long> time,
								   int nSeries, vector<string> seriesID,
								   vector<validationEnum> seriesValidation,
								   map<string,int> scalarIDMap,
								   map<string,pair<int,int> > vectorIDMap,
								   int nimport)
	: timeSeriesBasics(time, seriesID, seriesValidation, scalarIDMap, vectorIDMap)
{
	this->nimport = nimport;
	this->nSparse = 0;
	this->iState=0;
	this->state = NULL;
}

/** destructor */
timeSeriesSparseMatrix::~timeSeriesSparseMatrix(void){
	// limited memory option
	if (state) {
		delete []state;
	}
}

/**
 * Returns the value for given tIndex and sIndex. If tIndex matches the time index of the state, 
 * the value stored in state is returned. Otherwise, the value is interpolated or extrapolated 
 * on the fly from the import timeseries.
 */
double timeSeriesSparseMatrix::getValue(int tIndex, int sIndex) {

   double val;
   // tIndex is index in full timeseries!
   if (tIndex == (iState - 3) && iState > 1)     //state - 3
   {
      val = state[sIndex];
   }
   else if (tIndex == (iState - 2) && iState > 1) //state - 2
   {
      val = state[sIndex + nSeries];
   }
   else if (tIndex == (iState - 1) && iState > 1) //state - 1  
   {
      val = state[sIndex + 2 * nSeries];
   }
   else if (tIndex == iState && iState > 1)     //current state
   {
      val = state[sIndex + 3 * nSeries];
   }
   else
   {
      val = getInterpolatedValue(tIndex, sIndex);
   }

   return val;
}

/*
* Sets the value in state for given tIndex and sIndex
* Returns false if tIndex does not match the time index of the state.
*/
bool timeSeriesSparseMatrix::setValue(int tIndex, int sIndex, double value) 
{

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
   else
   {
      return false;
   }
   return true;
}

/*
* Stores a sparse timeseries for one series index. Only to be used during initialisation.
*/
void timeSeriesSparseMatrix::setSeries(int sIndex, 
									   vector<long long>* times, 
									   vector<double>* vals,
									   long long t1, 
									   long long t2,
									   long long dt, 
									   interpolationOption interpol, 
									   interpolationOption extrapol)
{
	assert(sIndex>=0);
	assert(sIndex<nSeries);
	sparseTimeSeries *ts = new sparseTimeSeries(sIndex,times, vals, t1, t2, dt, interpol, extrapol);
	sparseseriesvector.push_back(ts);
	nSparse = (int)sparseseriesvector.size();
}

/*
* get a pointer to the state vector at t = tIndex 
*/
double* timeSeriesSparseMatrix::getState(int tIndex) 
{
    // tIndex is index in full timeseries!
    if (tIndex == iState - 3)
    {
        return state;
    }
    if (tIndex == iState - 2)
    {
        return state + nSeries;
    }
    if (tIndex == iState - 1)
    {
        return state + 2 * nSeries;
    }
    if (tIndex == iState)
    {
        return state + 3 * nSeries;
    }

    throw runtime_error("timeSeriesSparseMatrix::getState(int tIndex) - time series index " + std::to_string(tIndex) + " is invalid. Legal indexes values are " + 
        std::to_string(iState - 3) + " " +
        std::to_string(iState - 2) + " " +
        std::to_string(iState - 1)
    );
}

void timeSeriesSparseMatrix::initState()
{
	iState = 1;
	state = new double[4 * nSeries];
	// initialization of all values with NaN
	for (int i=0; i<nSeries; i++) 
   {
	  state[i              ] = numeric_limits<double>::quiet_NaN();
	  state[i +     nSeries] = numeric_limits<double>::quiet_NaN();
      state[i + 2 * nSeries] = numeric_limits<double>::quiet_NaN();
      state[i + 3 * nSeries] = numeric_limits<double>::quiet_NaN();
	}
	// initialization for existing sparse series
	// NOTE: this is causing the differences between limited memory and full memory option
	for (int i=0; i<nSparse; i++) 
   {
		int sIndex = this->sparseseriesvector.at(i).getSeriesIndex();
        double tmp = this->getInterpolatedValue(0, sIndex);
        state[sIndex              ] = tmp;
        tmp = this->getInterpolatedValue(0, sIndex);
        state[sIndex +     nSeries] = tmp;
        tmp = this->getInterpolatedValue(0, sIndex);
        state[sIndex + 2 * nSeries] = tmp;
        tmp = this->getInterpolatedValue(1,sIndex);
		state[sIndex + 3 * nSeries] = tmp;
	}
}

void timeSeriesSparseMatrix::incrementTimeStep()
{
	if (iState==0) {throw runtime_error("timeSeriesSparseMatrix::incrementTimeStep: state was not initialized yet, no increment possible.");}
	iState++;
	assert(iState>1);
	assert(iState<nTimeStep);

	// copy of new state into old state and initialization of new state
	for (int i=0; i<nSeries; i++) {
		state[i              ] = state[i + nSeries];
		state[i + nSeries    ] = state[i + 2 * nSeries];
      state[i + 2 * nSeries] = state[i + 3 * nSeries];
      state[i + 3 * nSeries] = numeric_limits<double>::quiet_NaN();
	}

	// initialization of new state from sparse series
	for (int i=0; i<nSeries; i++) {
		if (i<nSparse) {
			// initialize value for stored sparse timeseries
			int sIndex = sparseseriesvector.at(i).getSeriesIndex();
			double newval = this->getInterpolatedValue(iState, sIndex);
			state[sIndex + 3 * nSeries] = newval;
		}
	}
}

double timeSeriesSparseMatrix::getInterpolatedValue(int tIndex, int sIndex) {

	assert(sIndex>=0);
	assert(sIndex<2*nSeries);
	assert(tIndex>=0);
	assert(tIndex<nTimeStep);

	// find sparseTimeSeries vector with corresponding series index
	int index = -1;
	int i = 0;
	while (index < 0 && i < nSparse) {
		if (sparseseriesvector.at(i).getSeriesIndex()==sIndex) {index = i;}
		i++;
	}

	if (index < 0) {
        char buffer[255];
        snprintf(buffer, sizeof(buffer), "timeSeriesSparseMatrix::getValue: no series found for index %d", sIndex);
        throw runtime_error(buffer);
    }
	sparseTimeSeries *ts = &sparseseriesvector.at(index);

	// retrieve times in sparse representation
	vector<long long> sparsetime = ts->gettimes();
	int nTimeSparse = (int)sparsetime.size();
	// time value to interpolate
	long long time = this->getTime(tIndex);
	
	double val = getValue_sparseseries(time,sparsetime,ts->getvals(),ts->getStarttime(),ts->getEndtime(),ts->getdt(),
										ts->getInterpolationOption(),ts->getExtrapolationOption());
	return val;

}

double timeSeriesSparseMatrix::getValue_sparseseries(long long time,
                                                     vector<long long> tArray,
													 vector<double> vArray,
													 long long t1, 
													 long long t2,
													 long long dt, 
													 interpolationOption intOpt,
													 interpolationOption extOpt) 
{

	// find surrounding index positions in sparse timearray
    int idx_left = find_leftindex(time,tArray);
	int nTimeSparse = vArray.size();

	long long dt_left, dt_right;
	if (idx_left<0) {
		// time[tIndex] < t_sparse[0]
		if (extOpt==BLOCK) {
			return vArray[0];
		} else if (extOpt==PERIODIC) {
			return getValue_sparseseries(time + (t2-t1+dt),tArray,vArray,t1,t2,dt,intOpt,extOpt);
		} 
	} else if (idx_left<nTimeSparse-1) {
		// t_sparse[0] < time[tIndex] < t_sparse[nTimeSparse-1]
		if (intOpt==BLOCK) {
			return vArray[idx_left];
		} else if (intOpt==LINEAR) {
			dt_left = time - tArray[idx_left];
			dt_right = tArray[idx_left+1]-time;
			return (vArray[idx_left] + (double)(dt_left)/(double)(dt_right + dt_left) * (vArray[idx_left+1]-vArray[idx_left]));
		}

	} else if (idx_left==nTimeSparse-1) {
		// t_sparse[nTimeSparse-1] == time[tIndex] < time[nTimeSteps]
		if (extOpt==BLOCK ) {
			return vArray[nTimeSparse-1];
		} else if (extOpt==PERIODIC) {
			if (time >= t2 + dt) {
				return getValue_sparseseries(time - (t2-t1+dt),tArray,vArray,t1,t2,dt,intOpt,extOpt);
			} else if (intOpt==BLOCK) {
				return vArray[nTimeSparse-1];
			} else if (intOpt==LINEAR) {
				dt_left = time - tArray[idx_left];
				dt_right = t2 + dt - time;
				return (vArray[nTimeSparse-1] + (double)(dt_left)/(double)(dt_right + dt_left) * (vArray[0]-vArray[nTimeSparse-1]));
			}
		}
	}

	// something is wrong if this point is reached.
	return numeric_limits<double>::quiet_NaN();
}

int timeSeriesSparseMatrix::find_leftindex(long long time, vector<long long> sparsetime) {

	int ileft = -1;
	int imax = sparsetime.size();
	for (int i=0 ; i < imax ; i++) {
		long long tmp = sparsetime[i];
		if ( time >= tmp) {
			ileft = i;
		}
	}
	return ileft;
}

void timeSeriesSparseMatrix::validate(int nSeriesEnd, int nTimeStepT0)
{
	// no body
}

