// Copyright (C) 2015 Deltares
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


#ifndef TIMESERIESMATRIXIFACE_H
#define TIMESERIESMATRIXIFACE_H

#include <string>
#include <cmath>
#include <limits>
#include <ctime>
#include <iostream>

#include "timeSeriesBasics.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{
/**
 * @brief Interface to timeSeriesMatrix and timeSeriesSparseMatrix.
 * 
 * The underlying data structures contain timeseries values for (nSeries, nTimes).
 */
class timeSeriesMatrixInterface : public virtual timeSeriesBasics
{
public:
	/** @brief Destructor */
	virtual ~timeSeriesMatrixInterface() {}

	/** 
	 * @brief get a pointer to the state vector at t = tIndex 
	 * @param tIndex    timestep index 
	 * @returns         pointer to the state vector at t = tIndex. If the implementing class is a sparse matrix,
	 *                  an exception will be thrown if tIndex is not the current or next timestep
	 */
	virtual double* getState(int tIndex) = 0;

	/** 
	 * @brief gets value for t=tIndex and series=sIndex.
	 * @param tIndex    timestep index 
	 * @param sIndex	index of the series
	 * @returns         value at t = tIndex and series=sIndex.
	 *                  an exception may be thrown if no value can be found.
	 */
	virtual double getValue(int tIndex, int sIndex) = 0;

	/** 
	 * @brief sets value at position t=tIndex and series=sIndex
	 * @param tIndex    timestep index 
	 * @param sIndex	index of the series
	 * @param value	    value to store in the matrix
	 * @returns         true if successful
	 */
	virtual bool setValue(int tIndex, int sIndex, double value) = 0;

	/**
	 * @brief advance the timestep
	 */
	virtual void incrementTimeStep() = 0;

	/** 
	 * @brief validates matrix data
	 */
	virtual void validate(int nSeriesEnd, int nTimeStepT0) = 0;
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESMATRIXIFACE_H */
