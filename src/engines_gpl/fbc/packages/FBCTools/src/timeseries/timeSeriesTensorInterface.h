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

#pragma once

#ifndef TIMESERIESTENSORINTERFACE_H
#define TIMESERIESTENSORINTERFACE_H

#include "timeseries/timeSeriesInterface.h"
#include "timeseries/timeSeriesMatrixInterface.h"
#include "utilities/utils.h"
#include <vector>

using namespace std;
using namespace rtctools;

namespace rtctools
{
namespace timeseries
{
/**
 * @brief Interface to timeSeriesTensor and timeSeriesSparseTensor.
 * 
 * The underlying data structures contain timeseries values for (nEnsemble, nSeries, nTimes).
 */
class timeSeriesTensorInterface: public virtual timeSeriesInterface
{
public:
	/** @brief Destructor */
	virtual ~timeSeriesTensorInterface(){};
	/** @brief Return the renumbering of the ensembleIndx. */
	virtual vector<int> getEnsembleMap() = 0; 
	/** @brief Renumber the ensemleIndx, if wanted. */
	virtual void setEnsembleMap(vector<int> ensembleMap) = 0;
	/** @brief Return the number of ensembles */
	virtual int getNEnsemble() = 0; 
	/** @brief Return the number of timeseries */
	virtual int getNSeriesMatrix() = 0;
	/** @brief Return the value for specific index. */
	virtual double getValue(int ensembleId, int tIndex, int sIndex) = 0;
	/** @brief Set a value for specific index. */
	virtual void setValue(int ensembleId, int tIndex, int sIndex, double val) = 0;
	/** @brief Return State TODO elaborate*/
	virtual double* getState(int ensembleIndx, int timeIndx) = 0;
	/** @brief initialize State after timeseries-data is read from file */
	virtual void initState() = 0;
	/** @brief Update State, advance internal timestep */
	virtual void incrementTimeStep(int ensembleIndx) = 0;
	/** @brief Return a pointer to a 3D-Tensor with timeseries */    
	virtual double*** getValueTensor() = 0;
	/** @brief Return a pointer to the 3D-TensorObject */
	virtual double*** getObjTensor() = 0;
	/** @brief Return a pointer to a 2D-Matrix (nSeries,NTimes) with timeseries values */
    virtual timeSeriesMatrixInterface* getTimeSeriesMatrix(int ensembleIndx) = 0;
	/** @brief TODO comment */
	virtual void validate(int nSeriesEnd, int nTimeStepT0) = 0;

};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESTENSORINTERFACE_H */
