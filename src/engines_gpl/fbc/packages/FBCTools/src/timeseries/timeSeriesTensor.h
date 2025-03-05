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


#pragma once

#ifndef TIMESERIESTENSOR_H
#define TIMESERIESTENSOR_H

#include "timeseries/timeSeriesTensorInterface.h"
#include "timeseries/timeSeriesMatrix.h"
#include "utilities/utils.h"

using namespace std;
using namespace rtctools;

namespace rtctools
{
namespace timeseries
{

 /**
 * @brief Data structure to store the tensor containing fully interpolated 
 * timeseries values for (nEnsemble, nSeries, nTimes)
 * 
 * Timeseries are given in input file timeseries_import.xml. Interpolation 
 * of the timeseries is performed during initialization.
 */
class timeSeriesTensor : public timeSeriesTensorInterface
{
protected:
	// number of ensembles
	int nEnsemble;
	// vector containing the renumbered indices of the ensembles
	vector<int> ensembleMap;

	// data
	double ***valueTensor;
	double ***objTensor;
	// pointer to the matrix containing values for (nSeries,nTimes)
	timeSeriesMatrix **valueMatrix;

public:
	/** 
	* @brief Constructor
	*
	* @param nEnsenble        - number of ensembles
	* @param nTimeStep        - number of time steps
	* @param *time            - pointer to timeseries values
	* @param nSeries          - number of series
	* @param SeriesID         - pointer to array with timeseries ID's
	* @param seriesValidation - pointer to array with validation settings for the timeseries
	* @param scalarIDMap      - TODO comment
	* @param vectorIDMap      - TODO comment
	*/
	timeSeriesTensor(int nEnsemble,
					 int nTimeStep,
					 vector<long long> time,
					 int nSeries,
					 vector<string> seriesID,
					 vector<validationEnum> seriesValidation,
					 map<string,int> scalarIDMap, map<string,pair<int,int> > vectorIDMap);
	/** 
	* @brief Destructor
	*/
	~timeSeriesTensor(void);

	/** 
	* @brief Returns the number of ensembles.
	*
	* @return the number of ensembles
	*/
	int getNEnsemble();

	/** @brief Returns the renumbering of the ensembleIndx. 
	* 
	* @return vector with indices, to be used as mask-vector
	*/
	vector<int> getEnsembleMap(); 

	/** @brief Renumbers the ensemleIndx, if wanted. */
	void setEnsembleMap(vector<int> ensembleMap);

	/** @brief Returns the number of timeseries 
	* 
	* @return number of series in the underlying matrix structure
	*/
	int getNSeriesMatrix();

	/** @brief throws an exception, method is not available */    
	double*** getValueTensor();
	/** @brief throws an exception, method is not available */    
	double*** getObjTensor();

 	/** 
	* @brief Returns value at position [ensembleId][tIndex][sIndex].
    *
	* @param ensembleId - index of ensemble
	* @param tIndex - time index in full timeseries vector
	* @param sIndex - series index
	* @return value at specified indices
	*/
	double getValue(int ensembleId, int tIndex, int sIndex);

	/** 
	* @brief Sets a specific value 
	*
	* @param ensembleId - ensemble index
	* @param tIndex     - index in full timeseries vector
	* @param sIndex     - series index
	* @param value      - value to write
	*/
	void setValue(int ensembleId, int tIndex, int sIndex, double value);

	void validate(int nSeriesEnd, int nTimeStepT0);

	double* getState(int ensembleId, int tIndex);

	/** @brief Method not available.
	*   For this object, state is initialized by the Constructor.
	*
	*  @return exception: method not available. */
	void initState();


	void incrementTimeStep(int emsebleId);

	/** 
	* @brief Retutns a pointer to the underlying matrix for ensembleId
	*
	* @param ensembleId - ensemble index
	* @return           - pointer to a matrix for given ensembleId */
	timeSeriesMatrix* getTimeSeriesMatrix(int ensembleId);

};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESTENSOR_H */
