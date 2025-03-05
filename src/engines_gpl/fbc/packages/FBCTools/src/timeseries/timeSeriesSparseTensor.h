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

#pragma once

#ifndef TIMESERIESSPARSETENSOR_H
#define TIMESERIESSPARSETENSOR_H

#include <map>
#include "timeseries/timeSeriesTensorInterface.h"
#include "timeseries/timeSeriesSparseMatrix.h"
#include "utilities/utils.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{

 /**
 * @brief Data structure to store a sparse representation of the tensor
 * containing timeseries values for (nEnsemble, nSeries, nTimes).
 * 
 * Data is stored as given in input file timeseries_import.xml. Interpolation 
 * is performed only after the getValue request for specific (ensembleId, sIndex, tIndex).
 */
class timeSeriesSparseTensor: public timeSeriesTensorInterface
{
protected:
	// number of ensembles
	int nEnsemble;
	// vector containing the renumbered indices of the ensembles
	vector<int> ensembleMap;
	// vector of pointers to objects timeSeriesSparseMatrix
	boost::ptr_vector<timeSeriesSparseMatrix> smVector;

public:
	/** 
	* @brief Constructor
	*
	*/
	timeSeriesSparseTensor(int nEnsemble,                     ///< number of ensembles
					 int nTimeStep,                           ///< number of time steps
					 vector<long long> time,                  ///< vector with timeseries values
					 int nSeries,                             ///< number of series
					 vector<string> seriesID,                 ///< vector with timeseries ID's
					 vector<validationEnum> seriesValidation, ///< vector with validation settings for the timeseries
					 map<string,int> scalarIDMap,             ///< TODO comment
					 map<string,pair<int,int> > vectorIDMap,  ///< TODO comment
					 int nimport                              ///< number of import timeseries
					 );                           
	/** 
	* @brief Destructor
	*/
	~timeSeriesSparseTensor(void);

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


 	/** 
	* @brief Returns value at position [ensembleId][tIndex][sIndex] in full data-array,
	*	in the time direction interpolated or extrapolated from values stored in the sparse data structure.
	*
	* @return value at specified indices, inter- or extrapolated from values as stored in timeSeriesSparseTensor
	*/
	double getValue(int ensembleId,  ///< index of ensemble
					int tIndex,      ///< time index in full timeseries vector
					int sIndex       ///< series index
					);

	/** 
	* @brief Resets a specific value in an already stored timeseries; 
	* throws an error if the vector does not exist or does not contain the specified index
	*/
	void setValue(int ensembleId,   ///< ensemble index
                  int tIndex,       ///< index in full timeseries vector
				  int sIndex,       ///< series index
				  double value      ///< value to reset
				  );

	/**
	* @brief Sets a whole timeseries for one ensemble and series index.
	* 
	*/
	void setSeries(int ensembleId,               ///< ensemble index
				   int sIndex,                   ///< series index
				   vector<long long>* times,     ///< sparse time vector; setSeries will make a copy of the vector and will not take ownership of the original vector
				   vector<double>* vals,         ///< sparse value vector; setSeries will make a copy of the vector and will not take ownership of the original vector
				   long long t1,                 ///< start time of the sparse timeseries
				   long long t2,                 ///< end time of the sparse timeseries
				   long long dt,                 ///< timestep of the sparse timeseries
				   interpolationOption interpol, ///< interpolation method for timeseries
				   interpolationOption extrapol  ///< extrapolation method for timeseries
				   );

	timeSeriesMatrixInterface* getTimeSeriesMatrix(int ensembleId);

    /** @brief Method not available.
	*
	*  @return exception: method not available. */
	double*** getValueTensor();

    /** @brief Method not available.
	*
	*  @return exception: method not available. */
	double*** getObjTensor();
	 
	/** @brief Method not availble.
	*
	* @return exception: method not available */
	void validate(int nSeriesEnd, int nTimeStepT0);

	/** @brief Initialize the State after reading of input data. */
	void initState();

	/** @brief Return pointer to double array[2*nSeries]
	*          containing the State values.
	* 
	* @return pointer to double array[2*nSeries] */
	double* getState(int ensembleId,  ///< ensemble index
                     int tIndex       ///< index in full timeseries vector
					 );

	/** @brief Advance the internal time step and update 
	*          the array containing the State. 
	*/
	void incrementTimeStep(int ensembleId  ///< ensemble index
		);

};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESSPARSETENSOR_H */
