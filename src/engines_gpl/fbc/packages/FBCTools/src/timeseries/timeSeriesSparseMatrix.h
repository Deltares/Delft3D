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

#ifndef TIMESERIESSPARSEMATRIX_H
#define TIMESERIESSPARSEMATRIX_H

#include <map>
#include <vector>
#include <boost/ptr_container/ptr_vector.hpp>
#include "timeseries/sparseTimeSeries.h"
#include "timeseries/timeSeriesMatrixInterface.h"
#include "utilities/utils.h"
#include "rtcToolsEnums.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{

 /**
 * @brief Data structure to store a sparse representation of the matrix
 * containing timeseries values for (nSeries, nTimes).
 * 
 * Data is stored as given in input file timeseries_import.xml. Interpolation 
 * is performed only after the getValue request for specific (sIndex,tIndex).
 */
class timeSeriesSparseMatrix : public timeSeriesMatrixInterface
{
protected:
	// internal data structure
	/** vector with pointers to timeSeriesSparseMatrix objects */ 
	boost::ptr_vector<sparseTimeSeries> sparseseriesvector;
	/** TODO comment */
	double *state;
	/** index in state array */
	int iState;
    // number of import timeseries
    int nimport;

public:
	/** length of sparseseriesvector */
	int nSparse;

	/**
	 * @brief Constructor
	 */
	timeSeriesSparseMatrix(int nTimeStep, vector<long long> time,
					 int nSeries, vector<string> seriesID,
					 vector<validationEnum> seriesValidation,
					 map<string,int> scalarIDMap,
					 map<string,pair<int,int> > vectorIDMap,
					 int nimport);
	/**
	 * @brief Destructor
	 */
	virtual ~timeSeriesSparseMatrix(void);

	/**
	 * @brief Returns the value for given tIndex and sIndex.
	 *        If tIndex matches the time index of the state, the value stored in state is returned. 
	 *        Otherwise, the value is interpolated or extrapolated on the fly from the import timeseries.
	 */
	double getValue(int tIndex,	    ///< index in full timeseries vector
			int sIndex			    ///< index in series vector
			);

	/**
	 * @brief Sets the value in state for given tIndex and sIndex.
	 *        Returns false if tIndex does not match the time index of the state.
	 *        (No setValue possible for the data in the import timeseries)
	 * 
	 * @return       true if value in state was (re)set, false if input indices are out of bounds for the state vector.
	 */
	bool setValue(int tIndex,       ///< index in full timeseries vector
				  int sIndex,       ///< index in series vector
				  double value      ///< value to reset
				  );

	/** 
	* @brief Stores a sparse timeseries for one series index.
	* 
	*/
	void setSeries(int sIndex, 
				   vector<long long>* times,        ///< series index
				   vector<double>* vals,            ///< pointer to a vector containing the timeseries
				   long long t1, 
				   long long t2,
				   long long dt,                    ///< the timestep in sparse representation 
				   interpolationOption interpol,    ///< interpolation option to be used in retrieving values for times in between index values
				   interpolationOption extrapol     ///< extrapolation option to be used in retrieving values for times outside index values
				   );
	/** 
    * @brief get a pointer to the state vector at t = tIndex 
    * 
    * @return       pointer to the state vector at t = tIndex. An exception is thrown if tIndex
	*               is not the current or next timestep. 
	*/
	double* getState(int tIndex		///< index in full timeseries vector
			 );
	/** 
	* @brief initializes the state: an array containing interpolated values for all
	*        series, but only for time t and next timestep t+1. 
	*/
	void initState();

	/**
	 * @brief advances the timestep and updates the state vector.
	 */
	void incrementTimeStep();

	/** 
	* @brief TODO
	* 
	*/
	void validate(int nSeriesEnd, int nTimeStepT0);

	/**
	 * @brief Returns the value for tIndex and sIndex, interpolated or extrapolated from the import timeseries. 
	 *
 	 * @return	the interpolated value
	 */
	double getInterpolatedValue(int tIndex,		///< index in full timeseries vector
                                int sIndex  	///< index in series vector
								);

	/**
	* @brief 
	* 
	* @return the interpolated value
	*/
	double getValue_sparseseries(long long t,			    ///< the current time
		                         vector<long long> tArray,  ///<
			                	 vector<double> vArray,     ///< vector containing the values of the sparse timeseries
								 long long t1, 
								 long long t2,
								 long long dt,              ///<
								 interpolationOption intOpt,///< interpolation option to be used in retrieving values for times in between index values
								 interpolationOption extOpt ///< extrapolation option to be used in retrieving values for times outside index values
								 );

	/**
	* @brief 
	* 
	* @return 
	*/
	int find_leftindex(long long time,		                ///< the current time
                           vector<long long> sparsetime     ///< vector containing the sparse timeseries values
                          );
};

} // endnamespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESSPARSEMATRIX_H */
