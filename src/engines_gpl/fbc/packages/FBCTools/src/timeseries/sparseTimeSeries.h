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

#ifndef SPARSETIMESERIES_H
#define SPARSETIMESERIES_H

#include <vector>
#include "rtcToolsEnums.h"

using namespace std;

namespace rtctools
{
namespace timeseries 
{
/**
 * @brief Data structure to store sparse timeseries, as specified in the input file.
 * 
 * Initialisation is only possible with contructor. Method setvalue can only reset a value in an already existing vector. 
 */
class sparseTimeSeries
{
protected:
	// internal data structure
	int seriesIndex;            
	interpolationOption intpol; 
	interpolationOption extpol;  
	long long t1;               
	long long t2;               
	long long dt;                
	vector<long long> times;    
	vector<double> vals;        

public:
	sparseTimeSeries() {};
	/**
	 * @brief Constructor
	 */
	sparseTimeSeries( int sIndex,                    ///< index of series, used for internal reference
                      vector<long long>* times,      ///< vector containing the times for which values are available
                      vector<double>* vals,          ///< vector containing the values
                      long long t1,                  ///< start time of the timeseries
                      long long t2,                  ///< end time of the timeseries
                      long long dt,                  ///< timestep of the timeseries
                      interpolationOption interpol,  ///< interpolation method
                      interpolationOption extrapol   ///< extapolation method 
                     );

	/**
	 * @brief Destructor
	 */
	~sparseTimeSeries(void);

	/**
	 * @brief Returns the series index
	 */
	int getSeriesIndex();

	/**
     * @brief Returns the times vector.
	 */
	vector<long long> gettimes();

	/**
     * @brief Returns the value vector.
	 */
	vector<double> getvals();

	 /**
	 * @brief Returns the start time of the sparse timeseries
	 */
	long long getStarttime();

	/**
	* @brief Returns the end time of the sparse timeseries
	*/
	long long getEndtime();

	/**
	* @brief Returns the timestep used in the sparse timeseries
	*/
	long long getdt();

	/**
	 * @brief Returns the interpolation method.
	 */
	interpolationOption getInterpolationOption();

	/**
     * @brief Returns the extrapolation method.
	 */
	interpolationOption getExtrapolationOption();

	/**
     * @brief Returns a single value for specified index.
	 * 
	 * @param index - index
	 * @returns     - value at given index 
	 */
	double getvalue(int index);

   	/**
     * @brief Resets a single value for specified index.
	 * 
	 * @param index   - index to reset value for
	 * @param newval  - new value to store
	 */
	void setvalue(int index, double newval);

};

} // endnamespace timeseries
} // end namespace rtctools

#endif /* SPARSETIMESERIES_H */
