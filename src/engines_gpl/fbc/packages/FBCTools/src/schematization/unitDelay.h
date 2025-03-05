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


#ifndef UNITDELAY_H
#define UNITDELAY_H

#include <schematization/components/component.h>
#include <schematization/rules/rule.h>
#include <vector>

using namespace rtctools::schematization::components;
using namespace rtctools::schematization::rules;

namespace rtctools
{
namespace schematization
{

/**
  * @brief Unit delay operator
  *
  * Unit delay operator for keeping time series data beyond
  * the old system state available for computations
  */
class unitDelay : public rule, public component
{
public:
	/**
	  * @brief Struct with indices to output time series
	  */
	struct OUTPUT {
		/**
		  * @brief Delay vector with historical time series data
		  */
		vector<int> iY;
		/**
		  * @brief Index to output time series which is delayed by nStep time steps
		  */
		int iYFinal;
		/**
		  * @brief Optional output with the minimum value of the delay vector
		  */
		int iYMin;
		/**
		  * @brief Optional output with the mean value of the delay vector
		  */
		int iYMean;
		/**
		  * @brief Optional output with the miximum value of the delay vector
		  */
		int iYMax;
		/**
		  * @brief Optional output with the sum of the delay vector
		  */
		int iYSum;
	};

	/**
	  * @brief Constructor
	  *
	  * @param id		Identifyer
	  * @param name		Name
	  * @param nStep	Number of time steps of delay of the output value
	  * @param iXIn		Index to input time series
	  * @param iOutput	Struct with indices to output time series
	  */
	unitDelay(string id, string name, double nStep, int iXIn, OUTPUT iOutput);
	/**
	  * @brief Destructor
	  */
	~unitDelay(void) {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);

private:
	/**
	  * @brief Number of time steps of delay of the output value
	  */
	int nStep;
	/**
	  * @brief Number of time steps of delay of the output value
	  */
	int i1,i2;
	/**
	  * @brief Indices of delay vector
	  *
	  * Indices of the two values of the delay vector which
	  * are required for the computation of the final value in case of
	  * a non-integer input of nStep in the constructor
	  */
	double w1,w2;
	/**
	  * @brief Weighting coefficients
	  * 
	  * Weighting coefficient of the two values which form the
	  * final value, required because we also allow non-integer 
	  */
	int iXIn;
	/**
	  * @brief Struct with indices to output time series
	  */
	OUTPUT iOutput;
};

} // end namespace schematization
} // end namespace rtctools

#endif /* UNITDELAY_H */
