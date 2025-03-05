// Copyright (C) 2013 Deltares, University of Duisburg-Essen
// Institute of Hydraulic Engineering and Water Resources Management
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
 * @date 2013
 */

#ifndef MERGERSPLITTER_H
#define MERGERSPLITTER_H

#include <schematization/triggers/trigger.h>
#include <schematization/rules/rule.h>
#include <schematization/components/component.h>
#include <vector>

using namespace rtctools::schematization::rules;

namespace rtctools
{
namespace schematization
{
namespace components
{

/**
  * @brief Simulation component for merging and splitting data
  */
class mergerSplitter : public rule, public component
{
public:
	/**
	  * @brief Enumerator for defining the component's mode
	  */
	enum modeEnum {
		AVERAGE,
		DATAHIERARCHY,
		SPREAD,
		SUM
	};

	/**
	  * @brief Struct with indices of input time series
	  */
	struct INPUT {
		/**
		  * @brief Indices to input time series
		  */
		vector<int> iX;
		/**
		  * @brief Optional factor on input values
		  */
		vector<double> xFactor;
		/**
		  * @brief Optional time series defining the start index for the operation
		  */
		int iNStepStart;
		/**
		  * @brief Optional time series defining the end index for the operation
		  */
		int iNStepEnd;
	};

	/**
	  * @brief Struct with indices of output time series
	  */
	struct OUTPUT {
		/**
		  * @brief Indices to output time series
		  */
		vector<int> iY;
		/**
		  * @brief Optional factor on output values
		  */
		vector<double> yFactor;
	};

private:
	/**
	  * @brief Merge mode
	  */
	modeEnum mode;
	/**
	  * @brief Indices to input time series
	  */
	INPUT iInput;
	/**
	  * @brief Indices to output time series
	  */
	OUTPUT iOutput;
	/**
	  * @brief Number of input time series
	  */
	int nInput;
	/**
	  * @brief Number of output time series
	  */
	int nOutput;
	/**
	  * @brief Sum of output factors
	  */
	double yFactorSum;

public:
	/**
	  * @brief Constructor
	  */
	mergerSplitter(string id, string name,
		           modeEnum mode, INPUT iInput, OUTPUT iOutput);
	/**
	  * @brief Destructor
	  */
	~mergerSplitter(void) {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
    void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);
};

} // end namespace components
} // end namespace schematization
} // end namespace rtctools

#endif /* MERGERSPLITTER_H */
