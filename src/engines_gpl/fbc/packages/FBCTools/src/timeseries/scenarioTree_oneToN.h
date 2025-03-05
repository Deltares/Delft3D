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

/**
 * @file
 * @brief xxx
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2014
 */

#ifndef SCENARIOTREE_ONETON_H
#define SCENARIOTREE_ONETON_H

#include "scenarioTreeGenerator.h"
#include <string>
#include <vector>

using namespace std;

namespace rtctools
{
namespace timeseries
{

/**
  * @brief Scenario tree generator for 1:N scenario trees
  *
  * Generation of a simple scenarion tree with a single trajectory at 
  * the beginning and a single branching point at time step nStep
  * to N number of branches
  */
class scenarioTree_oneToN : public scenarioTreeGenerator
{
public:
	/**
	  * @brief Struct with the parameters of the 1:N tree generator
	  */
	struct PARAMETER {
		/**
		  * @brief Number of branches after the  branching point
	      */
		int nBranch;
		/**
		  * @brief Position of the branching point in time steps starting from the root of the ensemble
	      */
		int nStep;
		/**
		  * @brief Number of steps with smoothing for discontinuities at branching points
	      */
		int nStepSmoothing;
	};

	/**
	  * @brief Constructor
	  *
	  * @param iReference	reference to series for tree construction
	  * @param iInput		reference to ensemble input series
	  * @param iOutput		reference to scenario tree output series
	  * @param iProbability	reference to optional probability output
	  * @param par			parameters
	  */
	scenarioTree_oneToN(int iReference, vector<int> iInput, vector<int> iOutput, int iProbability, PARAMETER par);

	/**
	  * @brief Destructor
	  */
	~scenarioTree_oneToN() {};

	/**
	  * @brief tree generation and conversion of input to output
	  */
	scenarioTreeGenerator::scenarioTree generate(timeSeriesTensorInterface* tsTensor);

private:
	PARAMETER par;
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* SCENARIOTREE_ONETON_H */
