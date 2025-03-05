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

#ifndef SCENARIOTREE_BINARY_H
#define SCENARIOTREE_BINARY_H

#include "scenarioTreeGenerator.h"
#include "timeseries/timeSeriesTensorInterface.h"
#include <string>
#include <vector>

using namespace std;

namespace rtctools
{
namespace timeseries
{

/**
  * @brief Scenario tree generator for binary scenario trees
  *
  * Generation of a binary scenario tree with equidistant branching point.
  * The number of branches can be 2^x: 1, 2 4 8 16, ...
  */
class scenarioTree_binary : public scenarioTreeGenerator
{
public:
	struct branchSet {
		vector<int> indices;
		branchSet* set1;
		branchSet* set2;
		void split(int nReduction, vector<vector<double> >& ensemble, vector<vector<int> >& direct, int iStart);
	};

	struct PARAMETER {
		/**
		  * @brief Number of branches after the  branching point
	      */
		int nBranch;
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
	scenarioTree_binary(int iReference, vector<int> iInput, vector<int> iOutput, int iProbability, PARAMETER par);

	/**
	  * @brief Destructor
	  */
	~scenarioTree_binary() {};

	/**
	  * @brief tree generation and conversion of input to output
	  */
	scenarioTreeGenerator::scenarioTree generate(timeSeriesTensorInterface* tsTensor);

private:
	PARAMETER par;
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* SCENARIOTREE_BINARY_H */
