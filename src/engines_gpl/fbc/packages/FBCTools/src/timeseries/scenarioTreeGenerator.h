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

#ifndef SCENARIOTREE_GENERATOR_H
#define SCENARIOTREE_GENERATOR_H

#include "timeseries/timeSeriesTensorInterface.h"
#include <string>
#include <vector>

using namespace std;

namespace rtctools
{
namespace timeseries
{

class scenarioTreeGenerator
{
public:
	struct scenarioTree {

		/**
		  * @brief Number of final branches of the tree
	      */
		int nBranch;

		/**
		  * @brief Number of ensemble members in the underlying ensemble (nEnsemble >= nBranch)
	      */
		int nEnsemble;

		/**
		  * @brief Number of optimization variables in the total tree
	      */
		int nStepTotal;

		/**
		  * @brief Number of optimization variables in a single branch
	      */
		int nStepBranch;

		/**
		  * @brief 2D array with indices of each branch
	      */
		vector<vector<int> >indexArray;

		/**
		  * @brief Probability of each branch
	      */
		vector<double> pVec;

		/**
		  * @brief Ensemble indices of the branches
	      */
		vector<int> iVec;
	};

	/**
	  * @brief Constructor
	  *
	  * @param iReference	reference to series for tree construction
	  * @param iInput		reference to ensemble input series
	  * @param iOutput		reference to scenario tree output series
	  * @param iProbability	reference to optional probability output
	  */
	scenarioTreeGenerator(int iReference, vector<int> iInput, vector<int> iOutput, int iProbability);

	/**
	  * @brief Destructor
	  */
	~scenarioTreeGenerator() {};

	/**
	  * @brief Tree generation and conversion of input to output
	  */
	virtual scenarioTree generate(timeSeriesTensorInterface* tsTensor) = 0;

	vector<double> initP(int nEnsemble);
	vector<vector<int> > initDirect(int nEnsemble, int nStep);
	vector<vector<double> > getEnsemble(timeSeriesTensorInterface *tsTensor, int indx);
	void setEnsemble(timeSeriesTensorInterface *tsTensor, int indx, vector<vector<double> > ensemble);
	vector<vector<double> > computeDistanceMatrix(vector<vector<double> > ensemble, int k0, int k1);
	void reduction(int nStep, vector<vector<double> > ensemble, vector<double>& p, vector<vector<int> >& direct, int s);
	void average(timeSeriesTensorInterface* tsTensor, vector<vector<int> >& direct, vector<double>& p, int in, int out);
	scenarioTree generateScenarioTree(vector<double> p, vector<vector<int> > direct);

protected:
	/**
	  * @brief Reference to series for tree construction
	  */
	int iReference;

	/**
	  * @brief Reference to ensemble input series
	  */
	vector<int> iInput;

	/**
	  * @brief Reference to scenario tree output series
	  */
	vector<int> iOutput;

	/**
	  * @brief Reference to optional probability output
	  */
	int iProbability;
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* SCENARIOTREE_GENERATOR_H */
