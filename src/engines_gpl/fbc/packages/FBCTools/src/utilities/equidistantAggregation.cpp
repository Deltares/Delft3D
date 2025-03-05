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

#include "equidistantAggregation.h"

#include <cmath>
#include <stdexcept>

using namespace std;
using namespace rtctools::utilities;

equidistantAggregation::equidistantAggregation(
	interpolationOption intOpt, int nStep)
{
	this->intOpt = intOpt;
	this->nStep = nStep;
}

void equidistantAggregation::deaggregate(std::vector<double> &smallArray, double large0, std::vector<double> &largeArray)
{
	int nLarge = (int)largeArray.size();
	int nSmall = (int)smallArray.size();

	if (nLarge != nSmall*nStep) {
		throw runtime_error("inconsistent array lenths in equidistantAggregation::deaggregate");
	}

	if (intOpt==BLOCK) {
		// BLOCK
		for (int i=0; i<nLarge; i++) {
			largeArray[i] = smallArray[i/nStep];
		}
	} else if (intOpt==LINEAR) {
		// LINEAR
		for (int i=0; i<nStep; i++) {
			largeArray[i] = large0 + ((double)i+1.0)/(double)nStep*(smallArray[0]-large0);
		}
		for (int i=1; i<nSmall; i++) {
			for (int j=0; j<nStep; j++) {
				largeArray[i*nStep+j] =
					smallArray[i-1] + ((double)j+1.0)/(double)nStep*(smallArray[i]-smallArray[i-1]);
			}
		}
	} else {
		throw runtime_error("interpolation option not implemented in equidistantAggregation::deaggregate");
	}
}

void equidistantAggregation::aggregate(std::vector<double> &largeArray, std::vector<double> &smallArray, bool accumulative)
{
	int nLarge = (int)largeArray.size();
	int nSmall = (int)smallArray.size();

	if (nLarge != nSmall*nStep) {
		throw runtime_error("inconsistent array lengths in equidistantAggregation::aggregate");
	}

	if (intOpt==BLOCK) {
		// BLOCK
		for (int i=0; i<nSmall; i++) smallArray[i] = 0.0;
		for (int i=0; i<nLarge; i++) {
			smallArray[i/nStep] += largeArray[i];
		}
		if (!accumulative) {
			for (int i=0; i<nSmall; i++) smallArray[i] *= (double)nSmall/(double)nLarge;
		}
	/*} else if (intOpt==LINEAR) {
		// LINEAR
		for (int i=0; i<nStep; i++) {
			outArray[i] = in0 + ((double)i+1.0)/(double)nStep*(inArray[0]-in0);
		}
		for (int i=1; i<nIn; i++) {
			for (int j=0; j<nStep; j++) {
				outArray[i*nStep+j] =
					inArray[i-1] + ((double)j+1.0)/(double)nStep*(inArray[i]-inArray[i-1]);
			}
		}*/
	} else {
		throw runtime_error("interpolation option not implemented in equidistantAggregation::aggregate");
	}
}
