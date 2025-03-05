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

#ifndef EQUIDISTANT_AGGREGATION_H
#define EQUIDISTANT_AGGREGATION_H

#include "aggregation.h"
#include <rtcToolsEnums.h>

namespace rtctools
{
namespace utilities
{

class equidistantAggregation : public aggregation
{
public:
	equidistantAggregation(
		interpolationOption intOpt,
		int nStep);
	~equidistantAggregation(void) {};

	int getNStep() { return nStep; };
	void aggregate(std::vector<double> &largeArray, std::vector<double> &smallArray, bool accumulative = true);
	void deaggregate(std::vector<double> &smallArray, double large0, std::vector<double> &largeArray);

private:
	interpolationOption intOpt;
	int nStep;
};

} // end namespace utilities
} // end namespace rtctools

#endif // EQUIDISTANT_AGGREGATION_H
