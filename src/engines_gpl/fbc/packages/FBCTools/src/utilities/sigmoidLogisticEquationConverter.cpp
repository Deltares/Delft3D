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

#include "sigmoidLogisticEquationConverter.h"

#include <cmath>

using namespace rtctools;
using namespace rtctools::utilities;

sigmoidLogisticEquationConverter::sigmoidLogisticEquationConverter()
{
}

sigmoidLogisticEquationConverter::~sigmoidLogisticEquationConverter(void)
{
}

double sigmoidLogisticEquationConverter::convert(double x)
{
	double y = 1.0/(1.0+exp(-x));
    return y;
}

double sigmoidLogisticEquationConverter::convertDer(double x)
{
	// derivative of y = 1.0/(1.0+exp(-x));
    double dy = exp(-x)/pow(1.0+exp(-x),2.0);
    return dy;
}

double sigmoidLogisticEquationConverter::reverseConvert(double y)
{
    double x = -log(1.0/y-1.0);
    return x;
}

double sigmoidLogisticEquationConverter::reverseConvertDer(double yVal)
{
	// TODO

	return 0.0;
}

converter* sigmoidLogisticEquationConverter::getIntegrator()
{
	/**
	 * @todo implement converter* sigmoidLogisticEquationConverter::getIntegrator()
	 */
    return 0;
}
