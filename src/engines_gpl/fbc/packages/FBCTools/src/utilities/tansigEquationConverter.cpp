// Copyright (C) 2012 Deltares
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
 * @author Dirk Schwanenberg, Stefano Galelli
 * @version 1.0
 * @date 2012
 */

#include "tansigEquationConverter.h"

#include <cmath>

using namespace rtctools;
using namespace rtctools::utilities;

tansigEquationConverter::tansigEquationConverter()
{
}

tansigEquationConverter::~tansigEquationConverter(void)
{
}

double tansigEquationConverter::convert(double x)
{
	double y = 2.0/(1.0+exp(-2.0*x))-1.0;
    return y;
}

double tansigEquationConverter::convertDer(double x)
{
	// derivative of y = 2.0/(1.0+exp(-2.0*x))-1.0;
    double dy = 4.0*exp(-2.0*x)/pow(1.0+exp(-2.0*x),2.0);
    return dy;
}

double tansigEquationConverter::reverseConvert(double y)
{
    double x = -log(2.0/(y+1.0)-1.0)/2.0;
    return x;
}

double tansigEquationConverter::reverseConvertDer(double yVal)
{
	// TODO

	return 0.0;
}

converter* tansigEquationConverter::getIntegrator()
{
	/**
	 * @todo implement converter* sigmoidLogisticEquationConverter::getIntegrator()
	 */
    return 0;
}
