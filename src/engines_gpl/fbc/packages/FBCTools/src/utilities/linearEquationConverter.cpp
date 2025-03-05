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


#include "linearEquationConverter.h"

#include <cmath>
#include <algorithm>


#ifdef _WIN32
#include <windows.h>
#endif

using namespace rtctools;
using namespace rtctools::utilities;

linearEquationConverter::linearEquationConverter(double a, double b)
{
	this->a = a;
	this->b = b;
}

linearEquationConverter::~linearEquationConverter(void)
{
}

double linearEquationConverter::convert(double x)
{
	double y;

	y = a*x+b;

    return y;
}

double linearEquationConverter::convertDer(double x)
{
	double dy;

	// derivative of y = a*x+b;
    dy = a;

    return dy;
}

double linearEquationConverter::reverseConvert(double y)
{
	double x;

    x = (y-b)/a;

    return x;
}

double linearEquationConverter::reverseConvertDer(double yVal)
{
	// TODO

	return 0.0;
}

converter* linearEquationConverter::getIntegrator()
{
	// TODO
    return 0;
}
