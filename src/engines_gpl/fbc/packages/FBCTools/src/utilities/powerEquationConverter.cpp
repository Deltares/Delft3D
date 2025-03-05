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

#include "powerEquationConverter.h"

#include <cmath>
#include <algorithm>

using namespace rtctools;
using namespace rtctools::utilities;

powerEquationConverter::powerEquationConverter(double a, double b, double c)
{
	this->a = a;
	this->b = b;
    this->c = c;
}

powerEquationConverter::~powerEquationConverter(void)
{
}

double powerEquationConverter::convert(double xVal)
{
	double y;

	if (xVal<-b) {
		y = 0.0;
	} else {
	    y = a*pow(xVal+b,c);
	}

    return y;
}

double powerEquationConverter::convertDer(double xVal)
{
	double dy;

    if (xVal<-b) {
        dy = 0.0;
    } else {
        dy = a*c*pow(std::max(xVal+b, 0.0), c-1.0);
    }

    return dy;
}

double powerEquationConverter::reverseConvert(double yVal)
{
	double x;

    if (yVal<0.0) {
    	x = -b;
    } else {
        x = pow(yVal/a, 1.0/c) - b;
    }

    return x;
}

double powerEquationConverter::reverseConvertDer(double yVal)
{
	double dx;

    if (yVal<=0.0) {
    	dx = 0.0;
    } else {
        dx = 1/a * pow(yVal/a, 1.0/c-1.0);
    }

    return dx;
}

converter* powerEquationConverter::getIntegrator()
{
	// TODO
    return 0;
}
