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

#include "guideBandRule.h"
#include <stdexcept>

using namespace rtctools;
using namespace rtctools::schematization::rules;


guideBandRule::guideBandRule(string id, 
							 string name, 
							 dateLookupTableConverter *xMin, 
							 dateLookupTableConverter *xMax,
							 double yMin,
							 int iYMin,
	                         double yMax,
							 int iYMax,
							 int iXIn, 
							 int iYIn, 
							 int iYOut)
	: rule(id, name)
{
	this->xMin = xMin;
    this->xMax = xMax;
	this->yMin = yMin;
	this->iYMin = iYMin;
	this->yMax = yMax;
	this->iYMax = iYMax;
    this->iXIn = iXIn;
    this->iYIn = iYIn;
    this->iYOut = iYOut;
}

void guideBandRule::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double xMinValue = xMin->convert(t);
    double xMaxValue = xMax->convert(t);

	double yMinL = yMin;
	if (iYMin>-1 && stateNew[iYMin]==stateNew[iYMin]) {
		yMinL = stateNew[iYMin];
	}

	double yMaxL = yMax;
	if (iYMax>-1 && stateNew[iYMax]==stateNew[iYMax]) {
		yMaxL = stateNew[iYMax];
	}

    double x = stateOld[iXIn];
    if (x <= xMinValue) {
		stateNew[iYOut] = yMinL;
    } else if (x >= xMaxValue) {
        stateNew[iYOut] = yMaxL;
    } else {
        stateNew[iYOut] = (x-xMinValue) * (yMaxL-yMinL) / (xMaxValue-xMinValue) + yMinL;
    }
        
    if (iYIn > -1) {
        double y = stateNew[iYIn];
		if (y==y) {
			stateNew[iYOut] = y;
		}
    }
}

void guideBandRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew)
{
	throw runtime_error("void guideBandRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew) not implemented");
}
