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

#include <utilities/lookupTableConverter.h>
#include <iostream>
#include <stdexcept>
#include <cmath>

using namespace rtctools::utilities;

lookupTableConverter::lookupTableConverter(int n, double *xArray, double *yArray, 
	interpolationOption intOpt, interpolationOption extOpt)
	: n(n), intOpt(intOpt), extOpt(extOpt)
{
	this->xArray = vector<double>(n);
	this->yArray = vector<double>(n);
	for (int i=0; i<n; i++) {
		this->xArray[i] = xArray[i];
		this->yArray[i] = yArray[i];
	}
}

lookupTableConverter::lookupTableConverter(int n, vector<double> &xArray, vector<double> &yArray, 
	interpolationOption intOpt, interpolationOption extOpt)
	: n(n), xArray(xArray), yArray(yArray), intOpt(intOpt), extOpt(extOpt)
{ }

double lookupTableConverter::convert(double xVal) 
{ 
	return interpolate(xArray, yArray, xVal); 
}

double lookupTableConverter::convertDer(double xVal) 
{
	/**
	 * @todo refactor for analytical derivative
	 */
    double EPS = 1.0e-6;
	double h=EPS*abs(xVal);
	if (h == 0.0) h=EPS;
	double xVal2=xVal+h;
	h=xVal2-xVal;
	double yVal = interpolate(xArray , yArray, xVal);
	double yVal2 = interpolate(xArray, yArray, xVal2);
	double dy = (yVal2-yVal)/h;
    return dy;
}

double lookupTableConverter::reverseConvert(double yVal) { return interpolate(yArray, xArray, yVal); }

double lookupTableConverter::reverseConvertDer(double yVal)
{
	// TODO 
	throw runtime_error("double lookupTableConverter::reverseConvertDer(double yVal) - not implemented");
}

lookupTableConverter* lookupTableConverter::getIntegrator()
{
    vector<double> yInt = vector<double>(n);

    // integrate value
    yInt[0] = 0.0;
    for (int i=1; i<n; i++) {
        yInt[i] = yInt[i-1] + 0.5*(yArray[i]+yArray[i-1])*(xArray[i]-xArray[i-1]);
    }

	/**
	 * @todo add integrators for both block and linear conversion
	 */
    return new lookupTableConverter(n, xArray, yInt, lookupTableConverter::LINEAR, lookupTableConverter::LINEAR);
}

double lookupTableConverter::interpolate(vector<double> &x, vector<double> &y, double xVal)
{
	// check for NaN input
	if (xVal!=xVal) return numeric_limits<double>::quiet_NaN();

	// check if table contains one value only
	if (n==1) return y[0];

	// value is inside of table range
	for (int it=0; it<n-1; it++) {
		if ((xVal>=x[it]) & (xVal<x[it+1]) ||
			(xVal<=x[it]) & (xVal>x[it+1])) {
			if (intOpt==BLOCK) {
				return y[it];
			} else if (intOpt==LINEAR) {
				if (x[it]==x[it+1]) {
					return y[it];
				} else {
					return y[it]+(y[it+1]-y[it])*(xVal-x[it])/(x[it+1]-x[it]);
				}
			}
		} else if ((xVal==x[it]) & (xVal==x[it+1])) {
			return y[it];
		}
    }

	if ((xVal<x[0] && x[0]<x[n-1]) ||
		(xVal>x[0] && x[0]>x[n-1])) {
		// value is smaller than table range
		if (extOpt==BLOCK) {
			return y[0];
		} else if (extOpt==LINEAR) {
			if (x[0]==x[1]) {
				return y[0];
			} else {
				return (xVal-x[0])/(x[0]-x[1])*(y[0]-y[1])+y[0];
			}
        }
	} else if ((xVal>=x[n-1] && x[n-1]>x[0]) ||
		       (xVal<=x[n-1] && x[n-1]<x[0])) {
		// value is larger than table range
		if (extOpt==BLOCK) {
			return y[n-1];
        } else if (extOpt==LINEAR) {
			if (x[n-1]==x[n-2]) {
				return y[n-1];
			} else {
				return (xVal-x[n-1])/(x[n-1]-x[n-2])*(y[n-1]-y[n-2])+y[n-1];
			}
        }
	} 

	return numeric_limits<double>::quiet_NaN();
}
