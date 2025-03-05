// Copyright (C) 2011 Deltares
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
 * @date 2011
 */

#include <utilities/monotonLookupTableConverter.h>
#include <iostream>
#include <sstream>
#include <stdexcept>

using namespace rtctools::utilities;

monotonLookupTableConverter::monotonLookupTableConverter(int n, vector<double> &xArray, vector<double> &yArray,
	interpolationOption intOpt, interpolationOption extOpt)
	: n(n), xArray(xArray), yArray(yArray), intOpt(intOpt), extOpt(extOpt)
{
	// check
	if (n<2) {
		throw runtime_error("table needs >= 2 elements - monotonLookupTableConverter::monotonLookupTableConverter(int n, double *xArray, double *yArray, interpolationOption intOpt, interpolationOption extOpt)");
	}
	convertable = true;
	for (int i=1; i<n; i++) {
		if ((xArray[i]<=xArray[i-1]) || (yArray[i]<yArray[i-1])) {
			std::stringstream ss;
			ss << "monotonLookupTableConverter::monotonLookupTableConverter(...) - error - table is not monoton at index " << i;
			throw runtime_error(ss.str().c_str());
		}
		if (yArray[i]==yArray[i-1]) convertable = false;
	}
}

void monotonLookupTableConverter::checkProperty(converter::propertyEnum prop)
{
	if (prop==converter::POSITIVE_VALUE) {
		for (int i=1; i<n; i++) {
			if (!(yArray[i]>0)) {
				throw runtime_error("void monotonLookupTableConverter::checkProperty(converter::propertyEnum prop) - error - non-negative value property violated");
			}
		}
	} else if (prop==converter::NONNEGATIVE_VALUE) {
		for (int i=1; i<n; i++) {
			if (!(yArray[i]>=0)) {
				throw runtime_error("void monotonLookupTableConverter::checkProperty(converter::propertyEnum prop) - error - non-negative value property violated");
			}
		}
	} else if (prop==converter::STRICTLY_MONOTONIC) {
		if (convertable==false) {
			throw runtime_error("void monotonLookupTableConverter::checkProperty(converter::propertyEnum prop) - error - table is not strictly monotonic");
		}
	} else if (prop==converter::MONOTONIC_2ND) {
		double dy = (yArray[1]-yArray[0])/(xArray[1]-xArray[0]); 
		for (int i=2; i<n; i++) {
			double dyNew = (yArray[i]-yArray[i-1])/(xArray[i]-xArray[i-1]);
			if (dyNew<dy) {
				throw runtime_error("void monotonLookupTableConverter::checkProperty(converter::propertyEnum prop) - error - table 1st-order derivative is not monotonic");
			} else {
				dy = dyNew;
			}
		}
	} else {
		throw runtime_error("void monotonLookupTableConverter::checkProperty(converter::propertyEnum prop) - error - property is not supported");
	}
}

double monotonLookupTableConverter::convert(double xVal) { return interpolate(xArray, yArray, xVal); }
double monotonLookupTableConverter::convertDer(double xVal) { return interpolateDer(xArray, yArray, xVal); }

double monotonLookupTableConverter::reverseConvert(double yVal) 
{ 
	if (convertable==false) throw runtime_error("double monotonLookupTableConverter::reverseConvert(double yVal) - table is not convertable");
	return interpolate(yArray, xArray, yVal); 
}

double monotonLookupTableConverter::reverseConvertDer(double yVal) 
{ 
	if (convertable==false) throw runtime_error("double monotonLookupTableConverter::reverseConvertDer(double yVal) - table is not convertable");
	return interpolateDer(yArray, xArray, yVal); 
}

monotonLookupTableConverter* monotonLookupTableConverter::getIntegrator()
{
    vector<double> yInt(n);

    // integrate value
    yInt[0] = 0.0;
    for (int i=1; i<n; i++) {
        yInt[i] = yInt[i-1] + 0.5*(yArray[i]+yArray[i-1])*(xArray[i]-xArray[i-1]);
    }

	/**
	 * @todo add integrators for both block and linear conversion
	 */
    return new monotonLookupTableConverter(n, xArray, yInt, monotonLookupTableConverter::LINEAR, monotonLookupTableConverter::LINEAR);
}

double monotonLookupTableConverter::interpolate(vector<double> &x, vector<double> &y, double xVal)
{
	// check for NaN input
	if (xVal!=xVal) return numeric_limits<double>::quiet_NaN();

	int jl = 0;
	int ju = n-1;
	while (ju-jl > 1) {
		int jm = (ju+jl) >> 1;
		if (xVal >= x[jm]) {
			jl = jm;
		} else {
			ju = jm;
		}
	}

	if (intOpt==BLOCK) {
		if (xVal<x[ju]) {
			return y[jl];
		} else {
			return y[ju];
		}
	} else if (intOpt==LINEAR) {
		return y[jl]+(y[ju]-y[jl])*(xVal-x[jl])/(x[ju]-x[jl]);
	} 

	return numeric_limits<double>::quiet_NaN();
}

double monotonLookupTableConverter::interpolateDer(vector<double> &x, vector<double> &y, double xVal)
{
	// check for NaN input
	if (xVal!=xVal) return numeric_limits<double>::quiet_NaN();

	int jl = 0;
	int ju = n-1;
	while (ju-jl > 1) {
		int jm = (ju+jl) >> 1;
		if (xVal >= x[jm]) {
			jl = jm;
		} else {
			ju = jm;
		}
	}

	if (intOpt==BLOCK) {
		return 0.0;
	} else if (intOpt==LINEAR) {
		return (y[ju]-y[jl])/(x[ju]-x[jl]);
	} 

	return numeric_limits<double>::quiet_NaN();
}
