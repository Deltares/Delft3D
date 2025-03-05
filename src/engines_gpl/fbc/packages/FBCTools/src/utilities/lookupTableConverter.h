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

#ifndef LOOKUP_TABLE_CONVERTER_H
#define LOOKUP_TABLE_CONVERTER_H

#include "utilities/converter.h"
#include <limits>
#include <vector>

using namespace std;

namespace rtctools
{
namespace utilities
{

class lookupTableConverter : public converter
{
private:
	int n;
	vector<double> xArray;
	vector<double> yArray;
	interpolationOption intOpt;
	interpolationOption extOpt;

	double interpolate(vector<double> &x, vector<double> &y, double xVal);

public:
	lookupTableConverter(int n, double *x, double *y, interpolationOption intOpt, interpolationOption extOpt);
	lookupTableConverter(int n, vector<double> &x, vector<double> &y, interpolationOption intOpt, interpolationOption extOpt);
	~lookupTableConverter(void) {};

	void checkProperty(converter::propertyEnum prop) {};

	void setInterpolationOption(interpolationOption intOpt) { this->intOpt = intOpt; };
	void setExtrapolationOption(interpolationOption extOpt) { this->extOpt = extOpt; };
	double convert(double xVal);
	double convertDer(double xVal);
	double reverseConvert(double yVal);
	double reverseConvertDer(double yVal);
	lookupTableConverter* getIntegrator();
};

} // end namespace utilities
} // end namespace rtctools

#endif //LOOKUP_TABLE_CONVERTER_H
