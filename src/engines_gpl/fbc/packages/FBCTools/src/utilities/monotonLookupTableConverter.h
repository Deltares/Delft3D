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

#ifndef MONOTONLOOKUP_TABLE_CONVERTER_H
#define MONOTONLOOKUP_TABLE_CONVERTER_H

#include "utilities/converter.h"
#include <limits>
#include <vector>

using namespace std;

namespace rtctools
{
namespace utilities
{

class monotonLookupTableConverter : public converter
{
private:
	int n;
	vector<double> xArray;
	vector<double> yArray;
	interpolationOption intOpt;
	interpolationOption extOpt;
	bool convertable;

	double interpolate(vector<double> &x, vector<double> &y, double xVal);
	double interpolateDer(vector<double> &x, vector<double> &y, double xVal);

public:
	monotonLookupTableConverter(int n, vector<double> &x, vector<double> &y, interpolationOption intOpt, interpolationOption extOpt);
	~monotonLookupTableConverter(void) {};

	void setInterpolationOption(interpolationOption intOpt) { this->intOpt = intOpt; };
	void setExtrapolationOption(interpolationOption extOpt) { this->extOpt = extOpt; };
	void checkProperty(converter::propertyEnum prop);

	double convert(double xVal);
	double convertDer(double xVal);
	double reverseConvert(double yVal);
	double reverseConvertDer(double yVal);
	monotonLookupTableConverter* getIntegrator();
};

} // end namespace utilities
} // end namespace rtctools

#endif //MONOTONLOOKUP_TABLE_CONVERTER_H
