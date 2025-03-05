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
 * @author Tobias Schruff
 * @version 1.0
 * @date 2010
 */

#ifndef GUIDEBANDRULE_H
#define GUIDEBANDRULE_H

#include <string>
#include "utilities/dateLookupTableConverter.h"
#include "schematization/rules/rule.h"

using namespace rtctools::utilities;

namespace rtctools
{
namespace schematization
{
namespace rules
{

class guideBandRule : public rule
{

public:
	/** Default constructor */
	guideBandRule(string id, 
				  string name, 
				  dateLookupTableConverter *xMin, 
				  dateLookupTableConverter *xMax,
				  double yMin,
				  int iYMin,
				  double yMax,
				  int iYMax,
				  int iXIn, 
				  int iYIn, 
				  int iYOut);

	/** Default destructor */
	~guideBandRule() {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew);

private:
	dateLookupTableConverter *xMin;
    dateLookupTableConverter *xMax;
	double yMin;
	int iYMin;
	double yMax;
	int iYMax;
    int iXIn;
    int iYIn;
    int iYOut;
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif // GUIDEBANDRULE_H
