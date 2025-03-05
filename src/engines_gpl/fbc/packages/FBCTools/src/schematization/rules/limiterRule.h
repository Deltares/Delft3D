// Copyright (C) 2014 Deltares
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
 * @date 2014
 */

#ifndef LIMITER_H
#define LIMITER_H

#include <string>
#include "schematization/rules/rule.h"


namespace rtctools
{
namespace schematization
{
namespace rules
{

class limiterRule : public rule
{

public:
	/**
	  * @brief Enum for switching between relative [0-100] and absolute mode
	  * [m3/s] for spill target
	  */
	enum Mode {
		ABSOLUTE,
		PERCENTAGE
	};

	enum inputSelectionEnum {
		VALUE,
		TIMESERIES
	};

	struct inputSelection {
		inputSelectionEnum source;
		int indx;
		double value;
	};

	struct INPUT {
		inputSelection threshold;
		int x;
	};

	/** Default constructor */
	limiterRule(string id,
				string name,
				Mode mode,
				INPUT iInput);

	/** Default destructor */
	~limiterRule() {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew);

private:
    Mode mode;
	INPUT iInput;
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif // LIMITER_H
