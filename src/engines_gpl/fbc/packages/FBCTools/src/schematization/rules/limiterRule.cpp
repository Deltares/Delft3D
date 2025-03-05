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

#include "limiterRule.h" 
#include <stdexcept>

using namespace rtctools;
using namespace rtctools::schematization::rules;


limiterRule::limiterRule(string id,
					     string name,
					     limiterRule::Mode mode,
						 limiterRule::INPUT iInput)
	: rule(id, name), mode(mode), iInput(iInput)
{ }

void limiterRule::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double threshold;
	if (iInput.threshold.source==VALUE) {
		threshold = iInput.threshold.value;
	} else {
		threshold = stateOld[iInput.threshold.indx];
	}
	if (mode==PERCENTAGE) threshold *= stateOld[iInput.x]/100.0;

	double xOld = stateOld[iInput.x];
	double xNew = stateNew[iInput.x];
	if (threshold==threshold && xOld==xOld && xNew==xNew) {
		if (xNew-xOld > +threshold) stateNew[iInput.x] = xOld + threshold;
		if (xNew-xOld < -threshold) stateNew[iInput.x] = xOld - threshold;
	}
}

void limiterRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew)
{
	throw runtime_error("void limiterRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew) not implemented");
}
