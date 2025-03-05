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

#include <schematization/triggers/standardTrigger.h>

#include <limits>

using namespace rtctools::schematization::triggers;

standardTrigger::standardTrigger(string id,
								 string name,
								 condition con,
								 bool yDefaultPresent,
								 bool yDefaultValue,
								 int iYOut,
								 int iTimeTrue,
								 int iTimeFalse) : trigger(id, name, iYOut, iTimeTrue, iTimeFalse)
{
	this->con = con;
	this->yDefaultPresent = yDefaultPresent;
	this->yDefaultValue = yDefaultValue;
}

standardTrigger::~standardTrigger(void)
{
}

void standardTrigger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	// check for default value
	double yNew = numeric_limits<double>::quiet_NaN();
	if (yDefaultPresent) {
		if (yDefaultValue) yNew = 1.0; else yNew = 0.0;
	} 

	// apply condition result if valid
	double yNew2 = con.evaluate(stateOld, stateNew);
	if (yNew2==yNew2) yNew = yNew2;

	// write new status (NaN if no default is provided and the condition can not be evaluated)
	stateNew[iYOut] = yNew;

	// evaluate true/false times and sub-triggers
	evaluateTimes(stateOld, stateNew, t, dt);
	evaluateSubtriggers(stateOld, stateNew, t, dt);
}
