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

#include <schematization/triggers/deadbandTrigger.h>

#include <limits>

using namespace rtctools::schematization::triggers;

deadbandTrigger::deadbandTrigger(
	string id,
	string name,
	condition conOn,
	condition conOff,
	bool yDefaultPresent,
	bool yDefaultValue,
	int iYOut,
	int iTimeTrueOut,
	int iTimeFalseOut) : trigger(id, name, iYOut, iTimeTrueOut, iTimeFalseOut)
{
	this->conOn = conOn;
	this->conOff = conOff;
	this->yDefaultPresent = yDefaultPresent;
	this->yDefaultValue = yDefaultValue;
}

deadbandTrigger::~deadbandTrigger(void)
{
}

void deadbandTrigger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double yOld = stateOld[iYOut];

	// check for default value
	double yNew = numeric_limits<double>::quiet_NaN();
	if (yDefaultPresent) {
		if (yDefaultValue) yNew = 1.0; else yNew = 0.0;
	} 

	double vOn = conOn.evaluate(stateOld, stateNew);
	double vOff = conOff.evaluate(stateOld, stateNew);

	// evaluate new trigger status
	if ((vOn==vOn) & (vOff==vOff)) {
		if (vOn==1.0) {
			yNew = 1.0;
		} else if (vOff==1.0) {
			yNew = 0.0;
		} else {
			if (yOld==0.0) yNew = 0.0; else if (yOld==1.0) yNew = 1.0;
		}
	}

	// Write new status
	// Can be a NaN-value
	stateNew[iYOut] = yNew;

	// evaluate true/false times and sub-triggers
	evaluateTimes(stateOld, stateNew, t, dt);
	evaluateSubtriggers(stateOld, stateNew, t, dt);
}
