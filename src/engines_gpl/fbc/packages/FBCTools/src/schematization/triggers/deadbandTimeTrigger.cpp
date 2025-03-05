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
 * @date 2011
 */

#include <schematization/triggers/deadbandTimeTrigger.h>

#include <stdexcept>

using namespace rtctools::schematization::rules;

deadbandTimeTrigger::deadbandTimeTrigger(
	string id,
	string name,
	int nStepUp,
	int nStepDown,
	int iXIn,
	int iStepsUpOut,
	int iStepsDownOut,
	int iYOut,
	int iTimeTrueOut,
	int iTimeFalseOut) : trigger(id, name, iYOut, iTimeTrueOut, iTimeFalseOut), rule(id, name)
{
	this->nStepUp = nStepUp;
	this->nStepDown = nStepDown;
	this->iXIn = iXIn;
	this->iStepsUpOut = iStepsUpOut;
	this->iStepsDownOut = iStepsDownOut;
}

deadbandTimeTrigger::~deadbandTimeTrigger(void)
{
}

void deadbandTimeTrigger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double x = stateOld[iXIn];
	double sU = stateOld[iStepsUpOut];
	double sD = stateOld[iStepsDownOut];
	double y = stateOld[iYOut];

	// robust behaviour in case of incomplete inputs / outputs
	if (y!=y) {
		y = x;
	}

	// count the number of time steps with continuous up or down-crossing
	if (x<y) {
		sU = 0;
	    sD++;
	} else if (x>y) {
	    sU++;
	    sD = 0;
	} else {
	    sU = 0;
	    sD = 0;
	}

	// adapt the output, if step threshold is exceeded
	if ((sU>nStepUp) || (sD>nStepDown)) {
		y = x;
	    sU = 0;
	    sD = 0;
	}

	// write new status
	stateNew[iYOut] = y;
	stateNew[iStepsUpOut] = sU;
	stateNew[iStepsDownOut] = sD;

	// evaluate true/false times and sub-triggers
	evaluateTimes(stateOld, stateNew, t, dt);
	evaluateSubtriggers(stateOld, stateNew, t, dt);
}

void deadbandTimeTrigger::solveDer(double* stateOld, double* stateNew, long long t, double dt,
								   double* dStateOld, double* dStateNew)
{
	throw runtime_error("void deadbandTimeTrigger::solveDer(...) - error - not implemented");
}
