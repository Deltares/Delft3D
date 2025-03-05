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

#include <schematization/triggers/trigger.h>

using namespace rtctools::schematization::triggers;

trigger::trigger(string id, string name, int iYOut, int iTimeTrueOut, int iTimeFalseOut) : element(id, name)
{
	nTrueComponent = 0;
	trueComponent = 0;
	nFalseComponent = 0;
	falseComponent = 0;

	this->iYOut = iYOut;
	this->iTimeTrueOut = iTimeTrueOut;
	this->iTimeFalseOut = iTimeFalseOut;
}

void trigger::addTrueComponent(int n, element **c)
{
	nTrueComponent = n;
	trueComponent = c;
}

void trigger::addFalseComponent(int n, element **c)
{
	nFalseComponent = n;
	falseComponent = c;
}

void trigger::activate()
{
	for (int i=0; i<nTrueComponent; i++) {
		trueComponent[i]->activate();
	}
	for (int i=0; i<nFalseComponent; i++) {
		falseComponent[i]->activate();
	}
}

void trigger::deactivate()
{
	for (int i=0; i<nTrueComponent; i++) {
		trueComponent[i]->deactivate();
	}
	for (int i=0; i<nFalseComponent; i++) {
		falseComponent[i]->deactivate();
	}
}

void trigger::evaluateTimes(double *stateOld, double *stateNew, long long t, double dt)
{
	double yNew = stateNew[iYOut];

	if ((iTimeTrueOut>-1) && (yNew==1.0)) {
		stateNew[iTimeTrueOut] = 0.0;
		if (stateOld[iYOut]==1.0) {
			double dtOld = stateOld[iTimeTrueOut]; 
			if (t==t) stateNew[iTimeTrueOut] = dtOld + dt; 
		}
	}

	if ((iTimeFalseOut>-1) && (yNew==0.0)) {
		stateNew[iTimeFalseOut] = 0.0;
		if (stateOld[iYOut]==0.0) {
			double dtOld = stateOld[iTimeFalseOut]; 
			if (t==t) stateNew[iTimeFalseOut] = dtOld + dt; 
		}
	}
}

void trigger::evaluateSubtriggers(double *stateOld, double *stateNew, long long t, double dt)
{
	double yNew = stateNew[iYOut];

	// evaluate sub-triggers
	if (yNew==yNew) {
		if (yNew==1.0) {
			for (int i=0; i<nTrueComponent; i++) {
				trueComponent[i]->solve(stateOld, stateNew, t, dt);
			}
		} else if (yNew==0.0) {
			for (int i=0; i<nFalseComponent; i++) {
				falseComponent[i]->solve(stateOld, stateNew, t, dt);
			}
		}
	}
}

double trigger::getStatus(double *state)
{
	return state[iYOut];
}

