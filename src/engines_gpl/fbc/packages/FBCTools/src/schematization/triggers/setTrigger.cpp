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

#include <schematization/triggers/setTrigger.h>

#include <limits>

using namespace rtctools::schematization::triggers;

setTrigger::setTrigger(
	string id,
	string name,
	double x1Value,
	int iX1In,
	trigger *t1,
	logicalOperator op,
	double x2Value,
	int iX2In,
	trigger *t2,
	bool yDefaultPresent,
	bool yDefaultValue,
	int iYOut,
	int iTimeTrueOut,
	int iTimeFalseOut) : trigger(id, name, iYOut, iTimeTrueOut, iTimeFalseOut)
{
	this->x1Value = x1Value;
	this->iX1In = iX1In;
	this->t1 = t1;
	this->op = op;
	this->x2Value = x2Value;
	this->iX2In = iX2In;
	this->t2 = t2;
	this->yDefaultPresent = yDefaultPresent;
	this->yDefaultValue = yDefaultValue;
}

setTrigger::~setTrigger(void)
{
}

void setTrigger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double yOld = stateOld[iYOut];

	// check for default value
	double yNew = numeric_limits<double>::quiet_NaN();
	if (yDefaultPresent) {
		if (yDefaultValue) yNew = 1.0; else yNew = 0.0;
	}

	double x1 = x1Value;
	if (iX1In>-1) {
		x1 = stateOld[iX1In];
	} else if (t1!=0) { 
		t1->solve(stateOld, stateNew, t, dt);
		x1 = t1->getStatus(stateNew);
	}

	double x2 = x2Value;
	if (iX2In>-1) {
		x2 = stateOld[iX2In];
	} else if (t2!=0) { 
		t2->solve(stateOld, stateNew, t, dt);
		x2 = t2->getStatus(stateNew);
	}

	// one value should b at least available for OR
	if ((x1==x1) | (x2==x2)) {
		if (op == OR) {
			yNew = (x1==1.0) | (x2==1.0);
		}
	}
	
	// both values should be available for AND and XOR
	if ((x1==x1) & (x2==x2)) {
		if (op == AND) {
			yNew = (x1==1.0) & (x2==1.0);
		} else if (op == XOR) {
			yNew = (x1==1.0) | (x2==1.0);
			if ((x1==1.0) && (x2==1.0)) yNew = 0.0;
		}
	}

	// Write new status
	// Can be a NaN-value
	stateNew[iYOut] = yNew;

	// evaluate true/false times and sub-triggers
	evaluateTimes(stateOld, stateNew, t, dt);
	evaluateSubtriggers(stateOld, stateNew, t, dt);
}
