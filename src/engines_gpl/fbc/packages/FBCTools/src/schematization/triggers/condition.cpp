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

#include "condition.h"
#include <limits>

using namespace rtctools::schematization::triggers;
using namespace std;

condition::condition()
{
}

condition::condition(double x1Value,
					 int iX1In,
					 relationalOperator op,
					 double x2Value,
					 int iX2In)
{
	this->x1Value = x1Value;
	this->iX1In = iX1In;
	this->op = op;
	this->x2Value = x2Value;
	this->iX2In = iX2In;
}

condition::~condition(void)
{
}

double condition::evaluate(double *stateOld, double *stateNew) 
{
	// preset output with a NaN-value
	double y = numeric_limits<double>::quiet_NaN();

	// Retrieve condition parameters
	double x1;
	double x2;
	if (iX1In>-1) x1 = stateOld[iX1In]; else x1 = x1Value;
	if (iX2In>-1) x2 = stateOld[iX2In]; else x2 = x2Value;

	// Evaluate new trigger status
	if ((x1==x1) && (x2==x2)) {
		if (op==GREATER) { 
			if (x1>x2) y = 1.0; else y = 0.0;
		} else if (op==GREATEREQUAL) {
			if (x1>=x2) y = 1.0; else y = 0.0;
		} else if (op==EQUAL) {
			if (x1==x2) y = 1.0; else y = 0.0;
		} else if (op==UNEQUAL) {
			if (x1!=x2) y = 1.0; else y = 0.0;
		} else if (op==LESSEQUAL) {
			if (x1<=x2) y = 1.0; else y = 0.0;
		} else if (op==LESS) {
			if (x1<x2) y = 1.0; else y = 0.0;
		} 
	}

	// UNEQUAL condition also works for a single NaN value
	if ((x1==x1) || (x2==x2)) {
		if (op==UNEQUAL) {
			if (x1!=x2) y = 1.0; else y = 0.0;
		}
	}

	return y;
}
