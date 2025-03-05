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

#include <schematization/rules/constantRule.h> 

using namespace rtctools::schematization::rules;

constantRule::constantRule(string id, string name, double constant, int yOut)
	: rule(id, name)
{
	this->constant = constant;
	this->yOut = yOut;
}

constantRule::~constantRule(void)
{
}

void constantRule::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	stateNew[yOut] = constant;
}

void constantRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	dStateOld[yOut] = 0.0;
}
