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


#include "absoluteTimeController.h" 
#include <stdexcept>

using namespace rtctools::schematization::rules;

absoluteTimeController::absoluteTimeController(string id,
							                   string name,
							                   int iXIn,
							                   int iYOut)
	: rule(id, name)
{
	this->iXIn = iXIn;
	this->iYOut = iYOut;
}

absoluteTimeController::~absoluteTimeController(void)
{
}

void absoluteTimeController::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	stateNew[iYOut] = stateNew[iXIn];
}

void absoluteTimeController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void absoluteTimeController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
