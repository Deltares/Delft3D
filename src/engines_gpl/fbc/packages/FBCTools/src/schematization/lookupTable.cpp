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

#include "lookupTable.h"

using namespace rtctools::utilities;
using namespace rtctools::schematization;

lookupTable::lookupTable(string id,
						 string name,
						 converter *conv,
						 int iXIn,
						 int iYIn,
						 int iYOut)
	: component(id, name), rule(id, name), trigger(id, name, -1, -1, -1)
{
	this->conv = conv;
	this->iXIn = iXIn;
	this->iYIn = iYIn;
	this->iYOut = iYOut;
}

lookupTable::~lookupTable(void)
{
	// TODO
	delete this->conv;
}

void lookupTable::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double y = conv->convert(stateOld[iXIn]);

	if (iYIn>-1) 
    {
		double yEx = stateNew[iYIn];
		if (yEx==yEx) 
        {
			y = yEx;
		}
	}

	stateNew[iYOut] = y;
}

void lookupTable::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	if (stateNew!=0) {
		if (iYIn>-1) {
			double yEx = stateNew[iYIn];
			if (yEx==yEx) {
				dStateNew[iYIn] += dStateNew[iYOut];
			} else {
				dStateOld[iXIn] += dStateNew[iYOut]*conv->convertDer(stateOld[iXIn]);
			}
		} else {
			dStateOld[iXIn] += dStateNew[iYOut]*conv->convertDer(stateOld[iXIn]);
		}
	}
}
