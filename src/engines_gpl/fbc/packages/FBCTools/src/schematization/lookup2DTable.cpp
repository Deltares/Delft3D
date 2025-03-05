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


#include "lookup2DTable.h" 
#include <stdexcept>

using namespace rtctools;
using namespace rtctools::schematization;

lookup2DTable::lookup2DTable(string id,
							 string name,
							 lookup2DTableConverter *converter,
							 int iXIn,
							 int iYIn,
							 int iZIn,
							 int iZOut)
	: component(id, name), rule(id, name)
{
	this->converter = converter;
	this->iXIn = iXIn;
	this->iYIn = iYIn;
	this->iZIn = iZIn;
	this->iZOut = iZOut;
}

void lookup2DTable::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double x = stateOld[iXIn];
	double y = stateOld[iYIn];
	double z = converter->convert(x, y);

	if (iZIn>-1) {
		double zEx = stateNew[iZIn];
		if (zEx==zEx) {
			z = zEx;
		}
	}

	stateNew[iZOut] = z;
}

void lookup2DTable::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void lookup2DTableRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
