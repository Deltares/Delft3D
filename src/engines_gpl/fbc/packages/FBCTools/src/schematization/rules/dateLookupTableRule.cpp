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
 * @author Tobias Schruff, Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */

#include "dateLookupTableRule.h"

#include <utilities/utils.h> 
#include <stdexcept>

using namespace rtctools;
using namespace rtctools::schematization::rules;

dateLookupTableRule::dateLookupTableRule(string id,
										 string name,
										 int nDateRecord,
										 lookupTableConverter **dateRecord,
										 int iXIn,
										 int iYIn,
										 int iYOut) : rule(id, name)
{
	this->nDateRecord = nDateRecord;
	this->dateRecord = dateRecord;
    this->iXIn = iXIn;
    this->iYIn = iYIn;
    this->iYOut = iYOut;
}

void dateLookupTableRule::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double xOld = stateOld[iXIn];
    double yNew;
 
	if (nDateRecord == 1) { 
		// only single record
		yNew = dateRecord[0]->convert(xOld);
	} else {
		// record for each day of the year
		int dayOfYear = utils::getDayOfYear(t);
		yNew = dateRecord[dayOfYear]->convert(xOld);
	} 

	// check for overruling Y
	if (iYIn>-1) {
		double yIn = stateNew[iYIn];
		if (yIn==yIn) {
			yNew = yIn;
		}
	}

	stateNew[iYOut] = yNew;
}

void dateLookupTableRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void dateLookupTableRule::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
