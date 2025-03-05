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

#include <schematization/rtcMerger.h>

#include <limits>

using namespace rtctools::schematization::components;

rtcMerger::rtcMerger(
	string id,
	string name,
	modeEnum mode,
	int nXIn,
	int *iXIn,
	int iYOut) : component(id, name), rule(id, name), trigger(id, name, iYOut, -1, -1)
{
	this->mode = mode;
	this->nXIn = nXIn;
	this->iXIn = new int[nXIn];
	for (int i=0; i<nXIn; i++) {
		this->iXIn[i] = iXIn[i];
	}
}

rtcMerger::~rtcMerger(void)
{
}

void rtcMerger::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double yNew = numeric_limits<double>::quiet_NaN();

	if (mode==DATAHIERARCHY) {
		for (int i=0; i<nXIn; i++) {
			double x = stateOld[iXIn[i]];
			if (x==x) {
				yNew = x;
				break;
			}
		}
	} else if (mode==SUM) {
		yNew = 0.0;
		for (int i=0; i<nXIn; i++) {
			yNew += stateOld[iXIn[i]];
		}
	}

	stateNew[iYOut] = yNew;
}

void rtcMerger::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	if (mode==DATAHIERARCHY) {
		for (int i=0; i<nXIn; i++) {
			double x = stateOld[iXIn[i]];
			if (x==x) {
				dStateOld[iXIn[i]] += dStateNew[iYOut];
				break;
			}
		}
	} else if (mode==SUM) {
		for (int i=0; i<nXIn; i++) {
			// yNew += stateOld[iXIn[i]];
			dStateOld[iXIn[i]] += dStateNew[iYOut];
		}
	}
}
