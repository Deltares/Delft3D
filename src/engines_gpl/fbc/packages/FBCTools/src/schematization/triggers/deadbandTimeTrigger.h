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

#ifndef DEADBANDTIMETRIGGER_H
#define DEADBANDTIMETRIGGER_H

#include <schematization/triggers/condition.h>
#include <schematization/triggers/trigger.h>
#include <schematization/rules/rule.h>

using namespace rtctools::schematization::triggers;

namespace rtctools
{
namespace schematization
{
namespace rules
{

class deadbandTimeTrigger : public trigger, public rule
{
private:
	int nStepUp;
	int nStepDown;
	int iXIn;
	int iStepsUpOut;
    int iStepsDownOut;

public:
	deadbandTimeTrigger(string id, string name,
	    int nStepUp, int nStepDown,
	    int iXIn, int iStepsUpOut, int iStepsDownOut,
	    int iYOut, int iTimeTrueOut, int iTimeFalseOut);
	~deadbandTimeTrigger(void);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double* stateOld, double* stateNew, long long t, double dt,
				  double* dStateOld, double* dStateNew);
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif /* DEADBANDTIMETRIGGER */
