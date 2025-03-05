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


#ifndef RELATIVE_TIME_CONTROLLER_H
#define RELATIVE_TIME_CONTROLLER_H

#include "schematization/rules/rule.h"
#include <utilities/lookupTableConverter.h>

using namespace rtctools::utilities;

namespace rtctools
{
namespace schematization
{
namespace rules
{

class relativeTimeController : public rule
{
public:
	enum valueOption{RELATIVE, ABSOLUTE};
	enum ruleMode{ NATIVE, RETAINVALUEWHENINACTIVE };

private:
	double tActive;
	double tMaximum;
	double dt;
	lookupTableConverter *converter;
	valueOption vOpt;
	int iYIn;
	int iYOut;
	int iTimeActiveOut;
	ruleMode vRuleMode;

    // SOBEK - specific
	double tableResetTime;
	double staTime;
	bool ruleCanBeActivatedForFirstTime; // state keeping for (re)activition of rule in the current run
	bool firstIteration;

    struct TimeActiveState
	{
		long long time;
		bool active;
	};

	TimeActiveState timeActiveStates[3];
	int timeActiveStatesIndex;

public:
	relativeTimeController(string id, string name, lookupTableConverter *converter, 
		valueOption vOpt, double tMaximum, int iYIn, int iYOut, int iTimeActiveOut,
		ruleMode = NATIVE);
	~relativeTimeController(void) {};

	void deactivate();
	void stateTransfer(double *stateOld, double *stateNew, long long t, double dt);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);

	void updateTimeActiveState(long long t, bool);
    bool wasActiveInPreviousTimeStep(long long t);

    virtual int getIYOut() const override  { return iYOut; }
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif // RELATIVE_TIME_CONTROLLER_H
