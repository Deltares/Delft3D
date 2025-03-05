// Copyright (C) 2011 Deltares
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


#include "relativeTimeController.h"
#include <limits> 
#include <stdexcept>

#define isnan(a) ((a)!=(a))

using namespace rtctools::schematization::rules;
using namespace rtctools::utilities;
using namespace rtctools;

relativeTimeController::relativeTimeController(string id, 
											   string name, 
											   lookupTableConverter *converter,
											   valueOption vOpt,
											   double tMaximum,
											   int iYIn,
											   int iYOut,
											   int iTimeActiveOut,
											   ruleMode vRuleMode)
	: rule(id, name)
{
	this->converter = converter;
	this->vOpt = vOpt;
	this->tMaximum = tMaximum;
	this->iYIn = iYIn;
	this->iYOut = iYOut;
	this->iTimeActiveOut = iTimeActiveOut;
	this->vRuleMode = vRuleMode;

	// A number of fields have been added here. The reason behind this is twofold:
	// 1 - both relative time rules (the RTCTools native one and the SOBEK2-specific one) have been merged
	// 2 - restart functionality has been added

	ruleCanBeActivatedForFirstTime = true; // when this becomes false it means the rule has been activated in the current run
	tActive = numeric_limits<double>::quiet_NaN();
	staTime = numeric_limits<double>::quiet_NaN();
	tableResetTime = numeric_limits<double>::quiet_NaN();
	firstIteration = true;

	for (int i=0; i<3; i++)
	{
		timeActiveStates[i].time = numeric_limits<long long>::quiet_NaN();
		timeActiveStates[i].active = false;
	}
    timeActiveStatesIndex = -1;
}

void relativeTimeController::deactivate() { 
	active = false;
	// increment active time, if in absolute mode and within the maximum time of the table
	if (vRuleMode != RETAINVALUEWHENINACTIVE && vOpt==ABSOLUTE && tActive+dt<=tMaximum) {
		tActive += dt;
	} else {
        tActive = numeric_limits<double>::quiet_NaN();
	}
}

void relativeTimeController::stateTransfer(double *stateOld, double *stateNew, long long t, double dt) { 
	if (firstIteration && stateOld[iTimeActiveOut]==stateOld[iTimeActiveOut])
	{
		ruleCanBeActivatedForFirstTime = false; // we are restarting
	}
    // record time step t in timeActiveStates. Assume rule is false; this might get overwritten in the solve method
    updateTimeActiveState(t, false);
	firstIteration = false;
    stateNew[iTimeActiveOut] = stateOld[iTimeActiveOut] + dt;
}

void relativeTimeController::updateTimeActiveState(long long t, bool active)
{
    bool updated = false;
	for (int i=0; i < 3; i++)
	{
		if (timeActiveStates[i].time == t)
		{
			if (timeActiveStates[i].active == active)
		    {
			    return;
		    }
            timeActiveStates[i].active = active;
            updated = true;
            break;
        }
    }
    if (!updated) // a new time step is added
    {
        timeActiveStatesIndex++;
        timeActiveStatesIndex %= 3;
        timeActiveStates[timeActiveStatesIndex].time = t;
        timeActiveStates[timeActiveStatesIndex].active = active;
    }
}

bool relativeTimeController::wasActiveInPreviousTimeStep(long long t)
{
    int findIndex = -1;
    for (int i=0; i<3; i++)
    {
        if (timeActiveStates[i].time == t)
        {
            findIndex = i;
            break;
        }
    }
    if (findIndex == -1) return false;

    findIndex += 2;
    findIndex %= 3;
    return (timeActiveStates[findIndex].active && t > timeActiveStates[findIndex].time);
}

void relativeTimeController::solve(double *stateOld, double *stateNew, long long t, double dt)
{
    if (vRuleMode == NATIVE)
    {
        // hack for making dt available in the deactivate function
        this->dt = dt;

        // time
        if (stateOld[iTimeActiveOut] == stateOld[iTimeActiveOut])
        {
            tActive = stateOld[iTimeActiveOut] + dt;
        }

        if (tActive != tActive)
        {
            // rule is activated the first time
            if (vOpt == ABSOLUTE)
            {
                tActive = 0.0;
            }
            else if (vOpt == RELATIVE)
            {
                double y = stateOld[iYOut];
                if (iYIn > -1 && stateOld[iYIn] == stateOld[iYIn])
                {
                    y = stateOld[iYIn];
                }
                if (y == y)
                {
                    tActive = converter->reverseConvert(y);
                }
                else
                {
                    tActive = 0.0;
                }
            }
        }
    }
	else
	{
        updateTimeActiveState(t, true);

		double t_secs = (double)(t / 1000);
		// time
		if (stateOld[iTimeActiveOut]==stateOld[iTimeActiveOut]) {
			tActive = stateOld[iTimeActiveOut] + dt;
		}
		if (vOpt == ABSOLUTE) 
        {
            // rule is (re)activated
		    if (ruleCanBeActivatedForFirstTime || !wasActiveInPreviousTimeStep(t)) 
            { 
				if (staTime!=staTime) 
                {
					if (tActive == tActive) 
                    {
						staTime = t_secs - tActive;
					}
					else 
                    {
						staTime = t_secs - tMaximum;
					}
				}
				if (tActive!=tActive || !wasActiveInPreviousTimeStep(t)) {
					if (t_secs >= staTime + tMaximum) {
						staTime = t_secs;
					}
				}
				tActive = t_secs - staTime;
			}
		} 
	    else if (vOpt == RELATIVE) 
        {
			if (ruleCanBeActivatedForFirstTime || !wasActiveInPreviousTimeStep(t)) 
            { // rule is (re)activated
				if (isnan(tableResetTime)) 
                {
					tableResetTime = t_secs - tMaximum;
				}
				if (t_secs >= tableResetTime + tMaximum) 
                {
					double y = stateOld[iYOut];
                    if (iYIn > -1 && stateOld[iYIn] == stateOld[iYIn])
                    {
                        y = stateOld[iYIn];
                    }
					if (y==y)
                    {
						tActive = converter->reverseConvert(y);
					} 
				    else 
                    {
						tActive = 0.0;
					}
					staTime = t_secs - tActive;
					tableResetTime = t_secs;
					double tableStart = converter->convert(0.0);
				}
				tActive = t_secs - staTime;
			}
		}
        ruleCanBeActivatedForFirstTime = false; // if you pass here rule has been activated for the first time
	}
    // value
    double yNew = converter->convert(tActive);
    stateNew[iYOut] = yNew;
    stateNew[iTimeActiveOut] = tActive;
}

void relativeTimeController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void relativeTimeController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
