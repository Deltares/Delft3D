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


#include "intervalController.h" 
#include <stdexcept>
#include <algorithm>

using namespace rtctools::schematization::rules;

intervalController::intervalController(string id,
							           string name,
							 		   double settingBelow,
							 		   double settingAbove,
							 		   double settingMaxSpeed,
									   double settingMaxStep,
									   double deadbandSetpointAbsolute,
									   double deadbandSetpointRelative,
									   int iXIn,
									   int iSPIn,
									   int iYOut,
									   int iStatusOut)
	: rule(id, name)
{
	this->settingBelow = settingBelow;
	this->settingAbove = settingAbove;
	this->settingMaxSpeed = settingMaxSpeed;
	this->settingMaxStep = settingMaxStep;
	this->deadbandSetpointAbsolute = deadbandSetpointAbsolute;
	this->deadbandSetpointRelative = deadbandSetpointRelative;
	this->iXIn = iXIn;
	this->iSPIn = iSPIn;
	this->iYOut = iYOut;
	this->iStatusOut = iStatusOut;
}

intervalController::~intervalController(void)
{
}

void intervalController::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	double xOld = stateOld[iXIn];
	double yOld = stateOld[iYOut];
	double setpoint = stateNew[iSPIn];
	double statusOld = stateOld[iStatusOut];

	// evaluate new status
	double statusNew = statusOld;

    if (xOld == xOld) {
        if (deadbandSetpointAbsolute == deadbandSetpointAbsolute)
        {
            if (xOld > setpoint + 0.5 * deadbandSetpointAbsolute)
            {
                statusNew = 1.0;
            }
            else if (xOld < setpoint - 0.5 * deadbandSetpointAbsolute)
            {
                statusNew = -1.0;
            }
            else
            {
                statusNew = 0.0;
            }
        }
        else if (deadbandSetpointRelative == deadbandSetpointRelative)
        {
            double above = setpoint + abs(0.5*setpoint*deadbandSetpointRelative / 100.0);
            double below = setpoint - abs(0.5*setpoint*deadbandSetpointRelative / 100.0);

            if (xOld > above)
            {
                statusNew = 1.0;
            }
            else if (xOld < below)
            {
                statusNew = -1.0;
            }
            else
            {
                statusNew = 0.0;
            }
        }
    }

	// apply settings
	double yNew;
	if (yOld==yOld) {
		yNew = yOld;
	} else {
		yNew = (settingBelow+settingAbove)/2.0;
	}
	if (statusNew == -1.0) {
		yNew = settingBelow;
	} else if (statusNew == 1.0) {
		yNew = settingAbove;
	} 

	// correct for maximum rate of change
	if (settingMaxSpeed==settingMaxSpeed) {
		yNew = max(yNew, yOld - settingMaxSpeed*dt);
		yNew = min(yNew, yOld + settingMaxSpeed*dt);
	} else if (settingMaxStep==settingMaxStep) {
		yNew = max(yNew, yOld - settingMaxStep);
		yNew = min(yNew, yOld + settingMaxStep);
	}

	stateNew[iYOut] = yNew;
	stateNew[iStatusOut] = statusNew;
}

void intervalController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void intervalController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
