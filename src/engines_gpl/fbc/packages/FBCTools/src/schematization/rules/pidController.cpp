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


#include "pidController.h" 
#include <stdexcept>
#include <algorithm>

using namespace rtctools::schematization::rules;

pidController::pidController(string id,
							 string name,
							 double kp,
							 double ki,
							 double kd,
							 double kf,
							 double settingMin,
							 double settingMax,
							 double settingMaxSpeed,
							 int iXIn,
							 int iSPIn,
							 int iFIn,
							 int iYOut,
							 int iIOut,
							 int iDOut)
	: rule(id, name)
{
	this->kp = kp;
	this->ki = ki;
	this->kd = kd;
	this->kf = kf;
	this->settingMin = settingMin;
	this->settingMax = settingMax;
	this->settingMaxSpeed = settingMaxSpeed;
	this->iXIn = iXIn;
	this->iSPIn = iSPIn;
	this->SPIn = -1.0;
	this->iFIn = iFIn;
	this->iYOut = iYOut;
	this->iIOut = iIOut;
	this->iDOut = iDOut;
}

pidController::pidController(string id,
							 string name,
							 double kp,
							 double ki,
							 double kd,
							 double kf,
							 double settingMin,
							 double settingMax,
							 double settingMaxSpeed,
							 int iXIn,
							 double SPIn,
							 int iFIn,
							 int iYOut,
							 int iIOut,
							 int iDOut)
	: rule(id, name)
{
	this->kp = kp;
	this->ki = ki;
	this->kd = kd;
	this->kf = kf;
	this->settingMin = settingMin;
	this->settingMax = settingMax;
	this->settingMaxSpeed = settingMaxSpeed;
	this->iXIn = iXIn;
	this->iSPIn = -1;
	this->SPIn = SPIn;
	this->iFIn = iFIn;
	this->iYOut = iYOut;
	this->iIOut = iIOut;
	this->iDOut = iDOut;
}

void pidController::solve(double *stateOld, double *stateNew, long long t, double dt)
{
	// optional disturbance
	double f = 0.0;
	if (iFIn>-1) {
		f = stateOld[iFIn];
		if (f!=f) f = 0.0;
	}

	// deviation from set point
	double e;
	if (iSPIn>-1) {
		e = stateNew[iSPIn] - stateOld[iXIn];
	} else {
		e = SPIn - stateOld[iXIn];
	}

	// differential part
	double dedt = 0.0;
	if ((stateOld[iDOut]==stateOld[iDOut]) & (kd!=0.0)) {
		dedt = (e-stateOld[iDOut])/dt;
	}

	// initialization of integral part if not available
	double yOld = stateOld[iYOut];
	double eInt = stateOld[iIOut];
	if (eInt!=eInt) {
		if ((ki!=0.0) && (yOld==yOld)) {
			eInt = (yOld - settingMax - kp*e - kd*dedt - kf*f) / ki;
		} else {
			eInt = 0.0;
		}
	}
	eInt += dt*e;

	// execution of pid controller
	double yNew = settingMax + kp*e + ki*eInt + kd*dedt + kf*f;

	// correct for maximum rate of change
	double sMin = settingMin;
	double sMax = settingMax;
	if (yOld==yOld) {
		// indirect correction of rate of change by modifying min / max settings
		// therefore, it is considered in the wind-up correction
		sMin = max(yOld - dt*settingMaxSpeed, settingMin);
		sMax = min(yOld + dt*settingMaxSpeed, settingMax);
	} 

	// check for min / max bounds and apply wind-up correction
    if (yNew<sMin) {
		if (ki!=0.0) eInt = (sMin - settingMax - kp*e - kd*dedt - kf*f) / ki; 
        yNew = sMin;
    } else if (yNew>sMax) {
		if (ki!=0.0) eInt = (sMax - settingMax - kp*e - kd*dedt - kf*f) / ki; 
        yNew = sMax;
    }

	stateNew[iYOut] = yNew;
	stateNew[iIOut] = eInt;
	stateNew[iDOut] = e;
}

void pidController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void pidController::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
