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
 * @author Jan Mooiman
 * @version 1.0
 * @date 2017
 */

#include "pidControllerPositional.h"
//#include <piDiagInterface.h>
#include <stdexcept>
#include <limits>

#define isnan(a) ((a)!=(a))

using namespace rtctools::schematization::rules;
using namespace std;

/**
 * This method is available for backwards compatibility with Positional. It is obsolete and will
 * be removed in a future release.
 */
pidControllerPositional::pidControllerPositional(string id,
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
							 int iDOut,
							 int numSeries)
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
   _numSeries = numSeries;
    // statics
   this->e_n = numeric_limits<double>::quiet_NaN();
   this->e_nd = numeric_limits<double>::quiet_NaN();
   this->sum_integrator = 0.0;
}

pidControllerPositional::pidControllerPositional(string id,
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
							 int iDOut,
							 int numSeries)
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
   _numSeries = numSeries;
    // statics
   this->e_n = numeric_limits<double>::quiet_NaN();
   this->e_nd = numeric_limits<double>::quiet_NaN();
   this->sum_integrator = 0.0;
}

pidControllerPositional::~pidControllerPositional(void)
{
}

void pidControllerPositional::solve(double *stateOld, double *stateNew, long long t, double dt)
{
    // ..._n  : time level (n)
    // ..._nd : time level (n-1; n-down)

    double u_nd = stateOld[iYOut];
    if (isnan(stateOld[iYOut])) { 
        u_nd = (settingMin + settingMax) / 2.0;
    }

    // error (deviation) from set point
    if (this->iSPIn > -1) {
        this->e_n = stateNew[iSPIn]-stateOld[iXIn];
        if (this->e_nd != this->e_nd) this->e_nd = stateOld[iXIn] - stateOld[iXIn - _numSeries];
    } else {
        this->e_n = this->SPIn-stateOld[iXIn];
        if (isnan(this->e_nd)) this->e_nd = this->SPIn - stateOld[iXIn - _numSeries]; //-1
    }

    // if the values are still NaN then assign them to 0 (old behaviour)
    if (isnan(this->e_nd)) this->e_nd = 0.0;

    this->sum_integrator = this->sum_integrator + dt * this->e_n;
    double u_n = this->kp * this->e_n + this->ki * sum_integrator + this->kd * (this->e_n - this->e_nd)/dt;

    // if there was an old value, check for maximum allowable difference between new and old value
    double du_n = u_n - u_nd;
    double du_max = settingMaxSpeed * dt;
    if (du_n < -du_max) {
        u_n = u_nd - du_max;
    } else if (du_n > du_max) {
        u_n = u_nd + du_max;
    }

    // correct for maximum/minimum limits
    double u_min = this->settingMin;
    double u_max = this->settingMax;
    if (u_n < u_min) {
        u_n = u_min;
    }
    if (u_n > u_max) {
        u_n = u_max;
    }
    // store new values
    stateNew[iYOut] = u_n;
    this->e_nd = this->e_n;
    stateOld[iIOut] = this->sum_integrator;
}

void pidControllerPositional::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
    throw runtime_error("void pidControllerPositional::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
