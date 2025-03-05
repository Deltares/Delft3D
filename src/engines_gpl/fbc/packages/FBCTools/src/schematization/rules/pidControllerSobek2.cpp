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
 * @author Jaap Zeekant
 * @version 1.0
 * @date 2010
 */

#include "pidControllerSobek2.h" 
#include <stdexcept>

using namespace rtctools::schematization::rules;

/**
 * @brief SOBEK RUR Implementation by Jaap Zeekant
 * 
 * This method is available for backwards compatibility with SOBEK2. It is obsolete and will 
 * be removed in a future release.
 */
pidControllerSobek2::pidControllerSobek2(string id,
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

pidControllerSobek2::pidControllerSobek2(string id,
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

pidControllerSobek2::~pidControllerSobek2(void)
{
}

void pidControllerSobek2::solve(double *stateOld, double *stateNew, long long t, double dt)
{
    // deviation from set point
	double diffFromSetP;
	if (iSPIn>-1) {
		diffFromSetP = stateNew[iSPIn]-stateOld[iXIn];
	} else {
		diffFromSetP = SPIn-stateOld[iXIn];
	}

   // differential part
   double diffFromSetPOld = stateOld[iDOut];
   if (diffFromSetPOld!=diffFromSetPOld) {
      diffFromSetPOld = diffFromSetP;
   }

   // HACK by Alex Koster and Jaap Zeekant
   // usold!=usold means no old value available. As a first guess take average of limits.
   // TODO: Supply old value from structure data as in SOBEK-RUR. 
   double usold = stateOld[iYOut];
   if (usold != usold) {   
      usold = (settingMin + settingMax) / 2.0;
   }

   // initialization of integral part if not available
   double diffFromSetPSum = stateOld[iIOut];
   if (diffFromSetPSum==diffFromSetPSum) {   // Check for NaN's
      diffFromSetPSum = diffFromSetPSum + diffFromSetP;
   } else {
	   diffFromSetPSum = 0.0;
   }

   // execution of pid controller
   double us = usold + kp * diffFromSetP + ki * diffFromSetPSum + kd * (diffFromSetP - diffFromSetPOld);
   double dus;
   double dusmax;

   // if there was an old value, check for maximum allowable difference between new and old value
   if (usold==usold) {
      dus = us - usold;
      dusmax = settingMaxSpeed * dt;
      if (dus < -dusmax) {
         us = usold - dusmax;
      } else if (dus > dusmax) {
         us = usold + dusmax;
      }
   }

   // correct for maximum/minimum limits
   double usmin = settingMin;
   double usmax = settingMax;
   if (us < usmin) {
      us = usmin;
      diffFromSetPSum = diffFromSetPSum - diffFromSetP;
   }
   if (us > usmax) {
      us = usmax;
      diffFromSetPSum = diffFromSetPSum - diffFromSetP;
   }

   // store new values
   stateNew[iYOut] = us;
   stateNew[iIOut] = diffFromSetPSum;
   stateNew[iDOut] = diffFromSetP;
}

void pidControllerSobek2::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew)
{
	throw runtime_error("void pidControllerSobek2::solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) not implemented");
}
