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

#pragma once
#ifndef PIDSOBEK2_CONTROLLER_H
#define PIDSOBEK2_CONTROLLER_H

#include "rule.h"

namespace rtctools
{
namespace schematization
{
namespace rules
{

 /**
 * @brief SOBEK RUR Implementation by Jaap Zeekant
 * 
 * This method is available for backwards compatibility with SOBEK2. It is obsolete and will 
 * be removed in a future release.
 */
class pidControllerSobek2 : public rule
{
private:
	/**
	 * @brief factor for proportional part
	 */
	double kp;

	/**
	 * @brief factor for integral part
	 */
	double ki;

	/**
	 * @brief factor for differential part
	 */
	double kd;

	/**
	 * @brief factor for optional disturbance
	 */
	double kf;

	/**
	 * @brief minimum setting of actuator
	 */
	double settingMin;

	/**
	 * @brief maximum setting of actuator
	 */
	double settingMax;

	/**
	 * @brief maximum setting of actuator
	 */
	double settingMaxSpeed;

	/**
	 * @brief index of time series with controlled variable
	 */
	int iXIn;

	/**
	 * @brief index of time series with set point
	 */
	int iSPIn;

	/**
	 * @brief constant set point
	 */
	double SPIn;

	/**
	 * @brief index of time series with optional disturbance
	 */
	int iFIn;

	/**
	 * @brief index of time series with actuator output
	 */
	int iYOut;

	/**
	 * @brief index of time series with integral part
	 */
	int iIOut;

	/**
	 * @brief index of time series with the last difference between controlled variable and set point for computing the differential part
	 */
	int iDOut;

public:
	/**
	 * @brief Constructor for pid controller with variable set point
	 *
 	 * @param id id
	 * @param name name
	 * @param kp factor of proportional part
	 * @param ki factor of integral part
	 * @param kd factor of differential part
	 * @param kf factor of disturbance term
	 */
	pidControllerSobek2(string id, string name, double p, double i, double d, double f,
		double settingMin, double settingMax, double settingMaxSpeed,
		int iXIn, int iSPIn, int iFIn, int iYOut, int iIOut, int iDOut);
	pidControllerSobek2(string id, string name, double p, double i, double d, double f,
		double settingMin, double settingMax, double settingMaxSpeed,
		int iXIn, double SPIn, int iFIn, int iYOut, int iIOut, int iDOut);
	~pidControllerSobek2(void);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);
    virtual int getIYOut() const override { return iYOut; }
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif // PIDSOBEK2_CONTROLLER_H
