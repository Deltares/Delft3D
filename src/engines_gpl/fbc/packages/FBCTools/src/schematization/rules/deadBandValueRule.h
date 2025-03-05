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


#pragma once
#ifndef DEADBANDVALUERULE_H
#define DEADBANDVALUERULE_H

#include "rule.h"

namespace rtctools
{
namespace schematization
{
namespace rules
{

class deadBandValueRule : public rule
{
private:
	int yIn;
	double threshold;

public:
	deadBandValueRule(string id, string name, double threshold, int yIn);
	~deadBandValueRule(void);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif /* DEADBANDVALUERULE_H */
