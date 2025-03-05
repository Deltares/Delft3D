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

#ifndef POLYGON_TRIGGER_H
#define POLYGON_TRIGGER_H

#include <schematization/triggers/trigger.h>

#include <schematization/triggers/polygon.h>
#include <vector>

namespace rtctools
{
namespace schematization
{
namespace triggers
{

class polygonTrigger : public trigger
{
private:
	int iX1In;
	int iX2In;
	vector<polygon> polygons;
	double yDefaultValue;

public:
	polygonTrigger(string id, string name, int iX1In, int iX2In, int iYOut,
		vector<polygon> polygons, double yDefaultValue, int iTimeTrueOut, int iTimeFalseOut);
	~polygonTrigger(void) {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
};

} // end namespace triggers
} // end namespace schematization
} // end namespace rtctools

#endif //POLYGON_TRIGGER_H
