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

#ifndef STANDARD_TRIGGER_H
#define STANDARD_TRIGGER_H

#include <schematization/triggers/trigger.h>
#include <schematization/triggers/condition.h>
#include <utilities/utils.h>

namespace rtctools
{
namespace schematization
{
namespace triggers
{

class standardTrigger : public trigger
{
private:
	condition con;
	bool yDefaultPresent;
	bool yDefaultValue;

public:
	standardTrigger(string id, string name, 
		condition con, bool yDefaultPresent, bool yDefaultValue,
		int iYOut, int iTimeTrueOut, int iTimeFalseOut);
	~standardTrigger(void);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
};

} // end namespace triggers
} // end namespace schematization
} // end namespace rtctools

#endif //STANDARD_TRIGGER_H
