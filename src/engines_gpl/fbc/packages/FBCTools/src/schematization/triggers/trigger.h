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

#ifndef TRIGGER_H
#define TRIGGER_H

#include "schematization/element.h"
#include <stdexcept>

namespace rtctools
{
namespace schematization
{
namespace triggers
{

class trigger : public element
{
protected:
	int nTrueComponent;
	element **trueComponent;
	int nFalseComponent;
	element **falseComponent;

	int iYOut;
	int iTimeTrueOut;
	int iTimeFalseOut;

	trigger(string id, string name, int iYOut, int iTimeTrue, int iTimeFalse);

public:
	virtual ~trigger(void) {};

	void addTrueComponent(int n, element **c);
	void addFalseComponent(int n, element **c);
	void activate();
    void deactivate();

	virtual void solve(double *stateOld, double *stateNew, long long t, double dt) = 0;
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew) {
		throw runtime_error("void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew) not implemented");
	};

	void evaluateTimes(double *stateOld, double *stateNew, long long t, double dt);
	void evaluateSubtriggers(double *stateOld, double *stateNew, long long t, double dt);
	double getStatus(double *state);
};

} // end namespace triggers
} // end namespace schematization
} // end namespace rtctools

#endif /* TRIGGER_H */
