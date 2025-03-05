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

#ifndef RTCMERGER_H
#define RTCMERGER_H

#include <schematization/triggers/trigger.h>
#include <schematization/rules/rule.h>
#include <schematization/components/component.h>

using namespace rtctools::schematization::rules;
using namespace rtctools::schematization::triggers;

namespace rtctools
{
namespace schematization
{
namespace components
{

class rtcMerger : public trigger, public rule, public component
{
public:
	enum modeEnum
	{
		DATAHIERARCHY,
		SUM
	};

private:
	modeEnum mode;
	int nXIn;
	int *iXIn;

public:
	rtcMerger(string id, string name,
		      modeEnum mode, int nXIn, int *iXIn, int iYOut);
	~rtcMerger(void);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
    void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew);
};

} // end namespace components
} // end namespace schematization
} // end namespace rtctools

#endif /* RTCMERGER_H */
