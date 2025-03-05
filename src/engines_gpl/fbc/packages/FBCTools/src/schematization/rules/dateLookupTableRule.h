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
 * @author Tobias Schruff, Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */

#ifndef DATELOOKUPTABLERULE_H
#define DATELOOKUPTABLERULE_H

// system includes
#include <string>
// rtctools includes
#include "schematization/rules/rule.h"
#include "utilities/lookupTableConverter.h"

using namespace rtctools::utilities;

namespace rtctools
{
namespace schematization
{
namespace rules
{

class dateLookupTableRule : public rule
{

public:

	/** 
		Default constructor 
	*/
	dateLookupTableRule(string id,
						string name,
						int nDateRecord,
						lookupTableConverter **dateRecord,
						int iXIn,
						int iYIn,
						int iYOut);

	/** 
		Default destructor 
	*/
	~dateLookupTableRule() {};

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew);

private:
	// members
	int nDateRecord;
	lookupTableConverter **dateRecord;
    int iXIn;
    int iYIn;
    int iYOut;  
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif // DATELOOKUPTABLERULE_H
