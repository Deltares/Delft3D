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

#ifndef LOOKUP_TABLE_H
#define LOOKUP_TABLE_H

#include "schematization/components/component.h"
#include "schematization/rules/rule.h"
#include "schematization/triggers/trigger.h"
#include "utilities/converter.h"

using namespace rtctools::schematization::components;
using namespace rtctools::schematization::rules;
using namespace rtctools::schematization::triggers;
using namespace rtctools::utilities;

namespace rtctools
{
namespace schematization
{

/**
  * @brief One-dimensional lookup table
  */
class lookupTable : public component, public rule, public trigger
{
private:
	/**
	  * @brief Pointer to the lookup table
	  */
	converter *conv;
	/**
	  * @brief Index of index time series
	  */
	int iXIn;
	/**
	  * @brief Index of optional time series for overruling the output
	  *
	  * This time series is optional and overrules the output of the lookup
	  * table if being available. This means if it is configures and 
	  * includes valid data.
	  */
	int iYIn;
	/**
	  * @brief Index of output time series
	  */
	int iYOut;

public:
	/**
	  * Constructor
	  */
	lookupTable(string id, string name, converter *conv, int iXIn, int iYIn, int iYOut);
	/**
	  * Constructor
	  *
	  * @param id		Identifyer
	  * @param name		Name
	  * @param conv		Pointer to lookup table 
	  * @param iXIn		Input variable
	  * @param iYIn		Optional input for overruling the output of the lookup table
	  * @param iYOut	Output variable
	  */
	~lookupTable(void);

	void solve(double *stateOld, double *stateNew, long long t, double dt);
	void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *objOld, double *objNew);
    virtual int getIYOut() const override { return iYOut; }
};

} // end namespace schematization
} // end namespace rtctools

#endif //LOOKUP_TABLE_H
