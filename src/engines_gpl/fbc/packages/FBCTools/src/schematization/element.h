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

#ifndef ELEMENT_H
#define ELEMENT_H

#include <string>
#include <cmath>
#include <iostream>

using namespace std;

namespace rtctools
{
namespace schematization
{

/**
  * @brief Base class for all modeling element
  */
class element
{
private:
	/**
	  * @brief Unique identifyer
	  */
	string id;
	/**
	  * @brief Name of the element
	  */
	string name;

protected:
	/**
	  * @brief Activity status of element
	  */
	bool active;

    /**
    * @brief column index where to write the state (active/inactive)
    */
    int isActiveOutPosition = -1;

	/**
	  * @brief Constructor
	  */
	element(string id, string name);
	/**
	  * @brief Destructor
	  */
	~element(void);

public:
	/**
	  * @brief Get element identifier
	  */
	string getID();
	/**
	  * @brief Get element name
	  */
    string getName();

	/**
	  * @brief Activation of element (relevant for rules)
	  */
	virtual void activate() = 0;
	/**
	  * @brief Deactivation of element (relevant for rules)
	  */
    virtual void deactivate() = 0;
	/**
	  * @brief Check if element is active
	  */
	bool isActive();

	/**
	  * @brief Simulation mode to progress a time step forward in time
	  *
	  * @param stateOld		Vector with old system state (read only !!!)
	  * @param stateNew		Vector with new system state
	  * @param t			Time in milli seconds after 1970
	  * @param dt			Time step [s]
	  */
	virtual void solve(double *stateOld, double *stateNew, long long t, double dt) = 0;

	/**
	  * @brief Reverse adjoint mode to progress a time step backwards in time
	  *
	  * @param stateOld		Vector with old system state (read only !!!)
	  * @param stateNew		Vector with new system state
	  * @param t			Time in milli seconds after 1970
	  * @param dt			Time step [s]
	  * @param dStateOld	Vector with old adjoint state
	  * @param dStateNew	Vector with new adjoint state
	  */
    virtual void solveDer(double *stateOld, double *stateNew, long long t, double dt, double *dStateOld, double *dStateNew) = 0;
};

} // end namespace schematization
} // end namespace rtctools

#endif /* ELEMENT_H */
