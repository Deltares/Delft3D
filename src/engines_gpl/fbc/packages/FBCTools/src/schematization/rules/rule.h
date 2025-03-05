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

#ifndef RULE_H
#define RULE_H

#include "schematization/element.h"

using namespace std;

namespace rtctools
{
namespace schematization
{
namespace rules
{

class rule : public element
{
protected:
    rule(string id, string name);

public:
    virtual ~rule(void);

    void activate();
    void deactivate();
    virtual void stateTransfer(double*, double*, long long, double);
    // The defalt index for the output is -1 (not a valid index)
    virtual int getIYOut() const { return -1; }
};

} // end namespace rules
} // end namespace schematization
} // end namespace rtctools

#endif // RULE_H