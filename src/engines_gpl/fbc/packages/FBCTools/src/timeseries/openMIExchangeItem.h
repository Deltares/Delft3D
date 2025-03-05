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

#ifndef OPENMI_EXCHANGE_ITEM_H
#define OPENMI_EXCHANGE_ITEM_H

#include <string>

using namespace std;

namespace rtctools
{
namespace timeseries
{

class openMIExchangeItem
{
private:
	string id;
	int index;
	string elementID;
	string quantityID;
	string unit;

public:
	openMIExchangeItem();
	openMIExchangeItem(string id, int index, string elementID, string quantityID, string unit);
	~openMIExchangeItem();

	string getID();
	void setID(string id);
	int getIndex();
	void setIndex(int index);
	string getElementID();
	void setElementID(string elementID);
	string getQuantityID();
	void setQuantityID(string quantityID);
	string getUnit();
	void setUnit(string unit);
	string toString();
};

} // end namespace timeseries
} // end namespace rtctools

#endif //OPENMI_EXCHANGE_ITEM_H
