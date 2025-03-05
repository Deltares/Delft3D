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

#include "openMIExchangeItem.h"

#include <sstream>

using namespace std;
using namespace rtctools::timeseries;

openMIExchangeItem::openMIExchangeItem()
{
}

openMIExchangeItem::openMIExchangeItem(string id, int index, string elementID, string quantityID, string unit)
{
	this->id = string(id);
	this->index = index;
	this->elementID = string(elementID);
	this->quantityID = string(quantityID);
	this->unit = string(unit);
}

openMIExchangeItem::~openMIExchangeItem()
{
}

string openMIExchangeItem::getID() { return id; }

void openMIExchangeItem::setID(string id) { this->id = string(id); }

int openMIExchangeItem::getIndex() { return index; }

void openMIExchangeItem::setIndex(int index) { this->index = index; }

string openMIExchangeItem::getElementID() { return elementID; }

void openMIExchangeItem::setElementID(string elementID) { this->elementID = string(elementID); }

string openMIExchangeItem::getQuantityID() { return	quantityID; }

void openMIExchangeItem::setQuantityID(string quantityID) { this->quantityID = string(quantityID); }

string openMIExchangeItem::getUnit() { return unit; }

void openMIExchangeItem::setUnit(string unit) { this->unit = string(unit); }

string openMIExchangeItem::toString()
{
	stringstream out;
	out << "id[" << index << "] = '" << id;
	out << "', elementID = '" << elementID << "', quantityID = '" << quantityID << "'";
	return out.str();
}
