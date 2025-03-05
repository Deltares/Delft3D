// Copyright (C) 2011 Deltares
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

#include "piTimeSeries.h"
#include <sstream>
#include "piDiagInterface.h"

using namespace std;
using namespace rtctools::timeseries;


piTimeSeries::piTimeSeries(string id, int index, string locationID, string parameterID, multiset<string> qualifierIDs, string unit,
						   interpolationOption intOpt, interpolationOption extOpt) :
	id(id), index(index), locationID(locationID), parameterID(parameterID), qualifierIDs(qualifierIDs), unit(unit), intOpt(intOpt), extOpt(extOpt)
{ }

string piTimeSeries::getID() { return id; }

void piTimeSeries::setID(string id)
{
	this->id = string(id);
}

int piTimeSeries::getIndex() { return index; }

void piTimeSeries::setIndex(int index)
{
	this->index = index;
}


string piTimeSeries::getLocationID() { return locationID; };

void piTimeSeries::setLocationID(string locationID)
{
	this->locationID = string(locationID);
}

string piTimeSeries::getParameterID() { return	parameterID; };

void piTimeSeries::setParameterID(string parameterID)
{
	this->parameterID = string(parameterID);
}

string piTimeSeries::getQualifierID() { 
	string qualifierID = "";
	for (multiset<string>::const_iterator i(qualifierIDs.begin()), end(qualifierIDs.end()); i!=qualifierIDs.end(); ++i) {
		qualifierID.append(*i);
	};
	return qualifierID;
};

string piTimeSeries::getUnit() { return	unit; };

string piTimeSeries::toString()
{
	stringstream out;
	out << "id[" << index << "] = '" << id;
	out << "', locationID = '" << locationID << "', parameterID = '" << parameterID;
	// TODO for (int i=0; i<(int)qualifierIDs.size(); i++) { out << qualifierIDs. << ","; }
	out << "'";
	return out.str();
}

interpolationOption piTimeSeries::getInterpolationOption()
{
	return intOpt;
}

interpolationOption piTimeSeries::getExtrapolationOption()
{
	return extOpt;
}
