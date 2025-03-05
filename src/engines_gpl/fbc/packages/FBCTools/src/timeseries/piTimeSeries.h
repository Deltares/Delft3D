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


#ifndef PITIMESERIES_H
#define PITIMESERIES_H

#include <string>
#include <iostream>
#include "rtcToolsEnums.h"
#include <set>

using namespace std;

namespace rtctools
{
namespace timeseries
{

class piTimeSeries
{
private:
	string id;
	int index;
	string locationID;
	string parameterID;
	multiset<string> qualifierIDs;
	string unit;
	string comment;
	interpolationOption intOpt;
	interpolationOption extOpt;

public:
	piTimeSeries() {};
	piTimeSeries(string id, int index, string locationID, string parameterID, multiset<string> qualifierIDs, string unit,
		interpolationOption intOpt, interpolationOption extOpt);
	~piTimeSeries() {};

	string getID();
	void setID(string id);
	int getIndex();
	void setIndex(int index);

	string getLocationID();
	void setLocationID(string locationID);
	string getParameterID();
	void setParameterID(string parameterID);
	multiset<string> getQualifierIDs() { return qualifierIDs; };
	void addQualifierID(string newQualifierID) { qualifierIDs.insert(newQualifierID); };
	void setQualifierID(multiset<string> newQualifierIDs) { qualifierIDs = newQualifierIDs; };
	string getQualifierID();
	string getUnit();
	void setComment(string comment) { this->comment = comment; };
	string getComment() { return comment; };

	interpolationOption getInterpolationOption();
	interpolationOption getExtrapolationOption();

	string toString();
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* PITIMESERIES_H */
