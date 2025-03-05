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


#include "timeSeriesBasics.h"
#include <assert.h>
#include <stdexcept>

using namespace rtctools::timeseries;


timeSeriesBasics::timeSeriesBasics(vector<long long> time, vector<string> seriesID, 
	vector<validationEnum> seriesValidation, map<string,int> scalarIDMap, 
	map<string,pair<int,int> > vectorIDMap)
	: time(time), seriesID(seriesID), seriesValidation(seriesValidation), scalarIDMap(scalarIDMap), vectorIDMap(vectorIDMap)
{ 
	nTimeStep = (int)time.size();
	nSeries = (int)seriesID.size();
}

long long timeSeriesBasics::getDT(int index) {
	long long dt;
	if (index==0) {
		dt = time[1]-time[0];
	} else {
	    dt = time[index]-time[index-1];
	} 
	return dt;
}

int timeSeriesBasics::getScalarIndex(string s)
{
	// check if id is available
	if (scalarIDMap.find(s)==scalarIDMap.end()) {
		string message = "int timeSeriesBasics::getScalarIndex(string s) - index not found in time series model: " + s;
		throw runtime_error(message.c_str());
	}

	return scalarIDMap[s];
}

int timeSeriesBasics::getScalarIndex(string s, bool implicit)
{
	if (implicit) {
		return this->getScalarIndex(s) + nSeries;
	} else {
		return this->getScalarIndex(s);
	}
}

pair<int,int> timeSeriesBasics::getVectorIndex(string s)
{
	// check if id is available
	if (vectorIDMap.find(s)==vectorIDMap.end()) {
		string message = "int schematisation::getVectorIndex(string s) - index not found in time series model: " + s;
		throw runtime_error(message.c_str());
	}

	return vectorIDMap[s];
}

pair<int,int> timeSeriesBasics::getVectorIndex(string s, bool implicit)
{
	if (implicit) {
		pair<int,int> r = getVectorIndex(s);
		r.first += nSeries;
		return r;
	} else {
		return this->getVectorIndex(s);
	}
}


