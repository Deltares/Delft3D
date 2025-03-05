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


#ifndef TIMESERIESBASICS_H
#define TIMESERIESBASICS_H

#include <string>
#include <map>
#include <vector>
using namespace std;

namespace rtctools
{
namespace timeseries
{

enum validationEnum {
	VALIDATION_NO,
	VALIDATION_STATE,
	VALIDATION_UPDATE,
	VALIDATION_UPDATE_EXCEPT_STATE,
	VALIDATION_FORECAST,
	VALIDATION_FORECAST_EXCEPT_T0,
	VALIDATION_ALL,
	VALIDATION_ALL_EXCEPT_STATE
};

class timeSeriesBasics
{
protected:
	int nTimeStep;
	vector<long long> time;
	int nSeries;
	vector<string> seriesID;
	vector<validationEnum> seriesValidation;
	map<string,int> scalarIDMap;
	map<string,pair<int,int> > vectorIDMap;

public:
	timeSeriesBasics() {};
	timeSeriesBasics(vector<long long> time, vector<string> seriesID,
		vector<validationEnum> seriesValidation, map<string,int> scalarIDMap, 
		map<string,pair<int,int> > vectorIDMap);
	virtual ~timeSeriesBasics(void) {};

	// time related
	int getNTimeStep() { return nTimeStep; };
	long long getStartTime() { return time[0]; };
	long long getEndTime() { return time[nTimeStep-1]; };
	long long getTime(int index) { return time[index]; };
	vector<long long> &getTimes() { return time; };
	long long getDT() { return getDT(1); };
	long long getDT(int tIndex);

	// time series related
	int getNSeries() { return nSeries; };
	vector<string> getSeriesIDs() { return seriesID; };
	map<string,int> getScalarIDMap() { return scalarIDMap; }
	map<string,pair<int,int> > getVectorIDMap() { return vectorIDMap; }
	int getScalarIndex(string s);
	int getScalarIndex(string s, bool implicit);
	pair<int,int> getVectorIndex(string s);
	pair<int,int> getVectorIndex(string s, bool implicit);
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESBASICS_H */
