// Copyright (C) 2014 Deltares
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
 * @date 2014
 */


#ifndef TIMESERIESINTERFACE_H
#define TIMESERIESINTERFACE_H

#include "timeSeriesBasics.h"

namespace rtctools
{
namespace timeseries
{

class timeSeriesInterface : public timeSeriesBasics
{
public:
	timeSeriesInterface();
	timeSeriesInterface(int nTimeStep,
		vector<long long> time,
		int nSeries,
		vector<string> seriesID,
		vector<validationEnum> seriesValidation,
		map<string,int> scalarIDMap,
		map<string,pair<int,int> > vectorIDMap);

	virtual int getNEnsemble() = 0; 
	virtual double getValue(int ensembleIndx, int timeIndx, int seriesIndx) = 0;
	virtual void setValue(int ensembleIndx, int timeIndx, int seriesIndx, double value) = 0;
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESINTERFACE_H */
