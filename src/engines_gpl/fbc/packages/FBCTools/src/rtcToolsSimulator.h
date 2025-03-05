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

#ifndef RTCTOOLS_SIMULATOR_H
#define RTCTOOLS_SIMULATOR_H

#include "schematization/schematisation.h"
#include "timeseries/timeSeriesTensorInterface.h"
#include "timeseries/timeSeriesMatrix.h"

using namespace rtctools::timeseries;
using namespace rtctools::schematization;

namespace rtctools
{

class rtcToolsSimulator
{
public:
	rtcToolsSimulator() {};
	rtcToolsSimulator(int iEnsemble, timeSeriesMatrixInterface* tsMatrix, schematisation *schema, 
		double p, rtcRuntimeConfigSettings* runtimeSettings);
	~rtcToolsSimulator(void) {};

    double simulate(int iStart, int iEnd, double** JInc2DArray = (double**)0);
	void simulate(int iStep);
    void evaluateGradient(int iStart, int iEnd); 

private:
	int iEnsemble;
	double p;
	timeSeriesMatrixInterface* tsMatrix;
	schematisation* schema;
	rtcRuntimeConfigSettings* runtimeSettings;
};

}
#endif //RTCTOOLS_SIMULATOR_H
