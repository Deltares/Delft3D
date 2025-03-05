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


#if _MSC_VER && _MSC_VER < 1900 
#define snprintf sprintf_s
#endif

#include "openMIInterface.h"
#include "../piDiagInterface.h"
#include "timeSeriesMatrix.h"
#include "timeSeriesSparseMatrix.h"

using namespace std;
using namespace rtctools::timeseries;

openMIInterface::openMIInterface()
{

}

openMIInterface::openMIInterface(timeSeriesMatrixInterface *tsMatrix, vector<openMIExchangeItem> input, vector<openMIExchangeItem> output)
	: input(input)
	, output(output)
	, tsMatrix(tsMatrix)
{
	// diagnostics
	char buffer[500];
	snprintf(buffer, sizeof(buffer), "%d", input.size());
	piDiagInterface::addLine(4, "time series model: number of input exchange items = " + string(buffer));
	for (int i=0; i<(int)input.size(); i++) {
		piDiagInterface::addLine(4, "time series model: " + input[i].toString());
	}
	snprintf(buffer, sizeof(buffer), "%d", output.size());
	piDiagInterface::addLine(4, "time series model: number of output exchange items = " + string(buffer));
	for (int i=0; i<(int)output.size(); i++) {
		piDiagInterface::addLine(4, "time series model: " + output[i].toString());
	}

	// prepare for simulation
	this->prepare();
}

int openMIInterface::getInputExchangeItemCount() { return (int)input.size(); }

openMIExchangeItem* openMIInterface::getInputExchangeItem(int index) { return &input[index]; }

int openMIInterface::getOutputExchangeItemCount() { return (int)output.size(); }

openMIExchangeItem* openMIInterface::getOutputExchangeItem(int index) { return &output[index]; }

void openMIInterface::prepare()
{
	piDiagInterface::addLine(4, "openMIInterface::prepare()");

	iStep = 0;
	dTime = tsMatrix->getStartTime();
}

long long openMIInterface::getCurrentTime()
{
	piDiagInterface::addLine(4, "openMIInterface::getCurrentTime()");

	return dTime;
}

int openMIInterface::getCurrentTimeStep()
{
	return iStep;
}

double openMIInterface::getValue(int sIndex)
{
	//piDiagInterface::addLine(4, "openMIInterface::getValue(int sIndex)");

	return tsMatrix->getValue(iStep, sIndex);
}

void openMIInterface::setValue(int sIndex, double value, int timeStepCount /* =1, see header */)
{
	//piDiagInterface::addLine(4, "openMIInterface::setValue(int sIndex, double value, int timeStepCount=1)");

    if (timeStepCount == 1)
    {
    	tsMatrix->setValue(iStep, sIndex, value);
    	tsMatrix->setValue(iStep+1, sIndex, value);
        // The last value will be overwritten in the next call,
        // but we set it in order to have it written in the timeseries output
    }
    else
    {
	    for (int i=0; i<=timeStepCount; i++) {
		    tsMatrix->setValue(iStep+i, sIndex, value);
	    }
        // The last value will be overwritten in the next call,
        // but we set it in order to have it written in the timeseries output
    }
}

int openMIInterface::performTimeStep()
{
	//piDiagInterface::addLine(4, "openMIInterface::performTimeStep()");

	iStep++;
	dTime += tsMatrix->getDT(iStep);
	return iStep;
}
