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
 * @brief Data structure to store input timeseries. Interpolation is performed 
 * when reading the data, interpolated values are stored for (ensembleId,tIndex,sIndex).
 */

#include "timeseries/timeSeriesTensor.h"
#include "piDiagInterface.h"
#include <sstream>
#include <stdexcept>

using namespace timeseries;
using namespace utilities;

timeSeriesTensor::timeSeriesTensor(int nEnsemble,
								   int nTimeStep, vector<long long> time,
								   int nSeries, vector<string> seriesID, vector<validationEnum> seriesValidation,
								   map<string,int> scalarIDMap, map<string,pair<int,int> > vectorIDMap)
  	: timeSeriesInterface(nTimeStep, time, nSeries, seriesID, seriesValidation, scalarIDMap, vectorIDMap)
{
	this->nEnsemble = nEnsemble;

	// allocate
	try {
		piDiagInterface::addLine(4, "timeSeriesTensor::timeSeriesTensor(...) - allocating time series tensors");
		this->valueTensor = utils::dten(nEnsemble, nTimeStep, getNSeriesMatrix());
		this->objTensor = utils::dten(nEnsemble, nTimeStep, getNSeriesMatrix());
		piDiagInterface::addLine(4, "timeSeriesTensor::timeSeriesTensor(...) - time series tensors allocated");
	} catch (exception &e) {
		piDiagInterface::addLine(1, "timeSeriesTensor::timeSeriesTensor(...) - error - " + string(e.what()));
		throw;
	}

	// initialize
	for (int i=0; i<nEnsemble; i++) {
		for (int j = 0; j < nTimeStep; j++) {
			for (int k=0; k<getNSeriesMatrix(); k++) {
				this->valueTensor[i][j][k] = numeric_limits<double>::quiet_NaN();
				this->objTensor[i][j][k] = 0.0;
			}
		}
	}

	// distribute to matrix
	valueMatrix = new timeSeriesMatrix*[nEnsemble];
	for (int i=0; i<nEnsemble; i++) {
		valueMatrix[i] = new timeSeriesMatrix(nTimeStep, time,
			nSeries, seriesID, seriesValidation, scalarIDMap, 
			vectorIDMap, valueTensor[i], objTensor[i]);
	}
}

timeSeriesTensor::~timeSeriesTensor(void)
{
	for (int i=0; i<nEnsemble; i++) {
		delete valueMatrix[i];
	}
	delete valueMatrix;

	utils::free_dten(this->valueTensor);
	utils::free_dten(this->objTensor);
}

int timeSeriesTensor::getNEnsemble() { 
	return nEnsemble; 
}

int timeSeriesTensor::getNSeriesMatrix() { 	
	return nSeries;
}

timeSeriesMatrix* timeSeriesTensor::getTimeSeriesMatrix(int ensembleId) { 
	return valueMatrix[ensembleId]; 
}

double*** timeSeriesTensor::getValueTensor() { 
	return valueTensor; 
}

double*** timeSeriesTensor::getObjTensor() { 
	return objTensor; 
}

double timeSeriesTensor::getValue(int ensembleId, int tIndex, int sIndex) { 
	return valueMatrix[ensembleId]->getValue(tIndex, sIndex); 
}

void timeSeriesTensor::setValue(int ensembleId, int tIndex, int sIndex, double value) { 
	if (ensembleId<0) {
		for (int i=0; i<nEnsemble; i++) {
			valueMatrix[i]->setValue(tIndex, sIndex, value); 
		} 
	} else {
		valueMatrix[ensembleId]->setValue(tIndex, sIndex, value);
	}
}

void timeSeriesTensor::validate(int nSeriesEnd, int nTimeStepT0) {
	for (int i=0; i<nEnsemble; i++) {
		try {
			valueMatrix[i]->validate(nSeriesEnd, nTimeStepT0);
		} catch (exception &e) {
			stringstream ss;
			ss << "void timeSeriesTensor::validate() - validation error in time series matrix " 
			   << i << ": " << e.what(); 
			throw runtime_error(ss.str().c_str());
		}
	}
}

double* timeSeriesTensor::getState(int ensembleId, int tIndex) { 
	return this->getTimeSeriesMatrix(ensembleId)->getState(tIndex);
}

void timeSeriesTensor::initState() {
	throw runtime_error("Method not available timeSeriesSparseTensor::getValueTensor.");
}

void timeSeriesTensor::incrementTimeStep(int ensembleId) {
	return this->getTimeSeriesMatrix(ensembleId)->incrementTimeStep();
}

vector<int> timeSeriesTensor::getEnsembleMap() {
	return this->ensembleMap;
} 

void timeSeriesTensor::setEnsembleMap(vector<int> ensembleMap) {
	this->ensembleMap = ensembleMap;
}
