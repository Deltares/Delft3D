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
 * @brief Data structure to store input timeseries in a sparse way. Interpolation 
 *	is performed only after the getValue request for specific (ensembleId,tIndex,sIndex).
 */

#include "timeseries/timeSeriesSparseTensor.h"
#include "piDiagInterface.h" 
#include <stdexcept>

using namespace timeseries;

timeSeriesSparseTensor::timeSeriesSparseTensor(int nEnsemble,
								   int nTimeStep, vector<long long> time,
								   int nSeries, vector<string> seriesID,
								   vector<validationEnum> seriesValidation,
								   map<string,int> scalarIDMap,
								   map<string,pair<int,int> > vectorIDMap,
								   int nimport)
   	: timeSeriesInterface(nTimeStep, time, nSeries, seriesID, seriesValidation, scalarIDMap, vectorIDMap)
{
	this->nEnsemble = nEnsemble;

	// create nEnsemble pointers to objects timeSeriesSparseMatrix
	for (int i=0; i<nEnsemble ; i++) {
		timeSeriesSparseMatrix *sm = new timeSeriesSparseMatrix(nTimeStep, time,
			nSeries, seriesID, seriesValidation, scalarIDMap, vectorIDMap,nimport);
		smVector.push_back(sm);
	}
	piDiagInterface::addLine(4, "timeSeriesSparseTensor::timeSeriesSparseTensor(...) - time series tensor initialized");
}

timeSeriesSparseTensor::~timeSeriesSparseTensor(void){
}

int timeSeriesSparseTensor::getNEnsemble() { 
	return this->nEnsemble; 
}

double timeSeriesSparseTensor::getValue(int ensembleId, int tIndex, int sIndex)
{
	return smVector.at(ensembleId).getValue(tIndex, sIndex);
}

void timeSeriesSparseTensor::setValue(int ensembleId, int tIndex, int sIndex, double value) 
{
	bool set;
	if (ensembleId<0) {
		// distribute value to all ensembles
		for (int i=0; i<nEnsemble; i++) {
			set = smVector.at(i).setValue(tIndex, sIndex, value);
			if (!set) {throw runtime_error("timeSeriesSparseTensor::setValue: no reset possible for given indices.") ;}
		} 
	} else {
		set = smVector.at(ensembleId).setValue(tIndex, sIndex, value);
		if (!set) {throw runtime_error("timeSeriesSparseTensor::setValue: no reset possible for given indices.") ;}
	}
}

void timeSeriesSparseTensor::setSeries(int ensembleId, 
									   int sIndex, 
									   vector<long long>* times, 
									   vector<double>* vals, 
									   long long t1,
									   long long t2,
									   long long dt, 
									   interpolationOption interpol, 
									   interpolationOption extrapol)
{
	int nEnsemble = timeSeriesSparseTensor::getNEnsemble();
    if (ensembleId<0) {
		//distribute this series to all ensembles
		for (int i=0; i<nEnsemble; i++) {
			smVector.at(i).setSeries(sIndex, times, vals, t1, t2, dt, interpol, extrapol);
		} 
	} else {
		smVector.at(ensembleId).setSeries(sIndex, times, vals, t1, t2, dt, interpol, extrapol);
	}
}

int timeSeriesSparseTensor::getNSeriesMatrix() {
	return smVector.at(0).nSparse;
}

double* timeSeriesSparseTensor::getState(int ensembleId, int tIndex) {
	return smVector.at(ensembleId).getState(tIndex);
}

void timeSeriesSparseTensor::initState() {
	for (int i=0 ; i<this->nEnsemble ; i++) {
		smVector[i].initState();
	}
}

vector<int> timeSeriesSparseTensor::getEnsembleMap() {
	return this->ensembleMap;
} 

void timeSeriesSparseTensor::setEnsembleMap(vector<int> ensembleMap) {
	this->ensembleMap = ensembleMap;
}

void timeSeriesSparseTensor::incrementTimeStep(int ensembleId) {
	smVector.at(ensembleId).incrementTimeStep();
}

void timeSeriesSparseTensor::validate(int nSeriesEnd, int nTimeStepT0) {
}

timeSeriesMatrixInterface* timeSeriesSparseTensor::getTimeSeriesMatrix(int ensembleId) {
	return &(smVector.at(ensembleId));
}

double*** timeSeriesSparseTensor::getValueTensor() {
	throw runtime_error("Method not available timeSeriesSparseTensor::getValueTensor");
}

double*** timeSeriesSparseTensor::getObjTensor() {
	throw runtime_error("Method not available timeSeriesSparseTensor::getObjTensor");
}

