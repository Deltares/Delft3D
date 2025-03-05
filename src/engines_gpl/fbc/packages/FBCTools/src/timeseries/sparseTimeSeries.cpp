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

#include "timeseries/sparseTimeSeries.h"
#include <assert.h>

using namespace rtctools::timeseries;

sparseTimeSeries::sparseTimeSeries(int sIndex,vector<long long>* times, vector<double>* vals, 
	long long t1, long long t2, long long dt, interpolationOption interpol, interpolationOption extrapol){
	    this->seriesIndex = sIndex;
		this->t1 = t1;
		this->t2 = t2;
		this->dt = dt;
		this->intpol = interpol;
		this->extpol = extrapol;
		this->times = *times;
		this->vals = *vals;
	};

sparseTimeSeries::~sparseTimeSeries(void) {
}

int sparseTimeSeries::getSeriesIndex() {
	return this->seriesIndex;
}

interpolationOption sparseTimeSeries:: getInterpolationOption(){
	return this->intpol;
}

interpolationOption sparseTimeSeries::getExtrapolationOption(){
	return this->extpol;
}

long long sparseTimeSeries::getStarttime(){
	return this->t1;
}

long long sparseTimeSeries::getEndtime(){
	return this->t2;
}

long long sparseTimeSeries::getdt(){
	return this->dt;
}

vector <long long> sparseTimeSeries::gettimes(){
	return this->times ;
}

vector <double> sparseTimeSeries::getvals(){
	return this->vals ;
}

double sparseTimeSeries::getvalue(int index){
	return this->vals[index] ;
}

void sparseTimeSeries::setvalue(int index, double newval){
	assert(index >= 0);
	assert(index < (int)this->vals.size());
	this->vals[index] = newval;
}
