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


#ifndef PITIMESERIESSAX2HANDLER_H
#define PITIMESERIESSAX2HANDLER_H

#include "timeseries/piTimeSeries.h"
#include "timeseries/stringContainer.h"
#include <iostream>
#include <map>
#include <vector>
#include <limits>
#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <stdexcept>
#include "boost/filesystem.hpp"

#include "timeSeriesInterface.h"

using namespace std;
using namespace xercesc;

namespace rtctools
{
namespace timeseries
{

class timeSeries
{
private:
	enum modeType{RESET, LOCATIONID, PARAMETERID, QUALIFIERID, ENSEMBLEMEMBERINDEX, MISSVALUE, UNITS};
	modeType mode;

	//where to look for pi schema (default is http://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_timeseries.xsd)
	string schemaLocation;
	timeSeriesInterface *tsInterface;

	// input file
	string inputFile;
	bool inputBin;
	int nInput;
	vector<piTimeSeries> input;

	// output file
	string outputFile;
	bool outputBin;
	int nOutput;
	piTimeSeries *output;

	vector<int> ensembleMap;

	multimap<string,piTimeSeries> inputMap;

	string locationId;
	string parameterId;
	string qualifierId;
	multiset<string> qualIdSet;
	string eIndexString;
	string missValString;

	double missVal;
	string units;
	vector<long long> temptime;
	vector<double> tempvals;

public:
	timeSeries(long long t1, long long t2, long long dt) {
		this->t1 = t1;
		this->t2 = t2;
		this->dt = dt;
		n = (int)((t2-t1)/dt+1);
		vArray = vector<float>((int)((t2-t1)/dt+1));
		for (int i=0; i<n; i++) vArray[i] = numeric_limits<float>::quiet_NaN();
		intOpt = NONE;
		extOpt = NONE;
		final = false;
	}

	void setInterpolation(interpolationOption intOpt) { this->intOpt = intOpt; }
	void setExtrapolation(interpolationOption extOpt) { this->extOpt = extOpt; }

	void setValue(long long t, float value) {
		if (final) throw runtime_error("no setValue() allowed after finalization of timeSeries");
		// Is time t inside of period [t1, t2]?
		assert(t>=t1);
		assert(t<=t2);
		int index = (int)((t-t1)/dt);
		// Does time t match a time step exactly?
		assert(t1+index*dt==t);
		vArray[index] = value;
	}

	void finalize() {	
		final = true;
		// index array with no zero elements
		vector<int> iArray;
		for (int i=0; i<n; i++) {
			if (vArray[i]==vArray[i]) {
				iArray.push_back(i);
			}
		}
		int ni = (int)iArray.size();
		// interpolation
		for (int i=0; i<ni-1; i++) {
			for (int j=iArray[i]+1; j<iArray[i+1]; j++) {
				if (intOpt==BLOCK) {
					vArray[j] = vArray[iArray[i]];
				} else if (intOpt==LINEAR) {
					vArray[j] = (float)(j-iArray[i])/(float)(iArray[i+1]-iArray[i]) *
						(vArray[iArray[i+1]]-vArray[iArray[i]]) + vArray[iArray[i]];
				}
			}
		}
		// extrapolation
		if (extOpt==BLOCK && iArray.size()>0) {
			for (int i=0; i<iArray[0]; i++) vArray[i] = vArray[iArray[0]];
			for (int i=iArray[ni-1]; i<(int)vArray.size(); i++) vArray[i] = vArray[iArray[ni-1]];
		}
	}

	float getValue(long long t) {
		if (final==false) finalize();
		int index = (int)((t-t1)/dt);
		if (index<0) {
			// extrapolation to negative time axis
			if (extOpt==BLOCK) {
				return vArray[0];
			} else if (extOpt==PERIODIC) {
				return getValue(t + (t2-t1+dt));
			} 
		} else if (index<n-1) {
			double w = (double)(t-(t1+index*dt))/(double)dt;
			if (w==0.0 || intOpt==BLOCK) {
				return vArray[index];
			} else if (intOpt==LINEAR){
				return (float)((1.0-w)*vArray[index] + w*vArray[index+1]);
			} 
		} else if (index==n-1) {
			double w = (double)(t-(t1+index*dt))/(double)dt;
			if (w==0.0 || extOpt==BLOCK || (extOpt==PERIODIC && intOpt==BLOCK)) {
				return vArray[index];
			} else if (extOpt==PERIODIC && intOpt==LINEAR) {
				return (float)((1.0-w)*vArray[index] + w*vArray[0]);
			} 
		} else {
			// extrapolation to positive time axis
			if (extOpt==BLOCK) {
				return vArray[n-1];
			} else if (extOpt==PERIODIC) {
				return getValue(t - (t2-t1+dt));
			} 
		}
		return numeric_limits<float>::quiet_NaN();
	}

private:
	long long t1; 
	long long t2;
	long long dt;
	int n;
	vector<float> vArray;
	interpolationOption intOpt;
	interpolationOption extOpt;
	bool final;
};

class piTimeSeriesSAX2Handler : public DefaultHandler
{
private:
	//where to look for pi schema (default is http://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_timeseries.xsd)
	boost::filesystem::path schemaLocation;
	timeSeriesInterface *tsInterface;

	// input file
	string inputFile;
	bool inputBin;
	vector<piTimeSeries> input;

	// output file
	string outputFile;
	bool outputBin;
	vector<piTimeSeries> output;

	int nEnsemble;

	multimap<string,piTimeSeries> inputMap;
    // flag to switch between full Tensor/Matrix storage and sparse data structures
	bool limitedMemory;
	// temporary arrays to store sparse values
	vector<long long> temptime;
	vector<double> tempvals;
	
public:
	piTimeSeriesSAX2Handler(boost::filesystem::path schemaLocation,
		timeSeriesInterface *valueInterface,
		vector<piTimeSeries> input,
		string outputFile, bool outputBin, vector<piTimeSeries> output,
		int nEnsemble
	);

	void init();

	// handler
	void startDocument();
    void startElement(
        const XMLCh* const uri,
        const XMLCh* const localname,
        const XMLCh* const qname,
        const Attributes& attrs
    );
	void characters(
		const XMLCh* const chars,
        const XMLSize_t length
	);
	void endElement(
		const XMLCh* const uri,
		const XMLCh* const localname,
		const XMLCh* const qname
	);
	void endDocument();

	//
	void warning(const SAXParseException& exc);
    void error(const SAXParseException& exc);
    void fatalError(const SAXParseException& exc);

	// others
	void read(string filename, bool hasBin);
	piTimeSeries* getPiTimeSeries(string id);
	void write(bool adjointOutput);
	void write(string filename, vector<string> series, 
		int nEnsemble, double ***tensor, string par_prefix = "");
	void write(string filename, vector<piTimeSeries> series, 
		int nEnsemble, double ***tensor, string par_prefix = "", const stringContainer& sc = stringContainer());
	long long getUnit(string unit);
	void transformation(vector<float> &valueArray,
		interpolationOption intOpt,
		interpolationOption extOpt);
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* PITIMESERIESSAX2HANDLER_H */
