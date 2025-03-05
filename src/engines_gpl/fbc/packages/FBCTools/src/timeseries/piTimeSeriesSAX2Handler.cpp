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

#include "timeseries/piTimeSeriesSAX2Handler.h"
#include <timeseries/piTimeSeries.h>
#include "utilities/utils.h"
#include "piDiagInterface.h"
#include "timeseries/timeSeriesTensor.h"
#include "timeseries/timeSeriesSparseTensor.h"
#include <iomanip>

#include <sstream>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/sax/SAXException.hpp>
#include <exception>
#ifdef _WIN32
#include <codecvt>
#define WSTRING(x) L ## x
#else
#define WSTRING(x) x
#endif

using namespace std;
using namespace timeseries;
using namespace utilities;

enum modeType{RESET, LOCATIONID, PARAMETERID, QUALIFIERID, ENSEMBLEMEMBERINDEX, MISSVALUE, UNITS};
modeType mode;

string locationId;
string parameterId;
string qualifierId;
multiset<string> qualIdSet;
string eIndexString;
string missValString;

long long t1F, t1M;
long long t2F, t2M;
long long dt;
double missVal;
string units;
timeSeries tempValueArray(0,1,1);

ifstream binFile;

// piTimeSeriesSAX2Handler
// schemas has to be a string of pattern {namespace url }*
piTimeSeriesSAX2Handler::piTimeSeriesSAX2Handler(
	boost::filesystem::path schemaLocation,
	timeSeriesInterface *tsInterface,
	vector<piTimeSeries> input,
	string outputFile, bool outputBin, vector<piTimeSeries> output,
	int nEnsemble)
	: schemaLocation(schemaLocation), tsInterface(tsInterface),
	inputFile(""), inputBin(false), input(input),
	outputFile(outputFile), outputBin(outputBin), output(output), nEnsemble(nEnsemble)
{
	// generate hash map for inputs
	for (int i=0; i<(int)input.size(); i++) {
		string lpqID = input[i].getLocationID()+input[i].getParameterID()+input[i].getQualifierID();
		inputMap.insert(pair<string,piTimeSeries>(lpqID, input[i]));
	}

	// check the type of (sparse)TimeSeriesTensor 
	if (dynamic_cast<timeSeriesSparseTensor*>(tsInterface)) {
		this->limitedMemory = true;
	} else {
		this->limitedMemory= false;
	}

	init();

	// diagnostics
	char buffer[500];
	snprintf(buffer, sizeof(buffer), "%d", input.size());
	piDiagInterface::addLine(4, "time series model: number of import PI time series = " + string(buffer));
	for (int i=0; i<(int)input.size(); i++) {
		piDiagInterface::addLine(4, "time series model: " + input[i].toString());
	}
	snprintf(buffer, sizeof(buffer), "%d", output.size());
	piDiagInterface::addLine(4, "time series model: number of export PI time series = " + string(buffer));
	for (int i=0; i<(int)output.size(); i++) {
		piDiagInterface::addLine(4, "time series model: " + output[i].toString());
	}
}

void piTimeSeriesSAX2Handler::init()
{
	locationId = "";
	parameterId = "";
	qualifierId = "";
	qualIdSet = multiset<string>();
	eIndexString = "";
	t1F = -1;
	t1M = -1;
	t2F = -1;
	t2M = -1;
	dt = -1;
	missValString = "";
	missVal = -1;
	units = "";
}

// ---------------------------------------------------------------------------
//  piTimeSeriesSAX2Handler: Implementation of the SAX DocumentHandler interface
// ---------------------------------------------------------------------------
void piTimeSeriesSAX2Handler::startDocument()
{
	// optional bin file
	if (inputBin) {
		binFile.open((inputFile.substr(0, outputFile.size()-4).append(".bin")).c_str(), ios::binary);
	}
}

void piTimeSeriesSAX2Handler::startElement(const XMLCh* const uri,
                            			   const XMLCh* const localname,
                            			   const XMLCh* const qname,
                            			   const Attributes& attrs)
{
    char *message = XMLString::transcode(localname);

	if (XMLString::compareIString(message, "header") == 0) {

		// new header, initialize time series settings
		mode = RESET;
		init();

	} else if (XMLString::compareIString(message, "locationId") == 0) {

		mode = LOCATIONID;

	} else if (XMLString::compareIString(message, "parameterId") == 0) {

		mode = PARAMETERID;

	} else if (XMLString::compareIString(message, "qualifierId") == 0) {

		mode = QUALIFIERID;

	} else if (XMLString::compareIString(message, "ensembleMemberIndex") == 0) {

		mode = ENSEMBLEMEMBERINDEX;

	} else if (XMLString::compareIString(message, "timeStep") == 0) {

		mode = RESET;

		long long u = 1;
		long long m = 1;
		long long d = 1;
		
		for (XMLSize_t i=0; i<attrs.getLength(); i++) {

			char *nameCArray = XMLString::transcode(attrs.getLocalName(i));
			char *dataCArray = XMLString::transcode(attrs.getValue(i));
			if (XMLString::compareIString(nameCArray, "unit")==0)
				u = getUnit(string(dataCArray));
			if (XMLString::compareIString(nameCArray, "multiplier")==0)
				m = atoi(dataCArray);
			if (XMLString::compareIString(nameCArray, "divider")==0)
				d = atoi(dataCArray);
			XMLString::release(&nameCArray);
			XMLString::release(&dataCArray);
		}

		dt = (u*m)/d;

	} else if (XMLString::compareIString(message, "startDate") == 0) {

		mode = RESET;

		char *dateCArray;
		char *timeCArray;

		for (XMLSize_t i=0; i<attrs.getLength(); i++) {

			char *nameCArray = XMLString::transcode(attrs.getLocalName(i));
			if (XMLString::compareIString(nameCArray, "date")==0)
				dateCArray = XMLString::transcode(attrs.getValue(i));
			if (XMLString::compareIString(nameCArray, "time")==0)
				timeCArray = XMLString::transcode(attrs.getValue(i));
			XMLString::release(&nameCArray);
		}

		t1F = utils::date2time(dateCArray, timeCArray);
		t1M = min(t1F, tsInterface->getStartTime());

		XMLString::release(&dateCArray);
		XMLString::release(&timeCArray);

	} else if (XMLString::compareIString(message, "endDate") == 0) {

		mode = RESET;

		char *dateCArray;
		char *timeCArray;

		for (XMLSize_t i=0; i<attrs.getLength(); i++)
		{
			char *nameCArray = XMLString::transcode(attrs.getLocalName(i));
			if (XMLString::compareIString(nameCArray, "date")==0)
				dateCArray = XMLString::transcode(attrs.getValue(i));
			if (XMLString::compareIString(nameCArray, "time")==0)
				timeCArray = XMLString::transcode(attrs.getValue(i));
			XMLString::release(&nameCArray);
		}
		
		t2F = utils::date2time(dateCArray, timeCArray);
		t2M = max(t2F, tsInterface->getEndTime());

		XMLString::release(&dateCArray);
		XMLString::release(&timeCArray);

	} else if (XMLString::compareIString(message, "missVal") == 0) {

		mode = MISSVALUE;

	} else if (XMLString::compareIString(message, "units") == 0) {

		mode = UNITS;

	} else if (XMLString::compareIString(message, "event") == 0) {

		mode = RESET;

		char *dateCArray;
		char *timeCArray;
		char *valueCArray;

		for (XMLSize_t i=0; i<attrs.getLength(); i++)
		{
			char *nameCArray = XMLString::transcode(attrs.getLocalName(i));
			if (XMLString::compareIString(nameCArray, "date")==0)
				dateCArray = XMLString::transcode(attrs.getValue(i));
			if (XMLString::compareIString(nameCArray, "time")==0)
				timeCArray = XMLString::transcode(attrs.getValue(i));
			if (XMLString::compareIString(nameCArray, "value")==0)
				valueCArray = XMLString::transcode(attrs.getValue(i));
			XMLString::release(&nameCArray);
		}

		long long t = utils::date2time(dateCArray, timeCArray);
		string valueString = string(valueCArray);
		if (!limitedMemory) {
			// store timeseries in temporary full vectors
			if ((valueString.compare(missValString)==0) || (valueString.compare("NaN")==0)) {
				tempValueArray.setValue(t,numeric_limits<float>::quiet_NaN());
			} else {
				tempValueArray.setValue(t,(float)atof(valueCArray));
			}
		} else {
			// store timeseries in temporary sparse vectors
			temptime.push_back(t);
			if ((valueString.compare(missValString)==0) || (valueString.compare("NaN")==0)) {
	    		tempvals.push_back(numeric_limits<double>::quiet_NaN());
			} else {
				tempvals.push_back((double)atof(valueCArray));
			}
		}

		XMLString::release(&dateCArray);
		XMLString::release(&timeCArray);
		XMLString::release(&valueCArray);

	} else {
		mode = RESET;
	}

    XMLString::release(&message);
}

void piTimeSeriesSAX2Handler::characters(const XMLCh* const chars,
                                         const XMLSize_t length)
{
	char *c = XMLString::transcode(chars);

	if (mode==LOCATIONID) {
		locationId += string(c);
	} else if (mode==PARAMETERID) {
		parameterId += string(c);
	} else if (mode==QUALIFIERID) {
		qualifierId += string(c);
	} else if (mode==ENSEMBLEMEMBERINDEX) {
		eIndexString += string(c);
	} else if (mode==MISSVALUE) {
		missValString += string(c);
	} else if (mode==UNITS) {
		units += string(c);
	}

	XMLString::release(&c);
}

void piTimeSeriesSAX2Handler::endElement(const XMLCh* const uri,
										 const XMLCh* const localname,
										 const XMLCh* const qname)
{
	char* message = XMLString::transcode(localname);

	if (XMLString::compareIString(message, "qualifierId") == 0) {
		qualIdSet.insert(qualifierId);
		qualifierId = "";
	}

	if (XMLString::compareIString(message, "header") == 0) {

		// missing value (required when reading events)
		missVal = numeric_limits<float>::quiet_NaN();
		if (missValString.compare("NaN")!=0) {
			missVal = atof(missValString.c_str());
		}

		if (!limitedMemory) {
			// allocate memory for temporary value array, full data structures
			tempValueArray = timeSeries(t1F,t2F,dt); 
		}
	}

	if (XMLString::compareIString(message, "series") == 0) {

		// read data from bin file if available
		if (inputBin) {
			float v;
			for (long long t=t1F; t<=t2F; t+=dt) {
				binFile.read((char*)&v, sizeof(float));
				tempValueArray.setValue(t, v);
			}
		}

		// ensemble index, -1 if undefined
		int eIndex = -1;
		if (eIndexString.size()>0) {
			eIndex = atoi(eIndexString.c_str());
		}

		// append all sorted qualifier IDs to location and parameter ID for identification
		string lpqID = locationId + parameterId;
		for (multiset<string>::const_iterator i(qualIdSet.begin()), end(qualIdSet.end()); i!=qualIdSet.end(); ++i) {
			lpqID.append(*i);
		};

		// get the internal time series Ids based on the combination of locationId,
		// parameterId and the qualifierIds
		pair<multimap<string,piTimeSeries>::iterator, multimap<string,piTimeSeries>::iterator> range 
			= inputMap.equal_range(lpqID);
		for (multimap<string,piTimeSeries>::iterator iter = range.first; iter != range.second; ++iter)
		{
			int sIndex = iter->second.getIndex();
			
			// optional validation for units
			if (units.size()>0 && iter->second.getUnit().size()>0 && units.compare(iter->second.getUnit())!=0) {
				stringstream ss;
				ss << "time series (locationId = " << locationId << ", parameterId = " << parameterId;
				ss << ") has the wrong unit = " << units << ", unit = " << iter->second.getUnit() << " expected";
				throw runtime_error(ss.str().c_str());
			}
		
            if (!this->limitedMemory) {
                // create local copy for the following interpolation,
				// the same PI-XML series may used for several internal series with
				// different interpolation options each
				timeSeries intValueArray = timeSeries(t1F,t2F,dt);
				for (long long t=t1F; t<=t2F; t+=dt) {
					intValueArray.setValue(t, tempValueArray.getValue(t));
				}

				// apply interpolation routines to local series
				intValueArray.setInterpolation(iter->second.getInterpolationOption()); 
				intValueArray.setExtrapolation(iter->second.getExtrapolationOption());
				intValueArray.finalize();

				// full data structures
				for (int tIndex=0; tIndex<tsInterface->getNTimeStep(); tIndex++) {
					float v = intValueArray.getValue(tsInterface->getTime(tIndex));
					if (v!=missVal) {
						tsInterface->setValue(eIndex, tIndex, sIndex, v);
					}
				}
			} else {
				// interpolation options 
				interpolationOption interpol = iter->second.getInterpolationOption();
				interpolationOption extrapol = iter->second.getExtrapolationOption();

				dynamic_cast<timeSeriesSparseTensor*>(tsInterface)->setSeries(eIndex, sIndex, &temptime, &tempvals, t1F, t2F, dt, interpol, extrapol);
				// clear workarrays after writing a serie
				temptime.clear();
				tempvals.clear();
			}
		}
	}

    XMLString::release(&message);
}

void piTimeSeriesSAX2Handler::endDocument()
{
	if (inputBin) {
		binFile.close();
	}
}

// ---------------------------------------------------------------------------
//  piTimeSeriesSAX2Handler: Overrides of the SAX ErrorHandler interface
// ---------------------------------------------------------------------------
void piTimeSeriesSAX2Handler::error(const SAXParseException& e)
{
	char *messCArray = XMLString::transcode(e.getMessage());
    XERCES_STD_QUALIFIER cerr << "\nError at file " << inputFile
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "\n  Message: " << messCArray << XERCES_STD_QUALIFIER endl;
	XMLString::release(&messCArray);
}

void piTimeSeriesSAX2Handler::fatalError(const SAXParseException& e)
{
	char *messCArray = XMLString::transcode(e.getMessage());
    XERCES_STD_QUALIFIER cerr << "\nFatal Error at file " << inputFile
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "\n  Message: " << messCArray << XERCES_STD_QUALIFIER endl;
	XMLString::release(&messCArray);
}

void piTimeSeriesSAX2Handler::warning(const SAXParseException& e)
{
	char *messCArray = XMLString::transcode(e.getMessage());
    XERCES_STD_QUALIFIER cerr << "\nWarning at file " << inputFile
		 << ", line " << e.getLineNumber()
		 << ", char " << e.getColumnNumber()
         << "\n  Message: " << messCArray << XERCES_STD_QUALIFIER endl;
	XMLString::release(&messCArray);
}

void piTimeSeriesSAX2Handler::read(string filename, bool hasBin)
{
	this->inputFile = filename;
	this->inputBin = hasBin;

	try {
		XMLPlatformUtils::Initialize();
    }
    catch (const XMLException& e) {
		char* message = XMLString::transcode(e.getMessage());
		piDiagInterface::addLine(1, "void piTimeSeriesInterface::readSAX() - " + string(message));
        XMLString::release(&message);
        throw;
    }

    SAX2XMLReader* parser = XMLReaderFactory::createXMLReader();
	parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);
    parser->setFeature(XMLUni::fgXercesSchema, true);
	parser->setFeature(XMLUni::fgXercesSchemaFullChecking, false);
    parser->setFeature(XMLUni::fgXercesIdentityConstraintChecking, false);
    parser->setFeature(XMLUni::fgSAX2CoreNameSpacePrefixes, false);
	
	string schemaLoc("http://www.wldelft.nl/fews/PI " + utils::xsd_filename(utils::getAbsoluteFilename(schemaLocation, "pi_timeseries.xsd")));
	parser->setProperty(XMLString::transcode("http://apache.org/xml/properties/schema/external-schemaLocation"), 
		(void*) XMLString::transcode(schemaLoc.c_str()));

	// automatic validation
	parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
    parser->setFeature(XMLUni::fgXercesDynamic, true);

	// SAX2 import handler
    parser->setContentHandler(this);
    parser->setErrorHandler(this);

	try {
		parser->parse(inputFile.c_str());
    }
    catch (const XMLException& e) {
		char* message = XMLString::transcode(e.getMessage());
		cout << message << endl;
		piDiagInterface::addLine(1, "void piTimeSeriesInterface::readSAX() - " + string(message));
        XMLString::release(&message);
        throw;
    }
    catch (const SAXParseException& e) {
		char* message = XMLString::transcode(e.getMessage());
		cout << message << endl;
		piDiagInterface::addLine(1, "void piTimeSeriesInterface::readSAX() - " + string(message));
        XMLString::release(&message);
        throw;
    }
    catch (const exception& e) {
        const char* message = e.what();
		cout << message << endl;
		piDiagInterface::addLine(1, "void piTimeSeriesInterface::readSAX() - " + string(message));
        throw;
    }

	delete parser;
}

void piTimeSeriesSAX2Handler::write(bool adjointOutput)
{
	if (!this->limitedMemory) {
		this->write(outputFile, output, this->nEnsemble,
			dynamic_cast<timeSeriesTensor*>(tsInterface)->getValueTensor(), "");
	} else {
		// removed writing to diagnostic file. timeseries_export.xml written only when limited memory == false
		//piDiagInterface::addLine(1, "piTimeSeriesSAX2Handler::write not possible (yet) if limitedMemory is true.");
	}
}

piTimeSeries* piTimeSeriesSAX2Handler::getPiTimeSeries(string id)
{
	for (int i=0; i<(int)input.size(); i++) {
		if (input[i].getID().compare(id)==0) return &input[i];
	}
	for (int i=0; i<(int)output.size(); i++) {
		if (output[i].getID().compare(id)==0) return &output[i];
	}

	return (piTimeSeries*)0;
}

void piTimeSeriesSAX2Handler::write(string filename, vector<string> id, 
	int nEnsemble, double ***tensor, string par_prefix)
{
	int n = (int)id.size();

	vector<piTimeSeries> series = vector<piTimeSeries>(n);
	for (int i=0; i<n; i++) {
		series[i] = piTimeSeries(id[i], i, id[i], id[i], multiset<string>(), "", NONE, NONE);
	}

	write(filename, series, nEnsemble, tensor, par_prefix);
}

void piTimeSeriesSAX2Handler::write(string filename, vector<piTimeSeries> series, 
	int nEnsemble, double ***tensor, string par_prefix, const stringContainer& sc)
{
	char datestring[30];
	char timestring[30];
	float missVal = -9999.0f;

	// xml output
#ifdef _WIN32
	wofstream xmlFile;
	xmlFile.imbue(std::locale(std::locale::empty(), new std::codecvt_utf8<wchar_t, 0x10ffff, std::generate_header>));
#else
	ofstream xmlFile;
#endif
	xmlFile.open(filename.c_str(), ios::out | ios::trunc);

	// optional bin output
	ofstream binFile;
	if (outputBin) {
		binFile.open((filename.substr(0, filename.size()-4).append(".bin")).c_str(), ios::out | ios::trunc | ios::binary);
	}

	xmlFile << WSTRING("<?xml version=\"1.0\" encoding=\"UTF-8\"?>") << endl;
	xmlFile << WSTRING("<TimeSeries xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns=\"http://www.wldelft.nl/fews/PI\">") << endl;
    xmlFile << WSTRING("  <timeZone>0.0</timeZone>") << endl;

	for (int i=0; i<nEnsemble; i++) {
		for (int j=0; j<(int)series.size(); j++) {

			// header
			xmlFile << WSTRING("  <series>") << endl;
			xmlFile << WSTRING("    <header>") << endl;
			xmlFile << WSTRING("      <type>instantaneous</type>") << endl;
			xmlFile << WSTRING("      <locationId>" << series[j].getLocationID().c_str() << "</locationId>") << endl;
			xmlFile << WSTRING("      <parameterId>" << par_prefix.c_str() << series[j].getParameterID().c_str() << "</parameterId>") << endl;

			multiset<string> qSet = series[j].getQualifierIDs();
			for (multiset<string>::const_iterator k(qSet.begin()), end(qSet.end()); k!=qSet.end(); ++k) {
				xmlFile << WSTRING("      <qualifierId>" << (*k).c_str() << "</qualifierId>") << endl;
			};

			if (tsInterface->getNEnsemble()>1) {
				xmlFile << WSTRING("      <ensembleMemberIndex>" << i << "</ensembleMemberIndex>") << endl;
			}

			xmlFile << "      <timeStep unit=\"second\" multiplier=\"" << tsInterface->getDT()/1000 << "\" />" << endl;
			xmlFile << "      <startDate date=\"" << utils::time2datestring(tsInterface->getStartTime(), datestring) << "\" time=\"" << utils::time2timestring(tsInterface->getStartTime(), timestring) << "\" />"  << endl;
			xmlFile << "      <endDate date=\"" << utils::time2datestring(tsInterface->getEndTime(), datestring) << "\" time=\"" << utils::time2timestring(tsInterface->getEndTime(), timestring) << "\" />"  << endl;
			xmlFile << "      <missVal>" << missVal << "</missVal>" << endl;

			if (series[j].getUnit().size()>0) {
				xmlFile << "      <units>" << series[j].getUnit().c_str() << "</units>" << endl;
			}

			xmlFile << "    </header>" << endl;

			// data
			int index = series[j].getIndex();
			vector<long long> time = tsInterface->getTimes();
			for (int k=0; k<tsInterface->getNTimeStep(); k++) {
				double value = tensor[i][k][index];
				if (value != value) {
					// value is NaN
					if (outputBin) {
						binFile.write((char*)&missVal, sizeof(float));
					} else {
						xmlFile << "    <event date=\"" 
							    << utils::time2datestring(time[k], datestring) 
								<< "\" time=\"" << utils::time2timestring(time[k], timestring) << "\" ";
						string comment = sc.getString(i,k,index);
						if (comment.size()>0) {
							xmlFile << "comment=\"" << comment.c_str() << "\" ";
						}
						xmlFile << "value=\"" << missVal << "\" />" << endl;
					}
				} else {
					if (outputBin) {
						float fvalue = (float)value;
						binFile.write((char*)&fvalue, sizeof(float));
					} else {
						xmlFile << "    <event date=\"" 
							    << utils::time2datestring(time[k], datestring) 
								<< "\" time=\"" << utils::time2timestring(time[k], timestring) << "\" ";
						string comment = sc.getString(i,k,index);
						if (comment.size()>0) {
							xmlFile << "comment=\"" << comment.c_str() << "\" ";
						}
						xmlFile << "value=\"" << setprecision(8) << value << "\" />" << endl;
					}
				}
			}

			// optional comment
			if (series[j].getComment().size()>0) {
				xmlFile << "    <comment>" << series[j].getComment().c_str() << "</comment>" << endl;
			}

			xmlFile << "  </series>" << endl;
		}
	}

	// close files
	xmlFile << "</TimeSeries>" << endl;
	xmlFile.close();
	if (outputBin) {
		binFile.close();
	}
}

long long piTimeSeriesSAX2Handler::getUnit(string unit)
{
	if (unit.compare("week")==0) {
		return 7*24*3600*1000;
	} else if (unit.compare("day")==0) {
		return 24*3600*1000;
	} else if (unit.compare("hour")==0) {
		return 3600*1000;
	} else if (unit.compare("minute")==0) {
		return 60*1000;
	} else if (unit.compare("second")==0) {
		return 1000;
	} else {
		string s = "time step " + unit + " is not supported";
		piDiagInterface::addLine(1, "long long piTimeSeriesSAX2Handler::getUnit(string unit) - " + s);
		throw runtime_error(s.c_str());
	}
}
