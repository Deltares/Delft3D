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

#include "timeSeriesModel.h"
#include "timeSeriesTensor.h"
#include "timeSeriesSparseTensor.h"
#include "piDiagInterface.h"
#include "dataBinding/rtcDataConfig.hxx"
#include "dataBinding/rtcRuntimeConfig.hxx"
#include "dataBinding/pi_timeseries.hxx"
#include "xsd/cxx/tree/elements.hxx"
#include <utilities/utils.h>
#include <stdexcept>

using fews::PI::TimeSeriesCollectionComplexType;
using fews::PI::TimeSeriesComplexType;
using fews::PI::parseTimeSeries;
using namespace timeseries;
using namespace fews;

const string TIMESERIESMODEL_CODE = "RTCTools.timeseries.timeSeriesModel";

long long getTime(DateTimeComplexType::DateType date, DateTimeComplexType::TimeType time);
long long getTimeStep(TimeStepComplexType::UnitType unit);

void insert_id(vector<string> &id, map<string,int> &idMap, string s)
{
	id.push_back(s);
	idMap[s] = (int)id.size()-1;
}

validationEnum getValidation(RTCTimeSeriesComplexType::ValidationType type)
{
	validationEnum v;

	if (type==RTCTimeSeriesComplexType::ValidationType::STATE) {
		v = VALIDATION_STATE;
	} else if (type==RTCTimeSeriesComplexType::ValidationType::UPDATE) {
		v = VALIDATION_UPDATE;
	} else if (type==RTCTimeSeriesComplexType::ValidationType::UPDATE_EXCEPT_STATE) {
		v = VALIDATION_UPDATE_EXCEPT_STATE;
	} else if (type==RTCTimeSeriesComplexType::ValidationType::FORECAST) {
		v = VALIDATION_FORECAST;
	} else if (type==RTCTimeSeriesComplexType::ValidationType::FORECAST_EXCEPT_T0) {
		v = VALIDATION_FORECAST_EXCEPT_T0;
	} else if (type==RTCTimeSeriesComplexType::ValidationType::ALL) {
		v = VALIDATION_ALL;
	} else if (type==RTCTimeSeriesComplexType::ValidationType::ALL_EXCEPT_STATE) {
		v = VALIDATION_ALL_EXCEPT_STATE;
	} else {
		v = VALIDATION_NO;
	}

	return v;
}

/* constructor */
timeSeriesModel::timeSeriesModel(boost::filesystem::path schemaDir, boost::filesystem::path workDir, string filename, long long t1, long long t2, long long dt, 
								 int nEnsemble, vector<int>& ensembleMap, bool adjointOutput, bool limitedMemory)
{
	try {
		// read data
		readTimeSeriesTensor(schemaDir, workDir, filename, t1, t2, dt, nEnsemble, ensembleMap, adjointOutput, limitedMemory);
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "timeSeriesModel::timeSeriesModel(...) - error - " + string(e.what()), TIMESERIESMODEL_CODE);
		throw;
	}
}

/* pre processing */
void timeSeriesModel::read(void)
{
	try {
		// state import
		stateInt->read();
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "timeSeriesModel::read(), error - " + string(e.what()), TIMESERIESMODEL_CODE);
		throw;
	}
}

void timeSeriesModel::openFiles(bool adjointOutput)
{
	try {
		// csv export
        if (csvInt != nullptr)
        {
            csvInt->openFiles();
        }
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "timeSeriesModel::openFiles(), error - " + string(e.what()), TIMESERIESMODEL_CODE);
		throw;
	}
}

void timeSeriesModel::closeFiles(bool adjointOutput)
{
	try {
		// csv export
		if (csvInt!= nullptr)
		{
            csvInt->closeFiles();
		}
            
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "timeSeriesModel::closeFiles(), error - " + string(e.what()), TIMESERIESMODEL_CODE);
		throw;
	}
}

void timeSeriesModel::write(bool adjointOutput)
{
	try {
	    // state export
	    stateInt->write();

	    // csv and PI export
	    if (csvInt!= nullptr)
        {
			vector<string> series = getTimeSeriesTensor()->getSeriesIDs();
			csvInt->writeFiles("timeseries", series, getTimeSeriesTensor()->getValueTensor());
			csvInt->writeFiles("timeseries_adjoint", series, getTimeSeriesTensor()->getObjTensor());
		}
	    if (piSAX2Int!=0) piSAX2Int->write(adjointOutput);
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "timeSeriesModel::write(), error - " + string(e.what()), TIMESERIESMODEL_CODE);
		throw;
	}
}

/* 
 * Write timeseries info into all available interfaces:
 * per timestep for csvInterface, at last timestep for all interfaces
 */
void timeSeriesModel::write(bool adjointOutput, int timeStep, bool isLastTimeStep, std::vector<string>& additionalTimeseriesNames, std::vector<int>& additionalTimeseries)
{
	try {
        if (isLastTimeStep) {
	        // state export
	        stateInt->write(timeStep);

	        // csv and PI export
	        if (csvInt!= nullptr && timeStep > 0)
	        {
                csvInt->writeFiles(timeStep);
	        }
                
	        if (piSAX2Int!=0 && timeStep > 0)
	        {
                piSAX2Int->write(adjointOutput);
	        }
        }
        else
        {
	        // csv export
	        if (csvInt != nullptr)
	        {
                csvInt->writeFiles(timeStep, additionalTimeseriesNames, additionalTimeseries);
	        }
                
        }
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "timeSeriesModel::write(), error - " + string(e.what()), TIMESERIESMODEL_CODE);
		throw;
	
	}
}

/* destructor */
timeSeriesModel::~timeSeriesModel(void)
{
	delete valueTensor;
	delete stateInt;
	delete csvInt;
	delete piSAX2Int;
	delete openMIInt;
}

timeSeriesTensorInterface* timeSeriesModel::getTimeSeriesTensor() { 
	return valueTensor;
}

openMIInterface* timeSeriesModel::getOpenMIInterface() { return openMIInt; }

void timeSeriesModel::readTimeSeriesTensor(boost::filesystem::path schemaDir, boost::filesystem::path workDir, string filename, 
										   long long t1, long long t2, long long dt, int nEnsemble, vector<int>& ensembleMap,
										   bool adjointOutput, bool limitedMemory)
{
	// number of time steps
	int nTimeStep = (int)((t2-t1)/dt+1);
	vector<long long> time = vector<long long>(nTimeStep);
	for (int i=0; i<nTimeStep; i++) {
		time[i] = t1 + i*dt;
	}
	piDiagInterface::addLine(4, "timeSeriesModel: number of time steps = " + to_string(nTimeStep), TIMESERIESMODEL_CODE);

	// rtc data config
	::xml_schema::Properties rtcDataConfigProperties;
	rtcDataConfigProperties.schema_location("http://www.wldelft.nl/fews", 
		utils::xsd_filename(utils::getAbsoluteFilename(schemaDir, "rtcDataConfig.xsd")));

	// parsing file with objective information
	auto_ptr<RTCDataConfigComplexType> dataConfig;
	try {
		dataConfig = parseRtcDataConfig(filename, 0, rtcDataConfigProperties);
	} catch (const xml_schema::Exception &e) {
		cout << e << endl;
		throw runtime_error("timeSeriesModel: error parsing rtcDataConfig.xml file");
	} 

	RTCSeriesImportComplexType::TimeSeriesSequence rtcTSI = dataConfig->getImportSeries().getTimeSeries();
	RTCSeriesExportComplexType::TimeSeriesSequence rtcTSO = dataConfig->getExportSeries().getTimeSeries();

	// count number of time series / interfaces
	int nImportPITimeSeries = 0;
	int nInputExchangeItem = 0;
	
	for (int i=0; i<(int)rtcTSI.size(); i++) {
		if (rtcTSI[i].getPITimeSeries().present()) {
			if (rtcTSI[i].getVectorLength().present()) {
				nImportPITimeSeries += rtcTSI[i].getVectorLength().get();
			} else {
				nImportPITimeSeries++;
			}
		}
		if (rtcTSI[i].getOpenMIExchangeItem().present()) {
			if (rtcTSI[i].getVectorLength().present()) {
				nInputExchangeItem += rtcTSI[i].getVectorLength().get();
			} else {
				nInputExchangeItem++;
			}
		}
	}

	int nExportPITimeSeries = 0;
	int nOutputExchangeItem = 0;

	for (int i=0; i<(int)rtcTSO.size(); i++) {
		if (rtcTSO[i].getPITimeSeries().present()) {
			if (rtcTSO[i].getVectorLength().present()) {
				nExportPITimeSeries += rtcTSO[i].getVectorLength().get();
			} else {
				nExportPITimeSeries++;
			}
		}
		if (rtcTSO[i].getOpenMIExchangeItem().present()) {
			if (rtcTSO[i].getVectorLength().present()) {
				nOutputExchangeItem += rtcTSO[i].getVectorLength().get();
			} else {
				nOutputExchangeItem++;
			}
		}
	}

	// imports and export IDs
	vector<string> id = vector<string>();
	vector<validationEnum> validation = vector<validationEnum>();
	map<string,int> scalarIDMap = map<string,int>();
	map<string,pair<int,int> > vectorIDMap = map<string,pair<int,int> >();

	for (int i=0; i<(int)rtcTSI.size(); i++) {
		if (rtcTSI[i].getVectorLength().present()) {
			int n = rtcTSI[i].getVectorLength().get();
			vectorIDMap[rtcTSI[i].getId()] = pair<int,int>((int)id.size(), n);
			for (int j=0; j<n; j++) {
				insert_id(id, scalarIDMap, rtcTSI[i].getId() + "[" + to_string(j) + "]");
				if (rtcTSI[i].getValidation().present()) {
					validation.push_back(getValidation(rtcTSI[i].getValidation().get()));
				} else {
					validation.push_back(VALIDATION_NO);
				}
			}
		} else {
			insert_id(id, scalarIDMap, rtcTSI[i].getId());
			if (rtcTSI[i].getValidation().present()) {
				validation.push_back(getValidation(rtcTSI[i].getValidation().get()));
			} else {
				validation.push_back(VALIDATION_NO);
			}
		}
	}
	nImport = (int)id.size();

	for (int i=0; i<(int)rtcTSO.size(); i++) {
		if (rtcTSO[i].getVectorLength().present()) {
			int n = rtcTSO[i].getVectorLength().get();
			vectorIDMap[rtcTSO[i].getId()] = pair<int,int>((int)id.size(), n);
			for (int j=0; j<n; j++) {
				insert_id(id, scalarIDMap, rtcTSO[i].getId() + "[" + to_string(j) + "]");
				if (rtcTSO[i].getValidation().present()) {
					validation.push_back(getValidation(rtcTSO[i].getValidation().get()));
				} else {
					validation.push_back(VALIDATION_NO);
				}
			}
		} else {
			insert_id(id, scalarIDMap, string(rtcTSO[i].getId()));
			if (rtcTSO[i].getValidation().present()) {
				validation.push_back(getValidation(rtcTSO[i].getValidation().get()));
			} else {
				validation.push_back(VALIDATION_NO);
			}
		} 
	}
	int nSeries = (int)id.size();
	nExport = nSeries - nImport;

	vector<string> idArray = vector<string>(nSeries);
	vector<validationEnum> validationArray = vector<validationEnum>(nSeries);
	for (int i=0; i<(int)id.size(); i++) {
		idArray[i] = id[i];
		validationArray[i] = validation[i];
	}

	// check for duplicate ids in time series model
	vector<string> idTest = id;
	std::sort(idTest.begin(), idTest.end());
	int nDuplicate = 0;
	for (int i=1; i<nSeries; i++) {
		if (id[i-1].compare(id[i])==0) {
			piDiagInterface::addLine(1, "duplicate id in time series model, id = " + id[i], TIMESERIESMODEL_CODE);
			nDuplicate++;
		}
	}
	if (nDuplicate>0) throw runtime_error("duplicate ids in time series model found, check 'diag.xml'");

	// diagnostics
	piDiagInterface::addLine(4, "time series model: number of import time series = " + to_string(nImport), TIMESERIESMODEL_CODE);
	piDiagInterface::addLine(4, "time series model: number of export time series = " + to_string(nExport), TIMESERIESMODEL_CODE);
	piDiagInterface::addLine(4, "time series model: total number of time series = " + to_string(nSeries), TIMESERIESMODEL_CODE);


	if (limitedMemory) 
    {
       timeSeriesSparseTensor *tsSparseTensor = new timeSeriesSparseTensor( nEnsemble, 
                                                                             nTimeStep, 
                                                                             time, 
                                                                             nSeries, 
                                                                             idArray, 
                                                                             validationArray, 
                                                                             scalarIDMap, 
                                                                             vectorIDMap,
                                                                             nImport );
       valueTensor = dynamic_cast<timeSeriesTensorInterface*>(tsSparseTensor);
	} 
    else 
    {
       timeSeriesTensor *tsTensor = new timeSeriesTensor( nEnsemble, 
                                                          nTimeStep, 
                                                          time, 
                                                          nSeries, 
                                                          idArray, 
                                                          validationArray, 
                                                          scalarIDMap, 
                                                          vectorIDMap);
			                                              valueTensor = dynamic_cast<timeSeriesTensorInterface*>(tsTensor);
	}

	valueTensor->setEnsembleMap(ensembleMap);

	/*
	 * state interface
	 */
	stateInt = new stateInterface(schemaDir, workDir, valueTensor);

	/*
	 * csv interface
	 */
	csvInt = 0;
	if (dataConfig->getExportSeries().getCSVTimeSeriesFile().present()) {
		char decimalSeparator = dataConfig->getExportSeries().getCSVTimeSeriesFile().get().getDecimalSeparator().c_str()[0];
		char delimiter = dataConfig->getExportSeries().getCSVTimeSeriesFile().get().getDelimiter().c_str()[0];
		bool adjountOutput = false;
		if (dataConfig->getExportSeries().getCSVTimeSeriesFile().get().getAdjointOutput() == true) {
			adjountOutput = true;
		}
		csvInt = new csvInterface(valueTensor, workDir, decimalSeparator, delimiter, adjointOutput);
	} 

	/*
	 * PI time series interface
	 */
	bool importBin = false;
	bool exportBin = false;

	string importPITimeSeriesFile("");
	if (dataConfig->getImportSeries().getPITimeSeriesFile().present()) {
		importPITimeSeriesFile = utils::getAbsoluteFilename(workDir, string(dataConfig->getImportSeries().getPITimeSeriesFile().get().getTimeSeriesFile()));
		// check file for availability
		if (!utils::fileAvailable(importPITimeSeriesFile)) {
			string m = "void timeSeriesModel::readTimeSeriesTensor(...) - file not available: " + importPITimeSeriesFile;
			piDiagInterface::addLine(1, m, TIMESERIESMODEL_CODE);
			throw runtime_error(m.c_str());
		}
		// check for file with binary data
		string binFile = importPITimeSeriesFile.substr(0,importPITimeSeriesFile.length()-3) + "bin";
		if (utils::fileAvailable(binFile)) importBin = true;
		// backwards compatibility
		if (dataConfig->getImportSeries().getPITimeSeriesFile().get().getUseBinFile().present()) {
			string m = "Element -useBinFile- is obsolete, file: " + importPITimeSeriesFile;
			piDiagInterface::addLine(2, m);
		}
		// end backwards compatibility
	}
	string exportPITimeSeriesFile;
	if (dataConfig->getExportSeries().getPITimeSeriesFile().present()) {
		exportPITimeSeriesFile = utils::getAbsoluteFilename(workDir, string(dataConfig->getExportSeries().getPITimeSeriesFile().get().getTimeSeriesFile()));
		if (dataConfig->getExportSeries().getPITimeSeriesFile().get().getUseBinFile().present()) {
			exportBin = dataConfig->getExportSeries().getPITimeSeriesFile().get().getUseBinFile().get();
		}
	}

	// imports
	vector<piTimeSeries> importPITimeSeries = vector<piTimeSeries>(nImportPITimeSeries);
	int rtcIndex = 0;
	int piIndex = 0;
	for (int i=0; i<(int)rtcTSI.size(); i++) {
		if (rtcTSI[i].getPITimeSeries().present()) {

			RTCTimeSeriesComplexType::PITimeSeriesType piTS = rtcTSI[i].getPITimeSeries().get();

			// interpolation / extrapolation
			interpolationOption intOpt = NONE;
			interpolationOption extOpt = NONE;
			if (piTS.getInterpolationOption().present()) {
				if (piTS.getInterpolationOption().get().compare("BLOCK")==0) intOpt = BLOCK;
				else if (piTS.getInterpolationOption().get().compare("LINEAR")==0) intOpt = LINEAR;
				else throw runtime_error("void timeSeriesModel::readTimeSeriesTensor(string schemaLocation, string runtimeFilename, string dataFilename) - interpolation option not implemented");
			} 
			if (piTS.getExtrapolationOption().present()) {
				if (piTS.getExtrapolationOption().get().compare("BLOCK")==0) extOpt = BLOCK;
				else if (piTS.getExtrapolationOption().get().compare("PERIODIC")==0) extOpt = PERIODIC;
				else throw runtime_error("void timeSeriesModel::readTimeSeriesTensor(string schemaLocation, string runtimeFilename, string dataFilename) - extrapolation option not implemented");
			} 

			string locID = piTS.getLocationId();
			string parID = piTS.getParameterId();
			
			// qualifierID
			multiset<string> quaID;
			for (int j=0; j<(int)piTS.getQualifierId().size(); j++) {
				quaID.insert(piTS.getQualifierId()[j]);
			}

			string unit = "";
			if (piTS.getUnit().present()) {
				unit = piTS.getUnit().get();
			}

			if (rtcTSI[i].getVectorLength().present()) {
				for (int j=0; j<rtcTSI[i].getVectorLength().get(); j++) {
					stringstream ss;
					ss << locID << "[" << j << "]";
					importPITimeSeries[piIndex+j] = piTimeSeries(id[rtcIndex+j], rtcIndex+j, ss.str(),
						parID, quaID, unit, intOpt, extOpt);
				}
				piIndex += rtcTSI[i].getVectorLength().get();
			} else {
				importPITimeSeries[piIndex] = piTimeSeries(id[rtcIndex], rtcIndex, locID, parID, quaID, unit, intOpt, extOpt);
				piIndex++;
			}
		}
		if (rtcTSI[i].getVectorLength().present()) {
			rtcIndex += rtcTSI[i].getVectorLength().get();
		} else {
			rtcIndex++;
		}
	}

	// exports
	vector<piTimeSeries> exportPITimeSeries = vector<piTimeSeries>(nExportPITimeSeries);
	piIndex = 0;
	for (int i=0; i<(int)rtcTSO.size(); i++) {
		if (rtcTSO[i].getPITimeSeries().present()) {

			RTCTimeSeriesComplexType::PITimeSeriesType piTS = rtcTSO[i].getPITimeSeries().get();

			interpolationOption intOpt = NONE;
			interpolationOption extOpt = NONE;

			string locID = piTS.getLocationId();
			string parID = piTS.getParameterId();

			// qualifierID
			multiset<string> quaID;
			for (int j=0; j<(int)piTS.getQualifierId().size(); j++) {
				quaID.insert(piTS.getQualifierId()[j]);
			}

			string unit = "";
			if (piTS.getUnit().present()) {
				unit = piTS.getUnit().get();
			}

			if (rtcTSO[i].getVectorLength().present()) {
				for (int j=0; j<rtcTSO[i].getVectorLength().get(); j++) {
					stringstream ss;
					ss << locID << "[" << j << "]";
					exportPITimeSeries[piIndex+j] = piTimeSeries(id[rtcIndex+j], rtcIndex+j, ss.str(),
						parID, quaID, unit, intOpt, extOpt);
				}
				piIndex += rtcTSO[i].getVectorLength().get();
			} else {
				exportPITimeSeries[piIndex] = piTimeSeries(id[rtcIndex], rtcIndex, locID, parID, quaID, unit, intOpt, extOpt);
				piIndex++;
			} 
		}
		if (rtcTSO[i].getVectorLength().present()) {
			rtcIndex += rtcTSO[i].getVectorLength().get();
		} else {
			rtcIndex++;
		}
	}

	piSAX2Int = 0;
	if (!importPITimeSeriesFile.empty()) {
		piSAX2Int = new piTimeSeriesSAX2Handler(schemaDir, valueTensor,
												importPITimeSeries,
												exportPITimeSeriesFile, exportBin,
												exportPITimeSeries,
												nEnsemble);
		piSAX2Int->read(importPITimeSeriesFile, importBin);
	}

	if (limitedMemory) {
        // initialize state after reading of data
		// for full data structures this is done during initialization of the timeSeriesTensor and Matrix objects
		valueTensor->initState();
	}

	/*
	 * OpenMI interface
	 */

	// imports
	vector<openMIExchangeItem> inputExchangeItem = vector<openMIExchangeItem>(nInputExchangeItem);
	rtcIndex = 0;
	int oMIIndex = 0;
	for (int i=0; i<(int)rtcTSI.size(); i++) {
		if (rtcTSI[i].getOpenMIExchangeItem().present()) {

			RTCTimeSeriesComplexType::OpenMIExchangeItemType exItem = rtcTSI[i].getOpenMIExchangeItem().get();
			string eID = exItem.getElementId();
			string qID = exItem.getQuantityId();
			string unit = exItem.getUnit();

			if (rtcTSI[i].getVectorLength().present()) {
				for (int j=0; j<rtcTSI[i].getVectorLength().get(); j++) {
					stringstream ss;
					ss << eID << "[" << j << "]";
					inputExchangeItem[oMIIndex+j] = openMIExchangeItem(id[rtcIndex+j], rtcIndex+j, ss.str(), qID, unit);
				}
				oMIIndex += rtcTSI[i].getVectorLength().get();
			} else {
				inputExchangeItem[oMIIndex] = openMIExchangeItem(id[rtcIndex], rtcIndex, eID, qID, unit);
				oMIIndex++;
			} 
		}
		if (rtcTSI[i].getVectorLength().present()) {
			rtcIndex += rtcTSI[i].getVectorLength().get();
		} else {
			rtcIndex++;
		}
	}

	// exports
	vector<openMIExchangeItem> outputExchangeItem = vector<openMIExchangeItem>(nOutputExchangeItem);
	oMIIndex = 0;
	for (int i=0; i<(int)rtcTSO.size(); i++) {
		if (rtcTSO[i].getOpenMIExchangeItem().present()) {

			RTCTimeSeriesComplexType::OpenMIExchangeItemType exItem = rtcTSO[i].getOpenMIExchangeItem().get();
			string eID = exItem.getElementId();
			string qID = exItem.getQuantityId();
			string unit = exItem.getUnit();

			if (rtcTSO[i].getVectorLength().present()) {
				for (int j=0; j<rtcTSO[i].getVectorLength().get(); j++) {
					stringstream ss;
					ss << eID << "[" << j << "]";
					outputExchangeItem[oMIIndex+j] = openMIExchangeItem(id[rtcIndex+j], rtcIndex+j, ss.str(), qID, unit);
				}
				oMIIndex += rtcTSO[i].getVectorLength().get();
			} else {
				outputExchangeItem[oMIIndex] = openMIExchangeItem(id[rtcIndex], rtcIndex, eID, qID, unit);
				oMIIndex++;
			} 
		}
		if (rtcTSO[i].getVectorLength().present()) {
			rtcIndex += rtcTSO[i].getVectorLength().get();
		} else {
			rtcIndex++;
		}
	}

	openMIInt = new openMIInterface(valueTensor->getTimeSeriesMatrix(0),
										inputExchangeItem, outputExchangeItem);
}

long long getTime(DateTimeComplexType::DateType date, DateTimeComplexType::TimeType time)
{
	int t[7];

	t[0] = date.year();
	t[1] = date.month();
	t[2] = date.day();
	t[3] = time.hours();
	t[4] = time.minutes();
	t[5] = (int)time.seconds();
	t[6] = 0;

	return utils::date2time(t);
}

long long getTimeStep(TimeStepComplexType::UnitType unit)
{
	long long dt = -1;

	if (unit.compare("week")==0) {
		dt = 7*24*3600*1000;
	} else if (unit.compare("day")==0) {
		dt = 24*3600*1000;
	} else if (unit.compare("hour")==0) {
		dt = 3600*1000;
	} else if (unit.compare("minute")==0) {
		dt = 60*1000;
	} else if (unit.compare("second")==0) {
		dt = 1000;
	} else {
		throw runtime_error("time step unit not implemented in getTimeStep()");
	}

	return dt;
}

double timeSeriesModel::getValue(int ensembleId, int tIndex, int sIndex)
{ 
	return this->getTimeSeriesTensor()->getValue(ensembleId, tIndex, sIndex);
}

void timeSeriesModel::setValue(int ensembleId, int tIndex, int sIndex, double value)
{ 
	this->getTimeSeriesTensor()->setValue(ensembleId, tIndex, sIndex, value);
}

void timeSeriesModel::writeState(string filename, int tIndex)
{
    this->stateInt->writeData(filename, tIndex);
}

void timeSeriesModel::writeStateExport(int tIndex, bool isSnapshot)
{
   this->stateInt->writeStateExport(tIndex, isSnapshot);
}