// Copyright (C) 2012 Deltares
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
 * @brief rtcToolsRuntime.cpp
 * @author Camiel van Breugelen, Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */

#include "rtcToolsRuntime.h"
#include "piDiagInterface.h"
#include <utilities/utils.h>
#include "boost/filesystem.hpp"
#include "boost/date_time/posix_time/posix_time.hpp"
#include "dataBinding/pi_run.hxx"
#include <omp.h>

#if _MSC_VER && _MSC_VER<1900 
#define snprintf sprintf_s
#define strncpy strncpy_s
#endif

using namespace std;
using namespace PI;
using namespace boost::posix_time;

const string RTCTOOLSRUNTIME_CODE = "RTCTools.runtime.rtcToolsRuntime";

versionInfo::versionInfo()
{
    isProprietary = false;
    version = OSS_Version;
    revision = OSS_Revision;
}

versionInfo::versionInfo(bool isProprietary, string version, string revision)
{
    isProprietary = isProprietary;
    version = version;
    revision = revision;
}

int rtcToolsRuntime::main(int argc, const char * const argv[], versionInfo info)
{
	rtcToolsRuntime* runtime;

	// --- startup ----------------------------------------------------
	try {
		// runtime constructor and preparation
		boost::filesystem::path defaultDir(".");
		char schemaDir[200] = "";
		char workDir[200] = "";
		
		strncpy(schemaDir, defaultDir.string().c_str(), sizeof(schemaDir));
		strncpy(workDir, defaultDir.string().c_str(), sizeof(workDir));

		// try to get path from calling instance
		if (argc>0) {
			boost::filesystem::path schemaPath(argv[0]);
		    schemaPath.remove_filename();
		    strncpy(schemaDir, schemaPath.string().c_str(), sizeof(schemaDir));
		}

		// scan input for optional schema and work directories
		for (int i=1; i<argc; i++) {
			if (!strcmp(argv[i], "-schemaDir")) {
				strncpy(schemaDir, argv[i+1], sizeof(schemaDir));
			} else if (!strcmp(argv[i], "-workDir")) {
				strncpy(workDir, argv[i+1], sizeof(workDir));
			}
		}

		runtime = new rtcToolsRuntime(schemaDir, workDir, info);

	} catch (exception &e) {
		cout << "int main(int argc, char *argv[]) - error during startup - " << e.what() << "\n";
		return 1; // failure during startup leads to direct termination
	} catch (...) {
		cout << "int main(int argc, char *argv[]) - unknown error during startup\n";
		return 1;
	}

	// set optional number of maximum threads for OpenMP parallelization
	if (runtime->runtimeSettings.nThread>0) {
		omp_set_num_threads(runtime->runtimeSettings.nThread);
	}

	// --- execution ----------------------------------------------------
	int status = 0;
	try {
		// execution
        if (!runtime->runtimeSettings.limitedMemory)
        {
            runtime->execute();
        }
        else
        {
            for (int step = runtime->iStart; step <= runtime->iEnd; step++)
            {
                runtime->writeOutput(step-1, false);
 
                runtime->execute(step);
                // Write state   
                auto currentTime = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getTime(step) / 1000.0; // in seconds   
                if (runtime->getStateFileExportTimeStep() > 0 &&
                    currentTime >= runtime->getStartExportStateFile() &&
                    currentTime < runtime->getEndExportStateFile() &&
                    currentTime - runtime->getLastTimeStateExported() >= runtime->getStateFileExportTimeStep())
                {
                    runtime->writeStateExport(step, true);
                    runtime->setLastTimeStateExported(currentTime);
                }
            }
        }

	} catch (exception &e) {
		cout << "int main(int argc, char *argv[]) - error during execution - " << e.what() << "\n";
		status = 1; // sets flag after failure in execution and tries to write out data
	} catch (...) {
		cout << "int main(int argc, char *argv[]) - unknown error during execution" << "\n";;
		status = 1;
	}

	// --- finalization ----------------------------------------------------
	try {
        // Flush diagnostics
        piDiagInterface::write();

        if (!runtime->runtimeSettings.limitedMemory)
        {
            runtime->finish();
        } else{
            runtime->finish(runtime->iEnd);
        }
		delete runtime;

	} catch (exception &e) {
		cout << "int main(int argc, char *argv[]) - error during finalization - " << e.what() << "\n";
		status = 1;
	} catch (...) {
		cout << "int main(int argc, char *argv[]) - unknown error during finalization" << "\n";;;
		status = 1;
	}

	return status;
}

/**
  * constructor, destructor and related classes for initialization
  */
rtcToolsRuntime::rtcToolsRuntime(const char schemaDir[], const char workDir[], versionInfo info)
{
	string license;

	if (info.isProprietary) {
		license = "Deltares Proprietary License";

		cout << "******************************************************************************" << endl;
		cout << "       This program contains RTC-Tools, a library for real-time control.      " << endl;
		cout << "           It is released under a proprietary Deltares license.               " << endl;
		cout << "      For more information visit http://oss.deltares.nl/web/rtc-tools/        " << endl;
		cout << "******************************************************************************" << endl;
	} else {
		license = "GPL2 License";

		cout << "******************************************************************************" << endl;
		cout << "   This program contains RTC-Tools, a library for real-time control. It is    " << endl;
		cout << "  released as open source code under the GNU General Public License 2 (GPL2). " << endl;
		cout << "      For more information visit http://oss.deltares.nl/web/rtc-tools/        " << endl;
		cout << "******************************************************************************" << endl;
	}

	eval_g_new = true;

    iEnd = 0;

	runtimeSettings.schemaDir = boost::filesystem::path(string(schemaDir));
	runtimeSettings.workDir = boost::filesystem::path(string(workDir));
	
	piDiagInterface::initializeRtcDiagWriter(runtimeSettings.workDir);

	// model identifier
	piDiagInterface::addLine(3, "rtcToolsRuntime: RTC-Tools, version "
		+ info.version + "." + info.revision + " (" + license + ")");
	piDiagInterface::addLine(3, "rtcToolsRuntime: model execution started at "
		+ to_simple_string(second_clock::universal_time()) + " UTC time");

	try {
		// check availability of schemas
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_diag.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_modelparameters.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_sharedtypes.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_state.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_timeseries.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "rtcDataConfig.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "rtcRuntimeConfig.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "rtcSharedTypes.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "rtcStateConfig.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "rtcToolsConfig.xsd"));
		assertFile(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "treeVector.xsd"));

		// read rtcRuntimeConfig.xml and other input files
		parseRuntimeConfigFile();
		parseInputFiles();

	} catch (exception &e) {
		piDiagInterface::addLine(1, "rtcToolsRuntime::rtcToolsRuntime(...) - error - " + string(e.what()));
		piDiagInterface::write();
		throw;
	} catch (...) {
		piDiagInterface::addLine(1, "rtcToolsRuntime::rtcToolsRuntime(...) - unknown error.");
		piDiagInterface::write();
		throw;
	}
}


rtcToolsRuntime::~rtcToolsRuntime(void)
{
	delete schema;
	delete parInt;
	delete tsModel;
	delete[] rtcSim;
}


void rtcToolsRuntime::parseRuntimeConfigFile()
{
	// xsd file
	::xml_schema::Properties rtcRunTimeConfigProperties;
    rtcRunTimeConfigProperties.schema_location("http://www.wldelft.nl/fews",
		utils::xsd_filename(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "rtcRuntimeConfig.xsd")));

	// xml file
	string filename = utils::getAbsoluteFilename(runtimeSettings.workDir, "rtcRuntimeConfig.xml");
	assertFile(filename);

    // parsing file with runtime information
	auto_ptr<RtcRuntimeConfigComplexType> root;
	try {
		root = parseRtcRuntimeConfig(filename, 0, rtcRunTimeConfigProperties);
	} catch (const xml_schema::Exception &e) {
		cout << e << endl;
		throw runtime_error("error parsing rtcRuntimeConfig.xml file");
	}

	piDiagInterface::addLine(4, "rtcToolsRuntime: runtimeSettings.schemaDir = " + runtimeSettings.schemaDir.string());
	piDiagInterface::addLine(4, "rtcToolsRuntime: runtimeSettings.workDir = " + runtimeSettings.workDir.string());

    // files
    if (root->getFiles().present()) {
		RtcRuntimeConfigComplexType::FilesType ft = root->getFiles().get();
		// required
        runtimeSettings.dataConfigFile = ft.getRtcDataConfig().getName();
		runtimeSettings.toolsConfigFile = ft.getRtcToolsConfig().getName();
		// optional
		int nPar = (int)ft.getRtcParameterConfig().size();
		if (nPar>0) {
			runtimeSettings.parameterConfigFile.resize(nPar);
			for (int i=0; i<nPar; i++) {
				rtcRuntimeConfigSettings::parameterFile pf;
				pf.filename = ft.getRtcParameterConfig()[i].getName();
				if (ft.getRtcParameterConfig()[i].getType().compare("TREEVECTOR")==0) {
					pf.type = TREEVECTOR;
				} else if (ft.getRtcParameterConfig()[i].getType().compare("PIMODELPARAMETERS")==0) {
					pf.type = PIMODELPARAMETERS;
				}
				pf.prefix = PREFIX_NONE;
				if (ft.getRtcParameterConfig()[i].getPrefix().compare("LOCATIONID")==0) {
					pf.prefix = PREFIX_LOCATIONID;
				}
				runtimeSettings.parameterConfigFile[i] = pf;
			}
		}
    }

	// import of the parameter file if present
	parInt = new parameterInterface();
	for (int i=0; i<(int)runtimeSettings.parameterConfigFile.size(); i++) {
		filename = utils::getAbsoluteFilename(runtimeSettings.workDir, runtimeSettings.parameterConfigFile[i].filename);
		if (utils::fileAvailable(filename)) {
			piDiagInterface::addLine(4, "rtcToolsRuntime: parameter file available", RTCTOOLSRUNTIME_CODE);
			parInt->addFileContent(runtimeSettings.schemaDir.string(), filename,
				runtimeSettings.parameterConfigFile[i].type,
				runtimeSettings.parameterConfigFile[i].prefix);
		}
	}

	// logging
	runtimeSettings.constraintViolationAsError = false;
	runtimeSettings.constraintViolationTol = 1e-4;

	runtimeSettings.outputObjectiveFunction = false;
	runtimeSettings.outputObjectiveFunctionGradient = false;
	runtimeSettings.outputConstraint = false;
	runtimeSettings.outputJacobian = false;

    if (root->getLogging().present()) {

		RtcRuntimeConfigComplexType::LoggingType log = root->getLogging().get();

		piDiagInterface::setLogLevel(parInt->getIntParameter(log.getLogLevel()));
		if (log.getEventCode().present()) {
			piDiagInterface::setEventCode(parInt->getBoolParameter(log.getEventCode().get()));
		}
		piDiagInterface::setFlush(parInt->getBoolParameter(log.getFlushing()));

		if (log.getConstraintViolationTolerance().present()) {
			runtimeSettings.constraintViolationTol =
				parInt->getDblParameter(log.getConstraintViolationTolerance().get());
		}

		if (log.getReportConstraintViolation().present()) {
			runtimeSettings.constraintViolationAsError = true;
			runtimeSettings.constraintViolationLevel =
				parInt->getIntParameter(log.getReportConstraintViolation().get().getLevel());
		}

		if (log.getOutputObjectiveFunction().present()) {

			string s = log.getOutputObjectiveFunction().get();
			if (s.compare("VALUE")==0 || s.compare("VALUE+DERIVATIVE")==0) {
				runtimeSettings.outputObjectiveFunction = true;
			}
			if (s.compare("DERIVATIVE")==0 || s.compare("VALUE+DERIVATIVE")==0) {
				runtimeSettings.outputObjectiveFunctionGradient = true;
			}

			s = log.getOutputConstraints().get();
			if (s.compare("VALUE")==0 || s.compare("VALUE+DERIVATIVE")==0) {
				runtimeSettings.outputConstraint = true;
			}
			if (s.compare("DERIVATIVE")==0 || s.compare("VALUE+DERIVATIVE")==0) {
				runtimeSettings.outputJacobian = true;
			}
		}
    }

	// get period from user definition
	if (root->getPeriod().getUserDefined().present()) {

		RtcRuntimeConfigComplexType::PeriodType::UserDefinedType ud = root->getPeriod().getUserDefined().get();

		// start and end time
		t1 = getTime(ud.getStartDate().getDate(), ud.getStartDate().getTime());
		t2 = getTime(ud.getEndDate().getDate(), ud.getEndDate().getTime());

		dt = getTimeStep(ud.getTimeStep().getUnit());
		dt *= ud.getTimeStep().getMultiplier();
		dt /= ud.getTimeStep().getDivider();

		// optional forecast time, default is the start time t1
		T0 = t1;
		if (ud.getForecastDate().present()) {
			T0 = getTime(ud.getForecastDate().get().getDate(), ud.getForecastDate().get().getTime());
		}
		iForecastTime = (int)((T0-t1)/dt);

		nEnsemble = 1;
		if (ud.getNumberEnsembles().present()) {
			nEnsemble = (int)ud.getNumberEnsembles().get();
		}
	}

   //start and end time get period from user definition
   if (root->getStateFiles().present())
   {
      auto udsf = root->getStateFiles().get();
      startExportStateFile = getTime(udsf.getStartDate().getDate(), udsf.getStartDate().getTime()) / 1000.0; // in seconds 
      endExportStateFile = getTime(udsf.getEndDate().getDate(), udsf.getEndDate().getTime());
      stateFileExportTimeStep = (double)udsf.getStateTimeStep().get();
   }

	// get period from PI XML time series file
	if (root->getPeriod().getPIInput().present()) {

		RtcRuntimeConfigComplexType::PeriodType::PIInputType pi = root->getPeriod().getPIInput().get();

		// get required info from pi file
		string filename = utils::getAbsoluteFilename(runtimeSettings.workDir, pi.getFile());
		if (!utils::fileAvailable(filename)) {
			piDiagInterface::addLine(1, "void rtcToolsRuntime::parseRunTimeConfigSettings(), file not found - " + filename, RTCTOOLSRUNTIME_CODE);
			throw runtime_error("void rtcToolsRuntime::parseRunTimeConfigSettings() - error - time series import file not found");
		}
		::xml_schema::Properties timeSeriesCollectionComplexTypeProperties;
		timeSeriesCollectionComplexTypeProperties.schema_location("http://www.wldelft.nl/fews/PI",
			utils::xsd_filename(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_timeseries.xsd")));
		auto_ptr<TimeSeriesCollectionComplexType> TimeSeries;
		try {
			TimeSeries = parseTimeSeries(filename, 0, timeSeriesCollectionComplexTypeProperties);
		} catch (const xml_schema::Exception &e) {
			cout << e << endl;
			throw runtime_error("error parsing timeseries_import.xml file");
		}

		// the first time series available
		PI::TimeSeriesComplexType::HeaderType h = TimeSeries->getSeries()[0].getHeader();

		// start and end time
		t1 = getTime(h.getStartDate().getDate(), h.getStartDate().getTime());
		t2 = getTime(h.getEndDate().getDate(), h.getEndDate().getTime());

		dt = getTimeStep(h.getTimeStep().getUnit());
		dt *= h.getTimeStep().getMultiplier();
		dt /= h.getTimeStep().getDivider();

		// optional forecast time, default is the start time t1
		T0 = t1;
		if (h.getForecastDate().present()) {
			T0 = getTime(h.getForecastDate().get().getDate(), h.getForecastDate().get().getTime());
		}
		iForecastTime = (int)((T0-t1)/dt);

		nEnsemble = 1;
		if (pi.getNumberEnsembles().present()) {
		   nEnsemble = (int)pi.getNumberEnsembles().get();
		}
	}

	// get period from PI XML run file
	if (root->getPeriod().getPIRunFile().present()) {

		RtcRuntimeConfigComplexType::PeriodType::PIRunFileType pi = root->getPeriod().getPIRunFile().get();

		// get required info from pi file
		string filename = utils::getAbsoluteFilename(runtimeSettings.workDir, pi.getFile());
		if (!utils::fileAvailable(filename)) {
			piDiagInterface::addLine(1, "void rtcToolsRuntime::parseRunTimeConfigSettings(), file not found - " + filename, RTCTOOLSRUNTIME_CODE);
			throw runtime_error("void rtcToolsRuntime::parseRunTimeConfigSettings() - error - pi run file not found");
		}
		::xml_schema::Properties runComplexTypeProperties;
		runComplexTypeProperties.schema_location("http://www.wldelft.nl/fews/PI",
			utils::xsd_filename(utils::getAbsoluteFilename(runtimeSettings.schemaDir, "pi_run.xsd")));
		auto_ptr<RunComplexType> run;
		try {
			run = parseRun(filename, 0, runComplexTypeProperties);
		} catch (const xml_schema::Exception &e) {
			cout << e << endl;
			throw runtime_error("error parsing run.xml file");
		}

		// start and end time
		t1 = getTime(run->getStartDateTime().getDate(), run->getStartDateTime().getTime());
		t2 = getTime(run->getEndDateTime().getDate(), run->getEndDateTime().getTime());

		dt = getTimeStep(pi.getTimeStep().getUnit());
		dt *= pi.getTimeStep().getMultiplier();
		dt /= pi.getTimeStep().getDivider();

		// optional forecast time, default is the start time t1
		T0 = getTime(run->getTime0().getDate(), run->getTime0().getTime());
		iForecastTime = (int)((T0-t1)/dt);

		nEnsemble = 1;
		if (pi.getNumberEnsembles().present()) {
		   nEnsemble = (int)pi.getNumberEnsembles().get();
		}
	}

	// ensemble map (relevant for scenario tree mode)
	ensembleMap = vector<int>(nEnsemble);
	for (int i=0; i<nEnsemble; i++) {
		ensembleMap[i] = i;
	}

	char buffer[500];
    char b[4][50];
    snprintf(buffer, sizeof(buffer), "%s %s - %s %s, dt[ms] = %d",
		utils::time2datestring(t1, b[0]), utils::time2timestring(t1, b[1]),
		utils::time2datestring(t2, b[2]), utils::time2timestring(t2, b[3]), dt);
    piDiagInterface::addLine(3, "rtcToolsRuntime: simulation period = " + string(buffer), RTCTOOLSRUNTIME_CODE);

    snprintf(buffer, sizeof(buffer), "%d", nEnsemble);
    piDiagInterface::addLine(4, "rtcToolsRuntime: number of ensemble members = " + string(buffer), RTCTOOLSRUNTIME_CODE);

    // single mode definition
    if (root->getMode().present()) {

        runtimeSettings.modeInfo.push_back(getModeInfo(root->getMode().get()));

	// definition of multiple modes
    } else if (root->getModes().present()) {

		for (int i=0; i<(int)root->getModes().get().getMode().size(); i++) {
			runtimeSettings.modeInfo.push_back(
				getModeInfo(root->getModes().get().getMode()[i]));
		}

	// defaut settings
	} else {

		rtcRuntimeConfigSettings::modeInfoContainer c;

		// default is a simulation over the complete period
		c.mode = SIMULATE;
		c.period = COMPLETE;

		runtimeSettings.modeInfo.push_back(c);
	}

	// parallelization
	runtimeSettings.nThread = -1;
	runtimeSettings.parallelEnsembleSim = false;
	runtimeSettings.parallelEnsembleCon = false;
	runtimeSettings.parallelInternalSim = false;
	runtimeSettings.parallelInternalCon = false;

	if (root->getParallelization().present()) {

		// number of maximum threads
		if (root->getParallelization().get().getNThread().present()) {
			runtimeSettings.nThread = root->getParallelization().get().getNThread().get();
		}

		// parallelization options for simulation and objective function evaluation
		if (root->getParallelization().get().getSimulation().present()) {
			if (root->getParallelization().get().getSimulation().get().find("ENSEMBLE") != string::npos) {
				runtimeSettings.parallelEnsembleSim = true;
			}
			if (root->getParallelization().get().getSimulation().get().find("INTERNAL") != string::npos) {
				runtimeSettings.parallelInternalSim = true;
			}
		}

		// parallelization options for constraints and constaint Jacobian
		if (root->getParallelization().get().getConstraints().present()) {
			if (root->getParallelization().get().getConstraints().get().find("ENSEMBLE") != string::npos) {
				runtimeSettings.parallelEnsembleCon = true;
			}
			if (root->getParallelization().get().getConstraints().get().find("INTERNAL") != string::npos) {
				runtimeSettings.parallelInternalCon = true;
			}
		}
	}
}


rtcRuntimeConfigSettings::modeInfoContainer rtcToolsRuntime::getModeInfo(fews::RtcRuntimeConfigComplexType::ModeType m) {

	rtcRuntimeConfigSettings::modeInfoContainer c;

    if (m.getSimulation().present()) {

		c.mode = SIMULATE;
		if (m.getSimulation().get().getLimitedMemory().present()) {
			runtimeSettings.limitedMemory = m.getSimulation().get().getLimitedMemory().get();
		}

		c.period = COMPLETE;
		if (m.getSimulation().get().getPeriod().present()) {
			if (m.getSimulation().get().getPeriod().get().compare("UPDATE")==0) {
				c.period = UPDATE;
			}
			if (m.getSimulation().get().getPeriod().get().compare("FORECAST")==0) {
				c.period = FORECAST;
			}
		}

	} else {
		throw runtime_error("void rtcToolsRuntime::parseRuntimeConfigFile() - error");
	}

	return c;
}


void rtcToolsRuntime::parseInputFiles()
{
	boost::filesystem::path sDir = runtimeSettings.schemaDir;
	boost::filesystem::path wDir = runtimeSettings.workDir;

	// check if one mode is different than simulation, in this case allocate adjoint matrix
	bool allocateAdjoint = false;
	for (int i=0; i<(int)runtimeSettings.modeInfo.size(); i++) {
		if (runtimeSettings.modeInfo[i].mode!=SIMULATE) allocateAdjoint = true;
	}

	// read time series data
	string filename = utils::getAbsoluteFilename(wDir, runtimeSettings.dataConfigFile);
	assertFile(filename);
	tsModel = new timeSeriesModel(sDir, wDir, filename, t1, t2, dt, nEnsemble, ensembleMap, allocateAdjoint, runtimeSettings.limitedMemory);
	int nTimeStep = tsModel->getTimeSeriesTensor()->getNTimeStep();

	// set the ensemble map
	tsModel->getTimeSeriesTensor()->setEnsembleMap(ensembleMap);

	// read schematisation
	filename = utils::getAbsoluteFilename(wDir, runtimeSettings.toolsConfigFile);
	assertFile(filename);
	schema = new schematisation(sDir, filename, tsModel->getTimeSeriesTensor(), parInt, &runtimeSettings);

	// prepare simulation
	rtcSim = new rtcToolsSimulator[ensembleMap.size()];
	for (int i=0; i<(int)ensembleMap.size(); i++) {
		rtcSim[i] = rtcToolsSimulator(i, tsModel->getTimeSeriesTensor()->getTimeSeriesMatrix(ensembleMap[i]), schema,
			getPVec()[i], &runtimeSettings);
	}

	// state import
	tsModel->read();

	iStart = 1;
	iEnd = tsModel->getTimeSeriesTensor()->getNTimeStep()-1;

    // prepare() is already called in constructor of OpenMIINterface, is this call redundant?
	tsModel->getOpenMIInterface()->prepare();

	openOutput();

	// data validation of the input time series
	tsModel->getTimeSeriesTensor()->validate(tsModel->getNImport(), iForecastTime);
}


/**
  * get and set functions
  **/
void rtcToolsRuntime::setParameters(int n, string* id, double* input)
{
	try {
		// put parameters into interface
		this->parInt->setDblParameters(n, id, input);

		// re-initialize schematization with new parameters
		this->schema->initialize(this->parInt);
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "rtcToolsRuntime::setParameters(int n, double *input) - unknown error.", RTCTOOLSRUNTIME_CODE);
		throw;
	}
}


double rtcToolsRuntime::simulate()
{
	try {
		return simulate(0, (double*)0);
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "rtcToolsRuntime::simulate() - error - " + string(e.what()) + ".", RTCTOOLSRUNTIME_CODE);
		throw;
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "rtcToolsRuntime::simulate() - unknown error.", RTCTOOLSRUNTIME_CODE);
		throw;
	}
}


void rtcToolsRuntime::simulate(int iStep)
{
	try {
		if ((iStep<1) || (iStep>=tsModel->getTimeSeriesTensor()->getNTimeStep())) {
			piDiagInterface::addLine(1, "void rtcToolsRuntime::simulate(int iStep), time step index outside of range.", RTCTOOLSRUNTIME_CODE);
			return;
		}

		for (int i=0; i<(int)ensembleMap.size(); i++) {
			rtcSim[i].simulate(iStep);
		}
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "rtcToolsRuntime::simulate(int iStep) - error - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
		throw;
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "rtcToolsRuntime::simulate(int iStep) - unknown error in rtcTools", RTCTOOLSRUNTIME_CODE);
		throw;
	}
}


double rtcToolsRuntime::simulate(int n, const double *input, double*** JInc3DArray)
{
	double J = 0;

	vector<double> Je(tsModel->getTimeSeriesTensor()->getEnsembleMap().size());

	// simulate ensembles and execute the branch-related objective function terms
	if (runtimeSettings.parallelEnsembleSim) {
		#pragma omp parallel for schedule(dynamic)
		for (int i=0; i<(int)ensembleMap.size(); i++) {
			// exception is handled locally to avoid an uncontrolled program termination
			try {
				if (JInc3DArray) {
					Je[i] = rtcSim[i].simulate(iStart, iEnd, JInc3DArray[ensembleMap[i]]);
				} else {
					Je[i] = rtcSim[i].simulate(iStart, iEnd);
				}
			} catch (exception &e) {
				{
					piDiagInterface::addLine(1, "double rtcToolsRuntime::simulate(int n, double *input) - error in parallel simulation - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
					throw runtime_error("double rtcToolsRuntime::simulate(int n, double *input) - error in parallel simulation - " + string(e.what()));
				}
			} catch (...) {
				{
					piDiagInterface::addLine(1, "double rtcToolsRuntime::simulate(int n, double *input) - unknown error in parallel simulation", RTCTOOLSRUNTIME_CODE);
					throw runtime_error("double rtcToolsRuntime::simulate(int n, double *input) - unknown error in parallel simulation - ");
				}
			}
		}
	} else {
		try {
			for (int i=0; i<(int)ensembleMap.size(); i++) {
				if (JInc3DArray) {
					Je[i] = rtcSim[i].simulate(iStart, iEnd, JInc3DArray[ensembleMap[i]]);
				} else {
					Je[i] = rtcSim[i].simulate(iStart, iEnd);
				}
			}
		} catch (exception &e) {
			piDiagInterface::addLine(1, "double rtcToolsRuntime::simulate(int n, double *input) - error in sequential simulation - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
			throw runtime_error("double rtcToolsRuntime::simulate(int n, double *input) - error in sequential simulation - " + string(e.what()));
		} catch (...) {
			piDiagInterface::addLine(1, "double rtcToolsRuntime::simulate(int n, double *input) - unknown error in sequential simulation", RTCTOOLSRUNTIME_CODE);
			throw runtime_error("double rtcToolsRuntime::simulate(int n, double *input) - unknown error in sequential simulation");
		}
	}

	// and add total objective function value
	for (int i=0; i<(int)ensembleMap.size(); i++) {
		J += Je[i];
	}

	// check for NaN as objective function value
	if (J!=J) {
		piDiagInterface::addLine(1, "double rtcToolsRuntime::simulate(int n, double *input) - objective function value is NaN", RTCTOOLSRUNTIME_CODE);
		throw runtime_error("double rtcToolsRuntime::simulate(int n, double *input) - objective function value is NaN");
	}

	return J;
}


void rtcToolsRuntime::openOutput()
{
	try {
		tsModel->openFiles(false);
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::openOutput(void) - error - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
		throw;
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::openOutput(void) - unknown error in rtcTools", RTCTOOLSRUNTIME_CODE);
		throw;
	}

	piDiagInterface::write();
}


void rtcToolsRuntime::writeOutput(int timeStep, bool isFinalTimeStep)
{
	try 
    {
		tsModel->write(false, timeStep, isFinalTimeStep, getCSVExtraColumnNames(), getCSVExtraValues());
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::writeOutput(int, bool) - error - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
		throw;
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::writeOutput(int, bool) - unknown error in rtcTools", RTCTOOLSRUNTIME_CODE);
		throw;
	}
}


void rtcToolsRuntime::finish(int timeStep /* =0*/)
{
	try {
		// data validation
		tsModel->getTimeSeriesTensor()->validate(tsModel->getTimeSeriesTensor()->getNSeries(), iForecastTime);

		// adjoint output in sensitivity and optimization modes
		bool optimizationMode = false;
		for (int i=0; i<(int)runtimeSettings.modeInfo.size(); i++) {
			if (runtimeSettings.modeInfo[i].mode == FIRSTORDERSENSITIVITY ||
				runtimeSettings.modeInfo[i].mode == OPTIMIZE ||
				runtimeSettings.modeInfo[i].mode == CLOSEDLOOP) {
					optimizationMode = true;
			}
		}

		// export time series
		if (timeStep > 0)
			tsModel->write(optimizationMode, timeStep, true, getCSVExtraColumnNames(), getCSVExtraValues());
		else
			tsModel->write(optimizationMode);
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::finish(void) - error - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
		piDiagInterface::write();
		throw;
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::finish(void) - unknown error in rtcTools", RTCTOOLSRUNTIME_CODE);
		piDiagInterface::write();
		throw runtime_error("void rtcToolsRuntime::finish() - unknown error");
	}

	piDiagInterface::write();
	piDiagInterface::freeRtcDiagWriter();
}


void rtcToolsRuntime::finishFromOpenMI(int timeStep)
{
	try {
		bool isFinalTimeStep = true;
		writeOutput(timeStep, isFinalTimeStep);
		tsModel->closeFiles(false);
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::finish(void) - error - " + string(e.what()), RTCTOOLSRUNTIME_CODE);
		throw ;
	}
	catch (...)
	{
		piDiagInterface::addLine(1, "void rtcToolsRuntime::finish(void) - unknown error in rtcTools", RTCTOOLSRUNTIME_CODE);
		throw;
	}

	piDiagInterface::write();
	piDiagInterface::freeRtcDiagWriter();
}


void rtcToolsRuntime::getTimeSeriesMatrix(int nTimeStep, int nSeries, double *tsm)
{
	if (!runtimeSettings.limitedMemory) {
        timeseries::timeSeriesMatrix* tsm_temp = dynamic_cast<timeseries::timeSeriesMatrix*>(tsModel->getTimeSeriesTensor()->getTimeSeriesMatrix(0));
		double **tsm_intern = tsm_temp->getValueMatrix();

		for (int i=0; i<nTimeStep; i++) {
			for (int j=0; j<nSeries; j++) {
				tsm[i+nTimeStep*j] = tsm_intern[i][j];
			}
		}
	} else {
		piDiagInterface::addLine(1, "void rtcToolsRuntime::getTimeSeriesMatrix - method not available if limitedMemory option is true", RTCTOOLSRUNTIME_CODE);
	}

}


void rtcToolsRuntime::setTimeSeriesMatrix(int nTimeStep, int nSeries, double *tsm)
{
	if (!runtimeSettings.limitedMemory) {
		timeseries::timeSeriesMatrix* tsMatrix= dynamic_cast<timeSeriesMatrix*>(tsModel->getTimeSeriesTensor()->getTimeSeriesMatrix(0));
		double** tsm_intern = tsMatrix->getValueMatrix();
	    for (int i=0; i<nTimeStep; i++) {
			for (int j=0; j<nSeries; j++) {
				tsm_intern[i][j] = tsm[i+nTimeStep*j];
			}
		}
	} else {
		piDiagInterface::addLine(1, "void rtcToolsRuntime::setTimeSeriesMatrix - method not available if limitedMemory option is true", RTCTOOLSRUNTIME_CODE);
	}
}


bool rtcToolsRuntime::showDebug()
{
	return piDiagInterface::getLogLevel() == 4;
}


bool rtcToolsRuntime::execute(int timeStep_in /* =0 */)
{
	char buffer[200];

    for (int i=0; i<(int)runtimeSettings.modeInfo.size(); i++) {
		// complete runtime by default ...
		this->iStart = 1;
		this->iEnd = tsModel->getTimeSeriesTensor()->getNTimeStep()-1;

		// ... or reduction to update or forecast period if applicable
		if (runtimeSettings.modeInfo[i].period==UPDATE) {
			this->iEnd = this->iForecastTime;
		} else if (runtimeSettings.modeInfo[i].period==FORECAST) {
			this->iStart = this->iForecastTime+1;
		}
		if (runtimeSettings.modeInfo[i].mode==SIMULATE) {

			clock_t t = clock();
			if (!runtimeSettings.limitedMemory) {
				double J = simulate();         // Perform entire simulation

				snprintf(buffer, sizeof(buffer), "rtcToolsRuntime: objective function value = %lf", J);
				piDiagInterface::addLine(3, string(buffer));

            }
            else {
                // shift the window over the input timeseries
                // tsModel->getTimeSeriesTensor()->incrementTimeStep(1);
                simulate(timeStep_in);            // Simulate until given time step
			}
			clock_t sim = clock()-t;

			snprintf(buffer, sizeof(buffer), "rtcToolsRuntime: CPU time (simulation) = %d ms", (int)sim);
			piDiagInterface::addLine(3, string(buffer));

		}
	}

	// returns false for correct execution, true if an error occured
    return piDiagInterface::getIncludeError();
}


bool rtcToolsRuntime::executeFromOpenMI(int timeStep /* =0 */)
{
    simulate(timeStep);
	bool isFinalTimeStep = false;
	writeOutput(timeStep, isFinalTimeStep);

    return true;
}


/**
 * Some private helper functions
 */
vector<double> rtcToolsRuntime::getPVec()
{
	vector<double> pVec;
	pVec = vector<double>(ensembleMap.size());
	for (int i=0; i<(int)ensembleMap.size(); i++) pVec[i] = 1.0/(double)ensembleMap.size();

	return pVec;
}


void rtcToolsRuntime::assertFile(string filename)
{
	if (utils::fileAvailable(filename)==false) {
		string m = "file not found: " + filename;
		piDiagInterface::addLine(1, "void rtcToolsRuntime::assertFile(string filename), " + filename, RTCTOOLSRUNTIME_CODE);
		throw runtime_error(m.c_str());
	}
}


long long rtcToolsRuntime::getTime(fews::DateTimeComplexType::DateType date, fews::DateTimeComplexType::TimeType time)
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


long long rtcToolsRuntime::getTimeStep(fews::TimeStepComplexType::UnitType unit)
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
		throw runtime_error("error - time step unit not implemented in long long rtcToolsHelperFunctions::getTimeStep(TimeStepComplexType::UnitType unit)");
	}

	return dt;
}

void rtcToolsRuntime::writeStateExport(int tIndex, bool isSnapshot)
{
   // state export
   tsModel->writeStateExport(tIndex, isSnapshot);
}

double rtcToolsRuntime::getStateFileExportTimeStep() const
{
   return stateFileExportTimeStep;
}

double rtcToolsRuntime::getStartExportStateFile() const
{
   return  startExportStateFile;
};

double rtcToolsRuntime::getEndExportStateFile() const
{
   return  endExportStateFile;
};

double rtcToolsRuntime::getLastTimeStateExported()
{
   return  lastTimeStateExported;
};

void rtcToolsRuntime::setLastTimeStateExported(const double value)
{
   lastTimeStateExported = value;
};

std::vector<string>& rtcToolsRuntime::getCSVExtraColumnNames()
{

    if (!additionalTimeSerieNames.empty() || schema == nullptr)
    {
        return additionalTimeSerieNames;
    }

    const int numTriggers = schema->getNTrigger();
    const int numRules = schema->getNRule();

    additionalTimeSerieNames.resize(numTriggers + numRules );
    additionalTimeseries.resize(numTriggers + numRules);


    for (int i = 0; i < numTriggers; ++i)
    {
        additionalTimeSerieNames[i] = schema->getTriggers()[i]->getName() + "_isActive";
    }

    for (int i = 0; i < numRules; ++i)
    {
        additionalTimeSerieNames[numTriggers + i] = schema->getRules()[i]->getName() + "_isActive";
    }

    return additionalTimeSerieNames;
}

std::vector<int>& rtcToolsRuntime::getCSVExtraValues()
{
    if(schema == nullptr)
    {
        return additionalTimeseries;
    }

    int numRules = schema->getNRule();
    int numTriggers = schema->getNTrigger();
    additionalTimeseries.resize(numTriggers + numRules);

    for (int i = 0; i < numTriggers; ++i)
    {
        additionalTimeseries[i] = schema->getTriggers()[i]->isActive() ? 1 : 0;
    }

    for (int i = 0; i < numRules; ++i)
    {
        additionalTimeseries[numTriggers + i] = schema->getRules()[i]->isActive()? 1 : 0;
    }
    return additionalTimeseries;
}
