#pragma once 
#include "bmi.h" 
#include "dimr.h"
#include "dimr_lib_version.h"

#ifndef _WIN32
#include "config.h"
#include <dlfcn.h>
#include <libgen.h>
#endif
#include <limits.h>

#if defined (MEMCHECK)
#include <mcheck.h>
#endif


#ifdef _WIN32
#  include <Strsafe.h>
#  include <windows.h>
#  include <direct.h>
#  define strdup _strdup
#  define chdir _chdir
#  define getcwd _getcwd
#  define dup2 _dup2
#else
#  include <unistd.h>
#endif

#include <cstring>
#include <string>
#include <sstream>

// static added to prevent name conflicts on Linux.
static Dimr * thisDimr = Dimr::GetInstance();     // global pointer to single object instance

extern "C" {
	//------------------------------------------------------------------------------
	BMI_API void set_logger_callback(WriteCallback writeCallBack)
	{
		if (thisDimr == NULL)
		{
			thisDimr = Dimr::GetInstance();
		}
		thisDimr->log->SetWriteCallBack(writeCallBack);
	}

	//------------------------------------------------------------------------------
	BMI_API void set_dimr_logger(Log * loggerFromDimrExe)
	{
		if (thisDimr == NULL)
		{
			thisDimr = Dimr::GetInstance();
		}
		thisDimr->log = loggerFromDimrExe;
	}

	BMI_API void set_logger(BMILogger logger)
	{
		if (thisDimr == NULL)
		{
			thisDimr = Dimr::GetInstance();
		}

		thisDimr->log->SetExternalLogger(logger);
		//Update or add the extertnal logger function in the kernels
		for (int i = 0; i < thisDimr->componentsList.numComponents; i++) {
			if (thisDimr->componentsList.components[i].type == COMP_TYPE_FLOW1D)
			{
				if (thisDimr->componentsList.components[i].setLogger != NULL) {
					thisDimr->componentsList.components[i].setLogger(logger);
				}
				double level = (double)thisDimr->logLevel;
				thisDimr->componentsList.components[i].dllSetVar("debugLevel", (const void *)&level);
			}
		}
	}
	//------------------------------------------------------------------------------
	BMI_API int initialize(const char * configfile) {

		// Return to library users informative messages when exceptions are thrown
		try
		{
			int nSettingsSet, nParamsSet;
			if (thisDimr == NULL)
			{
				thisDimr = Dimr::GetInstance();
			}

			if (thisDimr->redirectFile != NULL)
			{
				// RedirectFile must be including the full path:
				// - Get the basename (platform dependent implementation)
				// - if (redirectfile == basename) then
				//       Make copy of redirectfile
				//       Put CWD in redirectfile
				//       redirectfile = redirectfile + / + copy
				char *fileBasename = new char[MAXSTRING];
#ifndef _WIN32
				fileBasename = strdup(basename(thisDimr->redirectFile));
#else
				char * ext = new char[5];
				_splitpath(thisDimr->redirectFile, NULL, NULL, fileBasename, ext);
				StringCbCatA(fileBasename, MAXSTRING, ext);
				delete[] ext;
#endif
				if (strcmp(thisDimr->redirectFile, fileBasename) == 0)
				{
					char *filenameCopy = new char[MAXSTRING];
					strcpy(filenameCopy, thisDimr->redirectFile);

					delete[] thisDimr->redirectFile;
					thisDimr->redirectFile = (char *)malloc((MAXSTRING) * sizeof(char));

					if (!getcwd(thisDimr->redirectFile, MAXSTRING))
					{
						throw Exception(true, Exception::ERR_OS, "ERROR obtaining the current working directory (init)");
					}

					strcat(thisDimr->redirectFile, thisDimr->dirSeparator);
					strcat(thisDimr->redirectFile, filenameCopy);
					delete[] filenameCopy;
				}
				// Redirection to file is currently handled in the logger by writing directly to the specified file
				thisDimr->log->redirectFile = thisDimr->redirectFile;
				printf("DIMR messages are redirected to file \"%s\"\n", thisDimr->redirectFile);
				fflush(stdout);
				// Create an empty file
				FILE * fp = fopen(thisDimr->redirectFile, "w+");
				fclose(fp);
				delete[] fileBasename;
			}

			thisDimr->log->Write(INFO, thisDimr->my_rank, getfullversionstring_dimr_lib());
			thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr:initialize(%s)", configfile);
			//
			//
			// Read XML configuration file into tree structure
			thisDimr->configfile = configfile;
			FILE * conf;
			if (strcmp(thisDimr->configfile, "-") == 0)
				conf = stdin;
			else
			{
				conf = fopen(thisDimr->configfile, "r");
				if (conf == NULL)
				{
					throw Exception(true, Exception::ERR_OS, "Cannot open configuration file \"%s\"", thisDimr->configfile);
				}
			}

			thisDimr->config = new XmlTree(conf);
			thisDimr->config->ExpandEnvironmentVariables();
			fclose(conf);
			//
			// Build controlBlock administration by scanning the XmlTree
			thisDimr->log->Write(DEBUG, thisDimr->my_rank, "Build controlBlock administration by scanning the XmlTree");
			thisDimr->scanConfigFile();
			//
			// ToDo: check whether a core dump is requested on abort; if so set global variable for Dimr_CoreDump
			//
			// This is a good time to attach to the processes in case you want to debug
			thisDimr->processWaitFile();
			//
			// Build connection with dlls
			thisDimr->connectLibs();
			// Init the timers before calling the dllInitialize routines!
			thisDimr->timersInit();

			// Store dimr absolute path
			thisDimr->dimrWorkingDirectory = new char[MAXSTRING];
			if (!getcwd(thisDimr->dimrWorkingDirectory, MAXSTRING))
			{
				thisDimr->log->Write(FATAL, thisDimr->my_rank, "Cannot get the current working directory");
			}

			//
			// Initialize the components in the first controlBlock only
			if (thisDimr->control->subBlocks[0].type == CT_PARALLEL)
			{
				thisDimr->runParallelInit(&(thisDimr->control->subBlocks[0]));
			}
			else
			{
				// Start block

				// Hack for WAVE:
				if (thisDimr->control->subBlocks[0].unit.component->type == COMP_TYPE_WAVE) {
					int *waveModePtr = NULL;
					const char *key = "mode";
					(thisDimr->control->subBlocks[0].unit.component->dllGetVar) (key, (void**)(&waveModePtr));
					*waveModePtr = 0;
				}

				chdir(thisDimr->control->subBlocks[0].unit.component->workingDir);
				thisDimr->log->Write(INFO, thisDimr->my_rank, "%s.Initialize(%s)", thisDimr->control->subBlocks[0].unit.component->name, thisDimr->control->subBlocks[0].unit.component->inputFile);
				// SetKeyVals for settings (before initialize)
				nSettingsSet = thisDimr->control->subBlocks[0].unit.component->dllSetKeyVals(thisDimr->control->subBlocks[0].unit.component->settings);
				thisDimr->timerStart(thisDimr->control->subBlocks[0].unit.component);
				thisDimr->control->subBlocks[0].unit.component->result = (thisDimr->control->subBlocks[0].unit.component->dllInitialize) (thisDimr->control->subBlocks[0].unit.component->inputFile);

				if(thisDimr->control->subBlocks[0].unit.component->result != 0)
				{
					stringstream ss;
					ss << thisDimr->control->subBlocks[0].unit.component->result;
					std::string componentName = thisDimr->control->subBlocks[0].unit.component->name;
					std::string message = "#### ERROR: dimr initialize ABORT,: " + componentName + " initialize failed, with return value " + ss.str() + " \n";
					printf(message.c_str());
					thisDimr->log->Write(FATAL, thisDimr->my_rank, message.c_str(), thisDimr->configfile);
					return thisDimr->control->subBlocks[0].unit.component->result;
				}
				
				thisDimr->timerEnd(thisDimr->control->subBlocks[0].unit.component);
				// SetKeyVals for parameters (after initialize)
				nParamsSet = thisDimr->control->subBlocks[0].unit.component->dllSetKeyVals(thisDimr->control->subBlocks[0].unit.component->parameters);
				(thisDimr->control->subBlocks[0].unit.component->dllGetStartTime) (&thisDimr->control->subBlocks[0].tStart);
				(thisDimr->control->subBlocks[0].unit.component->dllGetEndTime) (&thisDimr->control->subBlocks[0].tEnd);
				(thisDimr->control->subBlocks[0].unit.component->dllGetTimeStep) (&thisDimr->control->subBlocks[0].tStep);
				(thisDimr->control->subBlocks[0].unit.component->dllGetCurrentTime) (&thisDimr->control->subBlocks[0].tCur);
			}
		}
		catch (Exception & ex)
		{
			printf("#### ERROR: dimr initialize ABORT: %s\n", ex.message);
			thisDimr->log->Write(FATAL, thisDimr->my_rank, ex.message, thisDimr->configfile);
			return ex.errorCode;
		}
		catch (...)
		{
			printf("#### ERROR: dimr initialize ABORT with unknown exception\n");
			return Exception::ERR_UNKNOWN;
		}
		// all ok (no exceptions)
		return 0;
	}

	//------------------------------------------------------------------------------
	BMI_API int update(double tStep)
	{
		// Return to library users informative messages when exceptions are thrown
		try
		{
			thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:update");
			// Execute update on the first controlBlock only
			if (thisDimr->control->subBlocks[0].type == CT_PARALLEL) {
				thisDimr->runControlBlock(&(thisDimr->control->subBlocks[0]), tStep, GLOBAL_PHASE_UPDATE);
			}
			else {
				// Start block
				chdir(thisDimr->control->subBlocks[0].unit.component->workingDir);
				thisDimr->log->Write(INFO, thisDimr->my_rank, "%s.Update(%6.1f)", thisDimr->control->subBlocks[0].unit.component->name, tStep);
				thisDimr->timerStart(thisDimr->control->subBlocks[0].unit.component);
				int state = (thisDimr->control->subBlocks[0].unit.component->dllUpdate) (tStep);
                if (state != 0)
                {
                    stringstream ss;
                    ss << state;
                    std::string componentName = thisDimr->control->subBlocks[0].unit.component->name;
                    std::string message = "#### ERROR: dimr update ABORT,: " + componentName + " update failed, with return value " + ss.str() + " \n";
                    printf(message.c_str());
                    thisDimr->log->Write(FATAL, thisDimr->my_rank, message.c_str(), thisDimr->configfile);
                    return state;
                }
				thisDimr->timerEnd(thisDimr->control->subBlocks[0].unit.component);
				(thisDimr->control->subBlocks[0].unit.component->dllGetCurrentTime) (&thisDimr->control->subBlocks[0].tCur);
			}
		}
		catch (Exception & ex)
		{
			printf("#### ERROR: dimr update ABORT: %s\n", ex.message);
			thisDimr->log->Write(FATAL, thisDimr->my_rank, ex.message, thisDimr->configfile);
			return ex.errorCode;
		}
		catch (...)
		{
			printf("#### ERROR: dimr update ABORT with unknown exception\n");
			return Exception::ERR_UNKNOWN;
		}
		return 0;
	}

	//------------------------------------------------------------------------------
	BMI_API int finalize(void)
	{
		// Return to library users informative messages when exceptions are thrown
		try
		{
			thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:finalize");
			// Execute finalize on the first controlBlock and
			// initialize, step, finalize on all other controlBlocks
			if (thisDimr->control->subBlocks[0].type == CT_PARALLEL) {
				thisDimr->runParallelFinish(&(thisDimr->control->subBlocks[0]));
			}
			else {
				// Start block
				chdir(thisDimr->control->subBlocks[0].unit.component->workingDir);
				thisDimr->log->Write(INFO, thisDimr->my_rank, "%s.Finalize()", thisDimr->control->subBlocks[0].unit.component->name);
				thisDimr->timerStart(thisDimr->control->subBlocks[0].unit.component);
				int state = (thisDimr->control->subBlocks[0].unit.component->dllFinalize) ();
				if(state != 0)
				{
					stringstream ss;
					ss << state;
					std::string componentName = thisDimr->control->subBlocks[0].unit.component->name;
					std::string message = "#### ERROR: dimr finalize ABORT,: " + componentName + " finalize failed, with return value " + ss.str() + " \n";
					printf(message.c_str());
					thisDimr->log->Write(FATAL, thisDimr->my_rank, message.c_str(), thisDimr->configfile);
					return state;
				}
				thisDimr->timerEnd(thisDimr->control->subBlocks[0].unit.component);
				fflush(stdout);
			}
			thisDimr->timersFinish();
			for (int i = 1; i < thisDimr->control->numSubBlocks; i++) {
				thisDimr->runControlBlock(&(thisDimr->control->subBlocks[i]), 999999999.0, GLOBAL_PHASE_FINISH);
			}

			if (thisDimr->redirectFile != NULL) {
				thisDimr->log->redirectFile = NULL;
				printf("Finished: redirecting DIMR messages to file \"%s\"", thisDimr->redirectFile);
				fflush(stdout);
			}
		}
		catch (Exception & ex)
		{
			printf("#### ERROR: dimr finalize ABORT: %s\n", ex.message);
			thisDimr->log->Write(FATAL, thisDimr->my_rank, ex.message, thisDimr->configfile);
			return ex.errorCode;
		}
		catch (...)
		{
			printf("#### ERROR: dimr finalize ABORT with unknown exception\n");
			return Exception::ERR_UNKNOWN;
		}
		return 0;
	}

	//------------------------------------------------------------------------------
	BMI_API void get_start_time(double * tStart)
	{
		thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:get_start_time");
		if (thisDimr->control->subBlocks[0].type == CT_PARALLEL)
		{
			*tStart = thisDimr->control->subBlocks[0].subBlocks[thisDimr->control->subBlocks[0].masterSubBlockId].tStart;
		}
		else {
			// Start block
			*tStart = thisDimr->control->subBlocks[0].tStart;
		}
	}

	//------------------------------------------------------------------------------
	BMI_API void get_end_time(double * tEnd) {
		thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:get_end_time");
		if (thisDimr->control->subBlocks[0].type == CT_PARALLEL) {
			*tEnd = thisDimr->control->subBlocks[0].subBlocks[thisDimr->control->subBlocks[0].masterSubBlockId].tEnd;
		}
		else {
			// Start block
			*tEnd = thisDimr->control->subBlocks[0].tEnd;
		}
	}

	//------------------------------------------------------------------------------
	BMI_API void get_time_step(double * tStep) {
		thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:get_time_step");
		if (thisDimr->control->subBlocks[0].type == CT_PARALLEL) {
			*tStep = thisDimr->control->subBlocks[0].subBlocks[thisDimr->control->subBlocks[0].masterSubBlockId].tStep;
		}
		else {
			// Start block
			*tStep = thisDimr->control->subBlocks[0].tStep;
		}
	}

	//------------------------------------------------------------------------------
	BMI_API void get_current_time(double * tCur) {
		thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:get_current_time");
		if (thisDimr->control->subBlocks[0].type == CT_PARALLEL) {
			*tCur = thisDimr->control->subBlocks[0].subBlocks[thisDimr->control->subBlocks[0].masterSubBlockId].tCur;
		}
		else {
			// Start block
			*tCur = thisDimr->control->subBlocks[0].tCur;
		}
	}

	//------------------------------------------------------------------------------
	BMI_API void get_var(const char * key, void ** ref) {
		char           * componentName = new char[MAXSTRINGLEN];
		const char     * slash = strstr(key, "/");
		const char     * sourceName;
		dimr_component * compPtr = NULL;
		double         * sourceVarPtr = NULL; // In a coupler, when the source component is get/setting by ref, the pointer to
											   // the resulting parameter is stored in sourceVarPtr
											   // In this get_var, sourceVarPtr is always undefined.
											   // NULL flags that it has to be retrieved.
		int              sourceProcess = 0;   // With multiple possible source processes, sourceProcess flags what process is actualy delivering the value
											   // Only relevant for parallel calculations. Possibly not working yet?

		thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:get_var");
		// Assumption: "key" has the structure "componentName/group/id/parameter"
		if (slash == NULL) {
			// No component name specified in "key"
			*ref == NULL;
			delete[] componentName;
			return;
		}
		// componentName is everything before the first / in key
		strncpy(componentName, key, slash - key);
		componentName[slash - key] = '\0';
		// sourceName is everything behind the first / in key
		sourceName = slash + 1;
		if (strlen(sourceName) < 1)
		{
			throw Exception(true, Exception::ERR_INVALID_INPUT, "dimr::get_var: No parameter specified. Expecting \"componentName/parameterName\"\n");
		}
		// Search componentName in the list of components of thisDimr
		for (int i = 0; i < thisDimr->componentsList.numComponents; i++) {
			if (strcmp(thisDimr->componentsList.components[i].name, componentName) == 0) {
				compPtr = &thisDimr->componentsList.components[i];
				break;
			}
		}
		if (compPtr == NULL) {
			throw Exception(true, Exception::ERR_INVALID_INPUT, "dimr::get_var: Unrecognized component \"%s\". Expecting \"componentName/parameterName\"\n", componentName);
		}
		// Get the pointer to the variable being asked for and put it in argument "ref"
		double transfer = -999000.0;

		thisDimr->getAddress(sourceName, compPtr->type, compPtr->dllGetVar, &sourceVarPtr, compPtr->processes, compPtr->numProcesses, transfer);
		*ref = thisDimr->send(sourceName, compPtr->type, sourceVarPtr, compPtr->processes, compPtr->numProcesses, &transfer);

		delete[] componentName;
	}

	//------------------------------------------------------------------------------
	BMI_API void set_var(const char * key, const void * value) {
		char           * componentName = new char[MAXSTRINGLEN];
		const char     * slash = strstr(key, "/");
		const char     * targetName;
		dimr_component * compPtr = NULL;
		double         * targetVarPtr = NULL; // In a coupler, when the target component is get/setting by ref, the pointer to
											  // the resulting parameter is stored in sourceVarPtr
											  // In this set_var, targetVarPtr is always undefined.
											  // NULL flags that it has to be retrieved via dllSetVar.
		int              sourceProcess = 0;   // With multiple possible source processes, sourceProcess flags what process is actualy delivering the value
											  // Only relevant for parallel calculations. Possibly not working yet?

		// thisDimr->log is not initialized when set_var is called before initialize
		if (thisDimr == NULL) {
			thisDimr = Dimr::GetInstance();
		}
		thisDimr->log->Write(DEBUG, thisDimr->my_rank, "dimr_lib:set_var");
		// Catch special keywords for Dimr_dll itself
		if (strcmp(key, "useMPI") == 0) {
			thisDimr->use_mpi = *(bool *)value;
		}
		else if (strcmp(key, "numRanks") == 0) {
			thisDimr->numranks = *(int *)value;
		}
		else if (strcmp(key, "myRank") == 0) {
			thisDimr->my_rank = *(int *)value;
		}
		else if (strcmp(key, "debugLevel") == 0) {
			thisDimr->logLevel = *(Level *)value;
			thisDimr->log->SetLevel(thisDimr->logLevel);
		}
		else if (strcmp(key, "feedbackLevel") == 0) {
			thisDimr->feedbackLevel = *(Level *)value;
			thisDimr->log->SetFeedbackLevel(thisDimr->feedbackLevel);
		}
		else if (strcmp(key, "redirectFile") == 0) {
			// value is a char*
			// Special value: "stdout/stderr" => switch off redirection to file by setting to NULL
			// Else: value is the name of the file to redirect to
			if (strcmp((const char*)value, "stdout/stderr") == 0 && thisDimr->redirectFile != NULL) {
				free(thisDimr->redirectFile);
				thisDimr->redirectFile = NULL;
			}
			else {
				if (thisDimr->redirectFile != NULL) {
					free(thisDimr->redirectFile);
				}
				int len = strlen((const char*)value);
				thisDimr->redirectFile = (char *)malloc((len + 1) * sizeof(char));
				strncpy(thisDimr->redirectFile, (const char*)value, len);
				thisDimr->redirectFile[len] = '\0';
			}
		}
		else {
			// Assumption: "key" has the structure "componentName/group/id/parameter"
			if (slash == NULL) {
				// No component name specified in "key"
				throw Exception(true, Exception::ERR_INVALID_INPUT, "dimr::set_var: Unrecognized keyword \"%s\"\n", key);
			}
			// componentName is everything before the first / in key
			strncpy(componentName, key, slash - key);
			componentName[slash - key] = '\0';
			// targetName is everything behind the first / in key
			targetName = slash + 1;
			if (strlen(targetName) < 1) {
				throw Exception(true, Exception::ERR_INVALID_INPUT, "dimr::set_var: No parameter specified. Expecting \"componentName/parameterName\"\n");
			}
			// Search componentName in the list of components of thisDimr
			for (int i = 0; i < thisDimr->componentsList.numComponents; i++) {
				if (strcmp(thisDimr->componentsList.components[i].name, componentName) == 0) {
					compPtr = &thisDimr->componentsList.components[i];
					break;
				}
			}
			if (compPtr == NULL) {
				throw Exception(true, Exception::ERR_INVALID_INPUT, "dimr::set_var: Unrecognized component \"%s\". Expecting \"componentName/parameterName\"\n", componentName);
			}
			// Send value to the receiving component
			thisDimr->receive(targetName,
				compPtr->type,
				compPtr->dllSetVar,
				compPtr->dllGetVar,
				targetVarPtr,
				compPtr->processes,
				compPtr->numProcesses,
				-1,
				value);
		}
		delete[] componentName;
	}


} // extern "C"


