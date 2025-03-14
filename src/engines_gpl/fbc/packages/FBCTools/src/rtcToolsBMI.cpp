// Copyright (C) 2015 Deltares
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
 * @file rtcToolsBMI.cpp
 * @brief BMI bindings for RTC-Tools
 * @author Jorn Baayen
 * @version 0.1
 * @date 2015
 */

#include <iterator>
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <limits>
#include <string>
#include <boost/filesystem.hpp>
#include <boost/property_tree/json_parser.hpp>

// These are defines in enums by RTC-Tools
#undef ABSOLUTE
#undef RELATIVE

#include "rtcToolsRuntime.h"
#include "piDiagInterface.h"
#include "bmi.h"

#include "fbc_version.h"

#if defined _WIN32
// Undefine min and max to avoid conflict with std::numeric_limits::{min,max}.
// See https://social.msdn.microsoft.com/Forums/vstudio/en-US/e94865f7-84cd-4912-8339-6ee95fb58825/numericlimitsdoublemax-not-recognized?forum=vcgeneral
#undef min
#undef max
#endif

dllexp void get_attribute(const char * attribute_name, char * attribute_value)
{
    if (strcmp(attribute_name, "version") == 0)
    {
        strcpy(attribute_value, rtctools_version);
        int len = strlen(rtctools_version);
        attribute_value[len] = '\0';
        return;
    }
}


dllexp char * get_rtctools_version()
{
   return rtctools_version;
}

dllexp char * get_rtctools_version_id()
{
   return rtctools_version_id;
}

#define MAXSTRLEN 1024

static rtcToolsRuntime *runtime = NULL;
static double *cache = NULL;
static int step;
static double dt;

// BMI interface

BMI_API int BMI::initialize(const char *moduleDir)
{
	// Parse module configuration file
	// We do this 3Di-style, that is, with a "settings.json" in the given folder.
	boost::filesystem::path modulePath(moduleDir);

	boost::filesystem::path settingsPath(modulePath / "settings.json");
	std::ifstream f(settingsPath.string());

    boost::property_tree::ptree pt;
    boost::property_tree::read_json(f, pt);

    // Look up paths from settings
    std::string schemaDir = pt.get<std::string>("schemaDir");
    std::string    xmlDir = pt.get<std::string>("xmlDir"   );

    // Resolve paths relative to module folder
    boost::filesystem::path schemaPath(schemaDir);
    boost::filesystem::path xmlPath(xmlDir);

    schemaPath = boost::filesystem::absolute(schemaPath, modulePath);
    xmlPath    = boost::filesystem::absolute(xmlPath   , modulePath);

	// Create RTC-Tools runtime object
	runtime = new rtcToolsRuntime(schemaPath.string().c_str(), xmlPath.string().c_str());

	// Cleanup
	f.close();

	// Check mode
	switch (runtime->getRuntimeSettings()->modeInfo[0].mode) {
	case SIMULATE:
	case OPTIMIZE:
		// Supported modes
		break;

	default:
		// Unsupported mode
		std::stringstream ss;
		ss << "BMI initialize(...) - error: Unsupported mode " << runtime->getRuntimeSettings()->modeInfo[0].mode << "!";
		std::string s = ss.str();
	    piDiagInterface::addLine(1, s);
		throw runtime_error(s);
	}

	// Allocate value cache
	// Search for the highest index; just using size(IDMap) does not always work.
	auto IDMap = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getScalarIDMap();
	int cacheSize = 0;
	for (auto it = IDMap.begin(); it != IDMap.end(); it++)
	{
		if (it->second > cacheSize) cacheSize = it->second;
	}
	cacheSize++;
	cache = new double[cacheSize];

	// Step size in seconds
	dt = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getDT() / 1000.0;

	// Initial step
	step = 0;

   // state import is at step 0, set lastTimeState accordingly
   auto currentTime = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getTime(step) / 1000.0;// in seconds
   runtime->setLastTimeStateExported(currentTime);
	
   // Done
   return 0;
}

BMI_API int BMI::finalize()
{
	// Flush diagnostics
	piDiagInterface::write();

	// Release memory
	runtime->finish(step);
	delete runtime;

	delete[] cache;

	// Done
	return 0;
}

BMI_API int BMI::update(double dt)
{
	// The time increment is determined by RTC-Tools
	if (dt >= 0) {
		const string msg = "BMI update(dt) - error: dt >= 0 - Timestep is provided by RTC-Tools.  Set dt to -1.";
        dt = -1;
        piDiagInterface::addLine(1, msg);
        //throw runtime_error(msg);
	}

	// Write output
	// We perform this task here, rather than after execute().  In this way, all set values are logged, regardless of whether they
	// are set implicitly or explicitly:
	// Step n - 1:  Set values at n - 1, n    .  Log values at n - 1.  Compute values at n    .
	// Step n    :  Set values at n    , n + 1.  Log values at n    .  Compute values at n + 1.
	// Step n + 1:  Set values at n + 1, n + 2.  Log values at n + 1.  Compute values at n + 2.
	runtime->writeOutput(step, false);

	// Increment current time step
	step++;

	// Perform mode-dependent update action
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
  
	// Done
	return 0;
}

BMI_API void BMI::get_var_count(int *count)
{
	// Return count of all variables
	*count = (int) runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getScalarIDMap().size();
}

BMI_API void BMI::get_var_name(int n, char *name)
{
	// Retrieve name of n'th variable
	map<string, int>::const_iterator it = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getScalarIDMap().begin();

	std::advance(it, n);

	strncpy(name, it->first.c_str(), MAXSTRLEN);
}

BMI_API void BMI::get_var_type(const char *name, char *type)
{
	// All variables are doubles
	strcpy(type, "double");
}

BMI_API void BMI::get_var_rank(const char *name, int *rank)
{
	// All variables are scalar (0-rank)
	*rank = 0;
}

BMI_API void BMI::get_var_shape(const char *name, int *shape)
{
	// Scalar variables have no shape
}

BMI_API void BMI::get_start_time(double *start_time)
{
	switch (runtime->getRuntimeSettings()->modeInfo[0].mode) {
	case SIMULATE:
		// Return start time
		*start_time = (double) runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getStartTime();

		break;

	case OPTIMIZE:
		// Return -inf
		*start_time = std::numeric_limits<double>::min();

		break;

	default:
		// Do nothing
		break;

	}
}

BMI_API void BMI::get_end_time(double *end_time)
{
	switch (runtime->getRuntimeSettings()->modeInfo[0].mode) {
	case SIMULATE:
		// Return start time
		*end_time = (double) runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getEndTime();

		break;

	case OPTIMIZE:
		// Return +inf
		*end_time = std::numeric_limits<double>::max();

		break;

	default:
		// Do nothing
		break;
	
	}
}

BMI_API void BMI::get_current_time(double *current_time)
{
	*current_time = step * dt;
}

BMI_API void BMI::get_time_step(double *time_step)
{
	// Return time step size
	*time_step = dt;
}

BMI_API void BMI::get_var(const char *name, void **data_ptr)
{
	// Cache latest variable value
	int index = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getScalarIndex(name);

	cache[index] = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getValue(0, step, index);

	// Return pointer to cache
	*data_ptr = (char *) &(cache[index]);
}

BMI_API void BMI::set_var(const char *name, const void *data)
{
	// Define the time step at which to set the variable 
	int target_step = step;

	// Parse variable name
	size_t name_len = strlen(name);
	char *parsed_name = (char *) alloca(name_len + 1);
	if (name[name_len - 1] == '*') {
		// n               n+1
		// o----------------o   D-FLOW
		//              .
		//          .
		//      .
		//  .
		// o----------------o   RTC
		// 
		// In the above scenario, the flow values at step n+1 are passed to RTC at RTC timestep n. 
		// In this case, the variable name can be annotated with an asterisk (*), which causes it to be
		// stored in the slot for RTC timestep n + 1.  RTC should then be configured to relate this
		// variable implicitly to the output variable.
		strncpy(parsed_name, name, name_len - 1);
		parsed_name[name_len - 1] = '\0';

		target_step++;

	} else {
		strncpy(parsed_name, name, name_len);
		parsed_name[name_len] = '\0';
	}

	// Set variable value
	int index = runtime->getTimeSeriesModel()->getTimeSeriesTensor()->getScalarIndex(parsed_name);

	double value = *((double *) data);

	runtime->getTimeSeriesModel()->getTimeSeriesTensor()->setValue(0, target_step, index, value);
}