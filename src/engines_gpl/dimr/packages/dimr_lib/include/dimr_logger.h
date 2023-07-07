#pragma once
#include "dimr_constants.h"
#include "netcdf_references.h"
// A logger logs values that are exhanged between two components
// Corresponds with a logger block in config.xml
typedef struct dimr_logger dimr_logger;
struct dimr_logger
{
	const char        * workingDir;
	const char        * outputFile;
	netcdf_references * netcdfReferences;

	// using std::string in entire dimr source code can simplify this function, 
	// but also others
	std::string GetLoggerFilename(const char * dimrWorkingDirectory, const char * dirSeparator)
	{
		char* loggerFileName = new char[MAXSTRING];
		strcpy(loggerFileName, dimrWorkingDirectory);
		strcat(loggerFileName, dirSeparator);
		strcat(loggerFileName, workingDir);
		strcat(loggerFileName, dirSeparator);
		strcat(loggerFileName, outputFile);
		std::string stringFileName(loggerFileName);
		delete[] loggerFileName;
		return stringFileName;
	}
};
