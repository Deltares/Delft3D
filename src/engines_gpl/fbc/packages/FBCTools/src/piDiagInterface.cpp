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

#include "piDiagInterface.h"
#include "dataBinding/pi_diag.hxx"
#include "utilities/utils.h"

#include <fstream>

using namespace fews;
using namespace PI;
using namespace rtctools;
using namespace utilities;
using namespace xml_schema;

static int logLevel = 3;
static bool eventCodeOutput = false;
static bool flushActive = false;
static bool diagWriterInitialized = false;
static bool includeError = false;
static boost::filesystem::path diagWorkDir;

struct logMessage {
	logMessage(int level, string message, string eventCode) 
		: level(level), message(message), eventCode(eventCode) {};
	int level;
	string message;
	string eventCode;
};

vector<logMessage> logging;

void piDiagInterface::initializeRtcDiagWriter(boost::filesystem::path workDir, bool codeOutput)
{
	#pragma omp critical (initializeWriter)
	{
	    if (!diagWriterInitialized)
	    {
    		diagWorkDir = workDir;
		    eventCodeOutput = codeOutput;

		    logging = vector<logMessage>();

		    diagWriterInitialized = true;
		    includeError = false;
	    }
	}
}

int piDiagInterface::getLogLevel()
{
	return logLevel;
}

bool piDiagInterface::getIncludeError()
{
	return includeError;
}

void piDiagInterface::setLogLevel(int myLogLevel)
{
	#pragma omp critical
	{
		logLevel = myLogLevel;
	}
}

void piDiagInterface::setEventCode(bool myEventCode)
{
	#pragma omp critical
	{
		eventCodeOutput = myEventCode;
	}
}

void piDiagInterface::setFlush(bool flag) 
{
	#pragma omp critical
	{
		flushActive = flag;
	}
}

void piDiagInterface::addLine(int level, string message, string eventCode)
{
	#pragma omp critical
	{
		if (!diagWriterInitialized) {
			piDiagInterface::initializeRtcDiagWriter();
		}
		if (level<2) includeError = true;

		logging.push_back(logMessage(level, message, eventCode));
	}
    if (flushActive) piDiagInterface::write();
}

void piDiagInterface::write(void)
{
	#pragma omp critical
	{
		// initialize the data binding class
		DiagComplexType* diag = new DiagComplexType();;

		// and set the xsd references
		xml_schema::NamespaceInfomap myMap;
		myMap[""].name = "http://www.wldelft.nl/fews/PI";

		// add lines
		for (int i=0; i<(int)logging.size(); i++) {
			if (logging[i].level<=logLevel) {
				LineComplexType newLine(logging[i].level, logging[i].message);
				if (eventCodeOutput && logging[i].eventCode.size()>0) {
				    newLine.setEventCode(logging[i].eventCode);
			    }
				diag->getLine().push_back(newLine);
			}
		}

		// serialize the modified object model to XML file
		string filename = string(utils::getAbsoluteFilename(diagWorkDir, "diag.xml"));
		ofstream ofs(filename.c_str());
		serializeDiag(ofs, *diag, myMap, "UTF-8");
		ofs.close();
		delete diag;
	}
}

void piDiagInterface::freeRtcDiagWriter(void)
{
	#pragma omp critical
	{
		logging.clear();
		diagWriterInitialized = false;
		includeError = false;
	}
}
