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
 * @author Bernhard Becker, Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */


#include "rtcToolsOpenMI.h"
#include "rtcToolsRuntime.h"
#include "piDiagInterface.h"
#include "version.h"

char* convert2CStr(std::string str);

// ------- DLL exports -------------------------------------------------

dllexp char * get_rtctools_version()
{
   return rtctools_version;
}

dllexp char * get_rtctools_version_id()
{
   return rtctools_version_id;
}

dllexp void LoadRtcToolsDll()
{
	// No action needed. This method only exists to offer external
	// (c#) code the possibility to force loading the DLL by perform
	// the call to this LoadRtcToolsDll().
}


dllexp void* CreateOmiAccessableRtcToolsClass(char schemaDir[], char xmlFilesDir[])
{
	// rtc tools object
	return (void*)new rtcToolsRuntime(schemaDir, xmlFilesDir);
}

dllexp void OmiInitialize(void *pObject)
{
	// everything should be done already
}

dllexp void OmiPrepare(void *pObject)
{
	// everything should be done already
}

dllexp void FinishOmiAccessableRtcToolsClass(void *pObject)
{
	if (pObject != NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		((rtcToolsRuntime*)pObject)->finishFromOpenMI(openMIInt->getCurrentTimeStep());
	}
}

dllexp void DisposeOmiAccessableRtcToolsClass(void *pObject)
{
	if (pObject != NULL) {
		delete ((rtcToolsRuntime*)pObject);
		pObject = NULL;
	}

	//piDiagInterface::write();
}

dllexp void OmiGetStartTimeString(void* pObject, char *retstring, int len)
{
	long long datetime = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getTimeSeriesTensor()->getStartTime();
    
    char datetimestring[30];
    utils::time2datetimestring(datetime, datetimestring);
    utils::convert2CStr(datetimestring, retstring, len);
    return;
}

dllexp void OmiGetEndTimeString(void* pObject, char *retstring, int len)
{
	long long datetime = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getTimeSeriesTensor()->getEndTime();
    
    char datetimestring[30];
    utils::time2datetimestring(datetime, datetimestring);
    utils::convert2CStr(datetimestring, retstring, len);
    return;
}

dllexp void OmiGetCurrentTimeString(void* pObject, char *retstring, int len)
{
	long long datetime = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getCurrentTime();
    
    char datetimestring[30];
    utils::time2datetimestring(datetime, datetimestring);
    utils::convert2CStr(datetimestring, retstring, len);
    return;
}

dllexp int OmiGetInputExchangeItemCount(void *pObject)
{
	int count = 0;

	if (pObject != NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
	    count = openMIInt->getInputExchangeItemCount();
	}

	return count;
}

dllexp int OmiGetOutputExchangeItemCount(void *pObject)
{
	int count = 0;

	if (pObject != NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		count = openMIInt->getOutputExchangeItemCount();
	}

	return count;
}

dllexp void OmiGetInputExchangeItem(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getInputExchangeItem(index)->getID();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiInputExchangeItemGetElementId(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getInputExchangeItem(index)->getElementID();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiInputExchangeItemGetQuantityId(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getInputExchangeItem(index)->getQuantityID();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiInputExchangeItemGetUnit(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getInputExchangeItem(index)->getUnit();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiGetOutputExchangeItem(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getOutputExchangeItem(index)->getID();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiOutputExchangeItemGetElementId(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getOutputExchangeItem(index)->getElementID();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiOutputExchangeItemGetQuantityId(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getOutputExchangeItem(index)->getQuantityID();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiOutputExchangeItemGetUnit(void *pObject, int index, char *retstring, int len)
{
	string id = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface()->getOutputExchangeItem(index)->getUnit();
	utils::convert2CStr(id, retstring, len);
	return;
}

dllexp void OmiSetInputValue(void *pObject, int index, double value, int timeStepCount = 1)
{
	if (pObject!=NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		int internalIndex = openMIInt->getInputExchangeItem(index)->getIndex();
        if (timeStepCount != 1)
        {
		    openMIInt->setValue(internalIndex, value, timeStepCount);
        }
        else
        {
            openMIInt->setValue(internalIndex, value);
        }
	}
}

dllexp int OmiSetInputValues(void *pObject, int nValue, double *valueArray, int timeStepCount = 1)
{
	if (pObject!=NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		int count = openMIInt->getInputExchangeItemCount();
		for (int i=0; i<count; i++) {
			int internalIndex = openMIInt->getInputExchangeItem(i)->getIndex();
            if (timeStepCount != 1)
            {
			    openMIInt->setValue(internalIndex, valueArray[i], timeStepCount);
    		}
            else
            {
                openMIInt->setValue(internalIndex, valueArray[i]);
            }
        }
  	    return 0;
	}

	return 1;
}

dllexp double OmiGetOutputValue(void *pObject, int index)
{
	double value;

	if (pObject!=NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		int internalIndex = openMIInt->getOutputExchangeItem(index)->getIndex();
		value = openMIInt->getValue(internalIndex);
	}

	return value;
}

dllexp int OmiGetOutputValues(void *pObject, int nValue, double *valueArray)
{
	if (pObject!=NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		int count = openMIInt->getOutputExchangeItemCount();
		for (int i=0; i<count; i++) {
			int internalIndex = openMIInt->getOutputExchangeItem(i)->getIndex();
			valueArray[i] = openMIInt->getValue(internalIndex);
		}
		return 0;
	}

	return 1;
}


dllexp int OmiPerformTimeStep(void *pObject, int timeStepCount)
{
	if (pObject!=NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
		for (int i = 0; i < timeStepCount; i++)
		{
			int iStep = openMIInt->performTimeStep();
			((rtcToolsRuntime*)pObject)->executeFromOpenMI(iStep);
		}
		return 0;
	}

	return 1;
}

dllexp void OmiWriteStateFile(void *pObject, char filepath[], char filename[])
{
	if (pObject!=NULL) {
		openMIInterface *openMIInt = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel()->getOpenMIInterface();
        timeSeriesModel *timeSeriesModel = ((rtcToolsRuntime*)pObject)->getTimeSeriesModel();
		string resolvedFilename = string(filepath).empty() ? string(filename) : string(filepath).append(string("/").append(filename));
        timeSeriesModel->writeState(resolvedFilename, openMIInt->getCurrentTimeStep());
	}
}