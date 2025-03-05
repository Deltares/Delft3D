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

#ifndef RTCTOOLS_DLL_H
#define RTCTOOLS_DLL_H

#ifdef _WIN32
#define dllexp extern "C" __declspec(dllexport)
#else
#define dllexp extern "C"
#endif

dllexp void LoadRtcToolsDll();
dllexp void* CreateOmiAccessableRtcToolsClass(char* schemaLocation, char* xmlFilesLocation);
dllexp void OmiInitialize(void *pObject);
dllexp void OmiPrepare(void *pObject);
dllexp void FinishOmiAccessableRtcToolsClass(void *pObject);
dllexp void DisposeOmiAccessableRtcToolsClass(void *pObject);
dllexp void OmiGetStartTimeString(void* pObject, char *retstring, int len);
dllexp void OmiGetEndTimeString(void* pObject, char *retstring, int len);
dllexp void OmiGetCurrentTimeString(void* pObject, char *retstring, int len);
dllexp int OmiGetInputExchangeItemCount(void *pObject);
dllexp int OmiGetOutputExchangeItemCount(void *pObject);
dllexp void OmiGetInputExchangeItem(void *pObject, int index, char *name, int len);
dllexp void OmiInputExchangeItemGetElementId(void *pObject, int index, char *retstring, int len);
dllexp void OmiInputExchangeItemGetQuantityId(void *pObject, int index, char *retstring, int len);
dllexp void OmiInputExchangeItemGetUnit(void *pObject, int index, char *retstring, int len);
dllexp void OmiGetOutputExchangeItem(void *pObject, int index, char *retstring, int len);
dllexp void OmiOutputExchangeItemGetElementId(void *pObject, int index, char *retstring, int len);
dllexp void OmiOutputExchangeItemGetQuantityId(void *pObject, int index, char *retstring, int len);
dllexp void OmiOutputExchangeItemGetUnit(void *pObject, int index, char *retstring, int len);
dllexp void OmiSetInputValue(void *pObject, int index, double value, int timeStepCount);
dllexp int OmiSetInputValues(void *pObject, int nValue, double *valueArray, int timeStepCount);
dllexp double OmiGetOutputValue(void *pObject, int index);
dllexp int OmiGetOutputValues(void *pObject, int nValue, double *valueArray);
dllexp int OmiPerformTimeStep(void *pObject, int timeStepCount);
dllexp void OmiWriteStateFile(void *pObject, char* filename, char* filepath);

#endif //RTCTOOLS_DLL_H
