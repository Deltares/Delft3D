//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2026.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dimr.h 933 2011-10-25 10:01:26Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  29 jun 12
//------------------------------------------------------------------------------
/*
 * @include{doc} dimr-mainpage.dox
 */

#pragma once

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC

#ifndef _WIN32
    #include "config.h"
#endif

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "clock.h"
#include <ctime>
#ifndef _WIN32
    #include <sys/wait.h>
    #include <unistd.h>
// #else
// #   include <sys/syscall.h>.
#endif

#include <cstddef>
#include <iostream>
#include <fstream>
#include <string>
#include <mpi.h>
#include <map>
#include "dimr_control_block.h"
#include "dimr_components.h"
#include "dimr_coupler.h"
#include "dimr_couplers.h"

class Dimr;
class Clock;
class Exception;
class Log;

#include "clock.h"
#include "component.h"
#include "exception.h"
#include "log.h"
#include "stringutils.h"
#include "xmltree.h"
#include "bmi.h"

//------------------------------------------------------------------------------

class Dimr
{
public:
    static Dimr* GetInstance()
    {
        if (instance == NULL) instance = new Dimr();
        return instance;
    }

    void scanConfigFile(void);
    void connectLibs(void);

    void printComponentVersionStrings(Level my_level);

    void freeLibs(void);
    void barrier(const MPI_Comm comm, const bool use_mpi, const int mpi_barrier_sleep);
    void processWaitFile(void);
    void createDistributeMPISubGroupCommunicator(dimr_component* component);
    void runControlBlock(dimr_control_block* cb, double tStep, int phase);
    void runParallelInit(dimr_control_block* cb);
    void runParallelFinish(dimr_control_block* cb);
    void timersInit(void);
    void timerStart(dimr_component* component);
    void timerEnd(dimr_component* component);
    void timersFinish(void);
    void timerFinish(void);
    void receive(const char* name, int compType, BMI_SETVAR dllSetVar, BMI_GETVAR dllGetVar, double* targetVarPtr,
                 int* processes, int nProc, int targetProcess, const void* transferValuePtr);
    void receive_ptr(const char* name, const char* sourceName, int compType, BMI_SETVAR dllSetVar, BMI_GETVAR dllGetVar,
                     BMI_GETVARSHAPE dllGetVarShape, double* targetVarPtr, int* processes, int nProc, int targetProcess,
                     double* sourceVarPtr);
    void getAddress(const char* name, int compType, BMI_GETVAR dllGetVar, double** sourceVarPtr, int* processes,
                    int nProc, double& transfer);
    double* send(const char* name, int compType, double* sourceVarPtr, int* processes, int nProc, double* transfer);

public:
    bool ready;                  // true means constructor succeeded and DH ready to run
    char* exePath;               // name of running dimr executable (argv[0])
    char* exeName;               // short name of executable
    Clock* clock;                // timing facility
    Log* log;                    // logging facility
    XmlTree* config;             // top of entire XML configuration tree
    char* mainArgs;              // reassembled command-line arguments (argv[1...])
    char* slaveArg;              // command-line argument for slave mode
    dimr_control_block* control; // structure containing all information from the control block in the config.xml file
    dimr_components componentsList; // Array of all components
    dimr_couplers couplersList;     // Array of all couplers
    bool use_mpi;                   // Whether MPI-mode is active for this run
    MPI_Group mpiGroupWorld;        // Overall MPI-group
    int nc_mode;  // [3 or 4]   NetCDF creation mode: NetCDF3 (NF90_CLASSIC_MODEL) or NetCDF4 (NF90_NETCDF4)
    int my_rank;  // Rank# of current process
    int numranks; // Total nr of MPI processes for dimr main
    Level logLevel;
    Level feedbackLevel;
    const char* configfile; // name of configuration file
    bool done;              // set to true when it's time to stop
    char* redirectFile;     // Name of file to redirect stdout/stderr to
                            // Default: Off when started via dimr-exe, On otherwise

    char* dimrWorkingDirectory; // File path where dimr configuration file is
    const char* dirSeparator;
    // String constants; initialized below, outside class definition
private:
    // static Dimr *m_pInstance;
    static Dimr* instance;

    Dimr();
    ~Dimr();
    Dimr(Dimr const&) = delete;           // Don't Implement.
    void operator=(Dimr const&) = delete; // Don't implement

    double transferValue;

    // Additional destructor routine
    void deleteControlBlock(dimr_control_block cb);

    // Additional run routines
    void runStartBlock(dimr_control_block* cb, double tStep, int phase);
    void runParallelUpdate(dimr_control_block* cb, double tStep);

    void scanControl(XmlTree* controlBlockXml, dimr_control_block* controlBlock);
    void scanGlobalSettings(XmlTree* rootXml);
    void scanUnits(XmlTree* rootXml);
    void scanComponent(XmlTree* xmlComponent, dimr_component* newComp);
    void scanCoupler(XmlTree* xmlCoupler, dimr_coupler* newCoup);

    dimr_component* getComponent(const char* compName);

    dimr_coupler* getCoupler(const char* couplerName);

    bool IsCouplerItemTypePTR(int couplerItem);

    void char_to_ints(const char* line, int** iarr, int* count);

    std::map<std::string, int> ncfiles;
    static void _log(Level level, const char* msg); /* BMILogger function */
    Clock::Timestamp timerStartStamp;
    Clock::Timestamp timerSumStamp;
};
