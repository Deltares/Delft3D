//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
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
// $Id: dimr_exe.h 5816 2016-02-10 08:43:38Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  dimr Main Program
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  29 jun 12
//------------------------------------------------------------------------------


#pragma once

// The following definition is needed since VisualStudio2015 before including <pthread.h>:
#define HAVE_STRUCT_TIMESPEC


#ifndef _WIN32
#   include "config.h"
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
#ifndef _WIN32
#   include <sys/wait.h>
#   include <unistd.h>
// #else
// #   include <sys/syscall.h>.
#endif

#include <cstddef>
#include <iostream>
#include <string>
#include <mpi.h>


class DimrExe;
class Clock;
class Exception;
class Log;


#include "clock.h"
#include "exception.h"
#include "log.h"
#include "stringutils.h"
#include "dimr.h"


//------------------------------------------------------------------------------


class DimrExe {
    public:
        DimrExe(void);
        void initialize(
            int     argc,
            char *  argv [],
            char *  envp []
            );
        ~DimrExe(void);
        void openLibrary(void);
        void freeLib (void);
        void lib_initialize(void);
        int lib_update(void);
        void lib_update_test(void);
        void lib_finalize(void);
        void timerFinish(void);

    public:
        bool       ready;          // true means constructor succeeded and DH ready to run
        char  *    exePath;        // name of running dimr executable (argv[0])
        char  *    exeName;        // short name of executable
        Clock *    clock;          // timing facility
        Log   *    log;            // logging facility
        char  *    mainArgs;       // reassembled command-line arguments (argv[1...])
        char  *    slaveArg;       // command-line argument for slave mode

    private:
        char  *    configfile;     // name of configuration file
        bool       done;           // set to true when it's time to stop
        char  *    library;        // Component library name, without extension/prefix
        Clock::Timestamp  timerStartStamp;
        Clock::Timestamp  timerSumStamp;

#ifndef _WIN32
    void          *    libHandle;         // (Linux) Handle to the loaded library for this component.
#else
    HINSTANCE          libHandle;         // (Windows) Handle to the loaded library for this component.
#endif
    BMI_INITIALIZE     dllInitialize;     // entry point in dll
    BMI_UPDATE         dllUpdate;         // entry point in dll
    BMI_FINALIZE       dllFinalize;       // entry point in dll
    BMI_GETSTARTTIME   dllGetStartTime;   // entry point in dll
    BMI_GETENDTIME     dllGetEndTime;     // entry point in dll
    BMI_GETTIMESTEP    dllGetTimeStep;    // entry point in dll
    BMI_GETCURRENTTIME dllGetCurrentTime; // entry point in dll
    BMI_GETVAR         dllGetVar;         // entry point in dll
    BMI_SETVAR         dllSetVar;         // entry point in dll
    Level              logLevel;
    private:
    };


//------------------------------------------------------------------------------


#ifdef DIMR_MAIN
    DimrExe * DHE;     // global pointer to single object instance

#else
    extern DimrExe * DHE;
#endif

// -- MPI settings -------------------------------------------------------------
static bool use_mpi; // Whether MPI-mode is active for this run.
static int my_rank;  // Rank# of current process
static int numranks; // Total nr of MPI processes for dimr main.

void initialize_parallel(int, char **);
void finalize_parallel(void);
void abort_parallel(void);

