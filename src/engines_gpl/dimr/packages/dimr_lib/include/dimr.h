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
// $Id: dimr.h 933 2011-10-25 10:01:26Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
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
#include "clock.h"
#include <ctime>
#ifndef _WIN32
#   include <sys/wait.h>
#   include <unistd.h>
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


class Dimr {
    public:
		static Dimr* GetInstance()
		{
			if (instance == NULL)
				instance = new Dimr();
			return instance;

		}
	
		void           scanConfigFile(void);
        void           connectLibs(void);

        void           printComponentVersionStrings (Level);

        void           freeLibs(void);
        void           processWaitFile(void);
        void           runControlBlock  (dimr_control_block *, double, int);
        void           runParallelInit  (dimr_control_block *);
        void           runParallelFinish(dimr_control_block *);
        void           timersInit(void);
        void           timerStart(dimr_component *);
        void           timerEnd(dimr_component *);
        void           timersFinish(void);
		void           timerFinish(void);
        void           receive(const char *, int, BMI_SETVAR, BMI_GETVAR, double *, int *, int, int, const void *);
        void           receive_ptr(const char *, const char *, int, BMI_SETVAR, BMI_GETVAR, BMI_GETVARSHAPE , double *, int *, int, int, double *);
        void           getAddress(const char * name, int compType, BMI_GETVAR dllGetVar, double ** sourceVarPtr, int * processes, int nProc, double& transfer);
        double *       send(const char * name, int compType, double* sourceVarPtr, int* processes, int nProc, double* transfer);
		
    public:
        bool                 ready;          // true means constructor succeeded and DH ready to run
        char *               exePath;        // name of running dimr executable (argv[0])
        char *               exeName;        // short name of executable
        Clock *              clock;          // timing facility
        Log *                log;            // logging facility
        XmlTree *            config;         // top of entire XML configuration tree
        char *               mainArgs;       // reassembled command-line arguments (argv[1...])
        char *               slaveArg;       // command-line argument for slave mode
        dimr_control_block * control;        // structure containing all information from the control block in the config.xml file
        dimr_components      componentsList; // Array of all components
        dimr_couplers        couplersList;   // Array of all couplers
        bool                 use_mpi;        // Whether MPI-mode is active for this run.
        int                  nc_mode;        // [3 or 4]   NetCDF creation mode: NetCDF3 (NF90_CLASSIC_MODEL) or NetCDF4 (NF90_NETCDF4)
        int                  my_rank;        // Rank# of current process
        int                  numranks;       // Total nr of MPI processes for dimr main.
        Level                logLevel;
        Level                feedbackLevel;
        const char *         configfile;     // name of configuration file
        bool                 done;           // set to true when it's time to stop
        char *               redirectFile;   // Name of file to redirect stdout/stderr to
                                             // Default: Off when started via dimr-exe, On otherwise
		
        char *               dimrWorkingDirectory; // File path where dimr configuration file is
        const char *         dirSeparator;
      // String constants; initialized below, outside class definition
    private:
		//static Dimr *m_pInstance;
		static Dimr*    instance;

		Dimr();
		~Dimr();
		Dimr(Dimr const&) = delete;         // Don't Implement.
		void operator=(Dimr const&) = delete; // Don't implement

	
		double         transferValue;

        // Additional destructor routine
        void           deleteControlBlock (dimr_control_block);

        // Additional run routines
        void           runStartBlock      (dimr_control_block *, double, int);
        void           runParallelUpdate  (dimr_control_block *, double);


        void           scanControl        (XmlTree *, dimr_control_block *);
        void           scanGlobalSettings (XmlTree *);
        void           scanUnits          (XmlTree *);
        void           scanComponent      (XmlTree *, dimr_component *);
        void           scanCoupler        (XmlTree *, dimr_coupler *);

        dimr_component * getComponent     (const char *);

        dimr_coupler *   getCoupler       (const char *);

        bool           IsCouplerItemTypePTR(int couplerItem);

        void           char_to_ints       (char *, int **, int *);

        map<string, int> ncfiles;
		static void		   _log				  (Level, const char*); /* BMILogger function */
		Clock::Timestamp  timerStartStamp;
		Clock::Timestamp  timerSumStamp;
    };
