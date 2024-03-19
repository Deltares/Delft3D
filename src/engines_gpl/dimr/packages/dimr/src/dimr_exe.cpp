//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id: dimr_exe.cpp 5816 2016-02-10 08:43:38Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  dimr Main Program
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#define DIMR_MAIN
//#define MEMCHECK

#include "dimr_exe.h"
#include "dimr.h"
#include "dimr_exe_version.h"

#ifndef _WIN32
#include "config.h"
#include <dlfcn.h>
#include <libgen.h>
#endif
//#include <expat.h>
#include <limits.h>


#if defined (MEMCHECK)
#include <mcheck.h>
#endif


#include <typeinfo>
using namespace std;

#include <string>
#include <sstream>
#include <math.h>
#include <mpi.h>

#if defined (WIN32)
#  include "getopt.h"
#  include <Strsafe.h>
#  include <windows.h>
#  include <direct.h>
#  define strdup _strdup
#  define chdir _chdir
#  define getcwd _getcwd
#else
#  include <unistd.h>
#endif


//------------------------------------------------------------------------------
static void printAbout (char * exeName);
static void printUsage (char * exeName);

#if defined (MEMCHECK)
static void
memAbort (
    enum mcheck_status status
    ) {

    printf ("ABORT: Malloc check error: %s\n",
                (status == MCHECK_DISABLED) ? "Consistency checking is not turned on" :
                (status == MCHECK_OK)       ? "Block is fine" :
                (status == MCHECK_FREE)     ? "Block freed twice" :
                (status == MCHECK_HEAD)     ? "Memory before the block was clobbered" :
                (status == MCHECK_TAIL)     ? "Memory after the block was clobbered" :
                "<undefined reason>");
    exit (1);
    }
#endif


//------------------------------------------------------------------------------
//  MAIN PROGRAM
int main (int     argc,
          char *  argv [],
          char *  envp []) {	
#if defined (MEMCHECK)
    int rc = mcheck_pedantic (& memAbort);
    printf ("DEBUG: mcheck_pedantic returns %d\n", rc);
#endif
    // Do not delete the next line; it ensures that the version string is added to the binary
	char * dimrexeversion = getversionidstring_dimr_exe();

    int ireturn = -1;
    DimrExe * DHE;
    bool doFinalize = false;

    initialize_parallel(argc, argv);

    try {
        DHE = new DimrExe();
        DHE->initialize(argc, argv, envp);
        if (! DHE->ready) return 1;

        DHE->log->Write(INFO, my_rank, getfullversionstring_dimr_exe());

        DHE->openLibrary();
        DHE->lib_initialize();

        doFinalize = true;

        int state = DHE->lib_update();
        if (state != 0)
        {
            throw Exception(true, (Exception::ErrorCode)state, "dimr_exe lib_update failed");
        }
        
        doFinalize = false;

        DHE->lib_finalize();
		DHE->timerFinish();
        delete DHE;
        ireturn = 0;
    }
    catch (exception& ex) {
        printf ("#### ERROR: dimr ABORT: C++ Exception: %s\n", ex.what());
        if (doFinalize) {
            printf("#### ERROR: dimr ABORT: Trying to finalize...\n");
            try {
                DHE->lib_finalize();
            }
            catch (...) {
                printf("#### ERROR: dimr ABORT: Finalize aborted\n");
            }
        }
        printf ("#### ERROR: dimr ABORT: See messages above\n");
        fflush(stdout);
        abort_parallel();
        ireturn = 1;
    }
    catch (Exception & ex) {
        printf ("#### ERROR: dimr ABORT: %s\n", ex.message);
        if (doFinalize) {
            printf("#### ERROR: dimr ABORT: Trying to finalize...\n");
            try {
                DHE->lib_finalize();
            }
            catch (...) {
                printf("#### ERROR: dimr ABORT: Finalize aborted\n");
            }
        }
        printf ("#### ERROR: dimr ABORT: See messages above\n");
        fflush(stdout);
        abort_parallel();
        ireturn = 1;
    }
    catch (char * str) {
        printf ("#### ERROR: dimr ABORT: %s\n", str);
        if (doFinalize) {
            printf("#### ERROR: dimr ABORT: Trying to finalize...\n");
            try {
                DHE->lib_finalize();
            }
            catch (...) {
                printf("#### ERROR: dimr ABORT: Finalize aborted\n");
            }
        }
        printf ("#### ERROR: dimr ABORT: See messages above\n");
        fflush(stdout);
        abort_parallel();
        ireturn = 1;
    }

    finalize_parallel();

    return ireturn;
}

//------------------------------------------------------------------------------
void DimrExe::lib_initialize(void)
{
    double tStart;
    double tEnd;
    double tStep;
    double tCurrent;

    // Disable redirecting stdout/stderr to file (default switched on)
    this->log->Write(DEBUG, my_rank, "%s.SetVar(redirectFile,stdout/stderr)", this->library, use_mpi);
    (this->dllSetVar) ("redirectFile", "stdout/stderr");
    this->log->Write(DEBUG, my_rank, "%s.SetVar(useMPI,%d)", this->library, use_mpi);
    (this->dllSetVar) ("useMPI", &use_mpi);
    this->log->Write (DEBUG, my_rank, "%s.SetVar(numRanks,%d)", this->library, numranks);
    (this->dllSetVar) ("numRanks", &numranks);
    this->log->Write (DEBUG, my_rank, "%s.SetVar(myRank,%d)", this->library, my_rank);
    (this->dllSetVar) ("myRank", &my_rank);
    this->log->Write (DEBUG, my_rank, "%s.SetVar(debugLevel,%d)", this->library, this->logLevel);
    (this->dllSetVar) ("debugLevel", &(this->logLevel));
    (this->dllSetVar) ("feedbackLevel", &(this->logLevel));
    this->log->Write (DEBUG, my_rank, "%s.Initialize(%s)", this->library, this->configfile);
    int result = (this->dllInitialize) (this->configfile);
    if (result != 0) 
    {
        // Error occurred, but apparently no exception has been thrown.
        // Throw one now
        this->log->Write(FATAL, my_rank, "%s.Initialize(%s) returned error value %d", this->library, this->configfile, result);
        // Re-throw the exception (so it will be handled in main)
        throw Exception(true, (Exception::ErrorCode)result, "%s.Initialize(%s) returned error value %d", this->library, this->configfile, result);
    }
    // Some basic tests
    (this->dllGetStartTime) (&tStart);
    (this->dllGetEndTime) (&tEnd);
    (this->dllGetTimeStep) (&tStep);
    (this->dllGetCurrentTime) (&tCurrent);
    this->log->Write(DEBUG, my_rank, "DimrExe::lib_initialize:tStart  : %20.10f", tStart);
    this->log->Write(DEBUG, my_rank, "DimrExe::lib_initialize:tCurrent: %20.10f", tCurrent);
    this->log->Write(DEBUG, my_rank, "DimrExe::lib_initialize:tStep   : %20.10f", tStep);
    this->log->Write(DEBUG, my_rank, "DimrExe::lib_initialize:tEnd    : %20.10f", tEnd);
}

//------------------------------------------------------------------------------
int DimrExe::lib_update(void)
{
    double tStart;
    double tEnd;
    double tStep;

    (this->dllGetStartTime) (&tStart);
    (this->dllGetEndTime) (&tEnd);
    tStep = tEnd - tStart;
    int state = (this->dllUpdate) (tStep);
    return state;
}

//------------------------------------------------------------------------------
// Test get_var/set_var subroutines in dimr.dll
// Specially written for testcase e105_dflowfm-drtc\f01_general\c010_weir_timeseries
// - Run the testcase in the normal way. Check that the waterlevel downstream changes in time (history plot)
// - In dimr.xml, remove the following two lines in the control block:
//   <coupler name="flow2rtc"/>
//   <coupler name="rtc2flow"/>
// - In DimrExe::main: call "DHE->lib_update_test()" instead of "DHE->lib_update()"
//   Now the waterlevel downstream still changes, but there are differences compared with the original run:
//   The order of executing the coupler steps and the update steps is changed.
void DimrExe::lib_update_test(void)
{
    double tStart;
    double tEnd;
    double tStep;
    double tCur;
    double * transfer = NULL;
    double value;

    (this->dllGetStartTime) (&tStart);
    (this->dllGetEndTime) (&tEnd);
    (this->dllGetTimeStep) (&tStep);
    (this->dllGetCurrentTime) (&tCur);
    do {
        (this->dllUpdate) (tStep);
        (this->dllGetCurrentTime) (&tCur);

        (this->dllGetVar) ("myNameDFlowFM/observations/Upstream/water_level", (void**)(&transfer));
        this->log->Write (INFO, my_rank, "DimrExe::lib_update_test:waterlevel: %20.10f", *transfer);
        // IMPORTANT: the value that "transfer" points to must be copied into another memory location ("value")
        // Inside dllSetVar, Dimr::send might be called to obtain the pointer to the variable inside the kernel that has to be set
        // This will reset the value that "transfer" points to into the current value.
        value = *transfer;
        (this->dllSetVar) ("myNameRTC/input_ObservationPoint01_water_level", (void*)(&value));

        (this->dllGetVar) ("myNameRTC/output_weir_crest_level", (void**)(&transfer));
        this->log->Write (INFO, my_rank, "DimrExe::lib_update_test:crestlevel: %20.10f", *transfer);
        // IMPORTANT: the value that "transfer" points to must be copied into another memory location ("value")
        // Inside dllSetVar, Dimr::send might be called to obtain the pointer to the variable inside the kernel that has to be set
        // This will reset the value that "transfer" points to into the current value.
        value = *transfer;
        (this->dllSetVar) ("myNameDFlowFM/weirs/weir01/crest_level", (void*)(&value));
    } while (tCur < tEnd);
}

//------------------------------------------------------------------------------
void DimrExe::lib_finalize(void)
{
   this->log->Write (DEBUG, my_rank, "    %s.Finalize()", this->library);
   (this->dllFinalize) ();
}
//------------------------------------------------------------------------------
void DimrExe::timerFinish(void)
{
   Clock::Timestamp curtime = clock->Epoch();
   this->timerSumStamp = curtime - this->timerStartStamp + this->timerSumStamp;
   this->timerStartStamp = 0;
   log->Write(INFO, my_rank, "%s\t: %d.%d sec", "DIMR_EXE",
	   this->timerSumStamp / 1000000,
	   this->timerSumStamp % 1000000);
   this->timerSumStamp = 0;
}

//------------------------------------------------------------------------------
void initialize_parallel(int argc, char *  argv [])
{
    int ierr;
    if (    getenv("PMI_RANK")             == NULL &&    // MPICH2
            getenv("OMPI_COMM_WORLD_RANK") == NULL &&    // OpenMPI 1.3
            getenv("OMPI_MCA_ns_nds_vpid") == NULL &&    // OpenMPI 1.2
            getenv("MPIRUN_RANK")          == NULL &&    // MVAPICH 1.1
            getenv("MV2_COMM_WORLD_RANK")  == NULL &&    // MVAPICH 1.9
            getenv("MP_CHILD")             == NULL ) {   // POE (IBM)
        use_mpi = false;
    } else {
        use_mpi = true;
    }

    if (use_mpi) {
        ierr = MPI_Init(&argc, &argv);
        if (ierr != 0) {
            throw Exception (true, Exception::ERR_MPI, "MPI_Init returns error code \"%d\"", ierr);
        }
        ierr = MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
        if (ierr != 0) {
            throw Exception (true, Exception::ERR_MPI, "MPI_Comm_rank returns error code \"%d\"", ierr);
        }
        ierr = MPI_Comm_size(MPI_COMM_WORLD, &numranks);
        if (ierr != 0) {
            throw Exception (true, Exception::ERR_MPI, "MPI_Comm_size returns error code \"%d\"", ierr);
        }
        printf("#%d: Running parallel with %d partitions\n", my_rank, numranks);
        fflush(stdout);
    } else {
        numranks = 1;
        my_rank  = 0;
    }
}

//------------------------------------------------------------------------------
void finalize_parallel()
{
    int ierr;

    if (use_mpi) {
        // NOTE: this barrier works, but only if all processes take the same route.
        // What didn't work: miprun -np 4, but only 3 FM master processes.
        // The fourth process (#3) then directly falls through to this finalize
        // barrier, which succeeds once in the first ParallelUpdate timestep
        // the other three processes have a barrier for the next timestep.
        // This problem is now avoided by NOT supporting less master processes than np.
        // (Exception thrown)
        ierr = MPI_Barrier(MPI_COMM_WORLD);
        ierr = MPI_Finalize();
    }
}

//------------------------------------------------------------------------------
void abort_parallel()
{
    int ierr;

    if (use_mpi) {
        ierr = MPI_Abort(MPI_COMM_WORLD, 1);
    }
}

//------------------------------------------------------------------------------
//  Constructor. Just create the object. Delay initialization actions in
//  initialization method
DimrExe::DimrExe (void) {
    this->ready = false;
}

//------------------------------------------------------------------------------
//  Constructor.  Initialize the object, read the configuration file and load
//  and invoke the start component's entry function.  All of the work is done
//  by the start component, including loading other components if necessary.
//  ToDo: make load-and-start a generic function available to any component.
void DimrExe::initialize (int     argc,
                              char *  argv [],
                              char *  envp []) {
    this->exePath = strdup (argv[0]);

#ifndef _WIN32
    this->exeName = strdup (basename (argv[0]));
#else
    char * ext = new char[5];
    this->exeName = new char[MAXSTRING];
    _splitpath (argv[0], NULL, NULL, this->exeName, ext);
    StringCbCatA (this->exeName, MAXSTRING, ext);
    delete [] ext;
#endif

    this->slaveArg  = NULL;
    this->done      = false;

    this->logLevel = INFO;  // selector of debugging/trace information
                                        // minLog: FATAL  maxLog: ALL
    FILE *      logFile = stdout;       // log file descriptor

    // Reassemble command-line arguments for later use (to spawn remote slave processes)

    int length = 0;
    for (int i = 1 ; i < argc ; i++)
        length += strlen (argv[i]) + 1;

    this->mainArgs = new char [length+1];
    memset (this->mainArgs, '\0', length+1);

    char * cp = this->mainArgs;
    for (int i = 1 ; i < argc ; i++) {
        int len = strlen (argv[i]);
        memcpy (cp, argv[i], len);
        cp += len;
        *cp++ = ' ';
    }


    //
    //
    // Process command-line arguments
    int c;
    while ((c = getopt (argc, argv, (char *) "d:l:S:v:i:?")) != -1) {
        switch (c) {
            case 'd': {
                if (sscanf (optarg, "%i", &logLevel) != 1)
                    throw Exception (true, Exception::ERR_INVALID_INPUT, "Invalid log level (-d option)");
                else
                    logLevel = min(max(logLevel,ALL),FATAL);
                break;
            }

            case 'v': {
                logLevel=ALL;
                break;
            }

            case 'l': {
                logFile = fopen (optarg, "w");
                if (logFile == NULL)
                    throw Exception (true, Exception::ERR_OS, "Cannot create log file \"%s\"", optarg);

                break;
            }

            case 'S': {
                this->slaveArg = optarg;
                break;
            }

            case 'i': {
                printAbout (this->exeName);
                this->done = true;
                return;
            }

            case '?': {
                printUsage (this->exeName);
                this->done = true;
                return;
            }

            default: {
                throw Exception (true, Exception::ERR_INVALID_INPUT, "Invalid command-line argument.  Execute \"%s -?\" for command-line syntax", this->exeName);
            }
        }
    }

    if (argc - optind != 1) {
        throw Exception (true, Exception::ERR_INVALID_INPUT,  "Improper usage.  Execute \"%s -?\" for command-line syntax", this->exeName);
    }

    this->clock = new Clock ();
    this->log = new Log (logFile, this->clock, this->logLevel);
    this->configfile = argv[optind];

    this->ready = true;
	this->timerStartStamp = clock->Epoch();
	this->timerSumStamp = 0;
}



//
//------------------------------------------------------------------------------
DimrExe::~DimrExe (void) {

    if (this->done)
        return;

    // to do:  (void) FreeLibrary(handle);
    freeLib();

    this->log->Write (DEBUG, my_rank, "dimr shutting down normally");

#ifndef _WIN32
    free (this->exeName);
#else
    delete [] this->exeName;
#endif

    delete this->clock;
    free (this->exePath);
    delete this->log;
    delete [] this->mainArgs;
    delete [] this->library;
    this->done = true;
}




//------------------------------------------------------------------------------
void DimrExe::openLibrary (void) {

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw Exception (true, Exception::ERR_OS, "ABORT: %s has not be ported to Apple Mac OS/X yet", this->exeName);
#endif
#ifndef _WIN32
    char *err;
#endif


#ifndef _WIN32
        this->library = new char[14];
        sprintf(this->library, "libdimr.so\0");
#else
        this->library = new char[16];
        sprintf(this->library, "dimr.dll\0");
#endif

        this->log->Write (DEBUG, my_rank, "Loading dimr library \"%s\"", this->library);

#ifndef _WIN32
        dlerror(); /* clear error code */
        void * dllhandle = dlopen (this->library, RTLD_LAZY);
        this->libHandle = dllhandle;
        #define GETPROCADDRESS dlsym
        #define GetLastError dlerror
        #define Sleep(msec) sleep((int)msec/1000)
#else
        SetLastError(0); /* clear error code */
        HINSTANCE dllhandle = LoadLibrary (LPCSTR(this->library));
        this->libHandle = dllhandle;
        #define GETPROCADDRESS GetProcAddress
#endif

        if (dllhandle == NULL) {

#ifndef _WIN32
            if ((err = dlerror()) != NULL)
                throw Exception (true, Exception::ERR_OS, "Cannot load component library \"%s\". Error: %s\n", this->library, err);
#else
            if (GetLastError() == 193)
                throw Exception (true, Exception::ERR_OS, "Cannot load component library \"%s\". Return code: %d\n    Most probably a 32bit - 64bit conflict.", this->library, GetLastError());
            else
                throw Exception (true, Exception::ERR_OS, "Cannot load component library \"%s\". Return code: %d", this->library, GetLastError());
#endif
        }

        // Get entry point for set_logger
        BMI_DIMR_SET_LOGGER set_logger_entry = (BMI_DIMR_SET_LOGGER) GETPROCADDRESS(dllhandle, BmiDimrSetLogger);
        if (set_logger_entry == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiDimrSetLogger, this->library, GetLastError());
        }
        (set_logger_entry)(this->log);

        // Collect BMI entry points
        this->dllInitialize = (BMI_INITIALIZE) GETPROCADDRESS (dllhandle, BmiInitializeEntryPoint);
        if (this->dllInitialize == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiInitializeEntryPoint, this->library, GetLastError());
        }

        this->dllUpdate = (BMI_UPDATE) GETPROCADDRESS (dllhandle, BmiUpdateEntryPoint);
        if (this->dllUpdate == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiUpdateEntryPoint, this->library, GetLastError());
        }

        this->dllFinalize = (BMI_FINALIZE) GETPROCADDRESS (dllhandle, BmiFinalizeEntryPoint);
        if (this->dllFinalize == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiFinalizeEntryPoint, this->library, GetLastError());
        }

        this->dllGetStartTime = (BMI_GETSTARTTIME) GETPROCADDRESS (dllhandle, BmiGetStartTimeEntryPoint);
        if (this->dllGetStartTime == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetStartTimeEntryPoint, this->library, GetLastError());
        }

        this->dllGetEndTime = (BMI_GETENDTIME) GETPROCADDRESS (dllhandle, BmiGetEndTimeEntryPoint);
        if (this->dllGetEndTime == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetEndTimeEntryPoint, this->library, GetLastError());
        }

        this->dllGetTimeStep = (BMI_GETTIMESTEP) GETPROCADDRESS (dllhandle, BmiGetTimeStepEntryPoint);
        if (this->dllGetStartTime == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetStartTimeEntryPoint, this->library, GetLastError());
        }

        this->dllGetCurrentTime = (BMI_GETCURRENTTIME) GETPROCADDRESS (dllhandle, BmiGetCurrentTimeEntryPoint);
        if (this->dllGetCurrentTime == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetCurrentTimeEntryPoint, this->library, GetLastError());
        }

        this->dllSetVar = (BMI_SETVAR) GETPROCADDRESS (dllhandle, BmiSetVarEntryPoint);
        if (this->dllSetVar == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiSetVarEntryPoint, this->library, GetLastError());
        }

        this->dllGetVar = (BMI_GETVAR) GETPROCADDRESS (dllhandle, BmiGetVarEntryPoint);
        if (this->dllGetVar == NULL) {
            throw Exception (true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetVarEntryPoint, this->library, GetLastError());
        }

}

//------------------------------------------------------------------------------
void DimrExe::freeLib (void) {

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw Exception (true, Exception::ERR_OS, "ABORT: %s has not be ported to Apple Mac OS/X yet", this->exeName);
#endif
#ifndef _WIN32
    char *err;
#endif

        this->log->Write (ALL, my_rank, "Freeing library \"%s\"", this->library);
#ifndef _WIN32
        dlerror(); /* clear error code */
        int ierr = dlclose(this->libHandle);
        if ((err = dlerror()) != NULL) {
            throw Exception (true, Exception::ERR_OS, "Cannot free component library \"%s\". Error: %s\n",  this->library, err);
        }
#else
        DWORD ierr;
        SetLastError(0); /* clear error code */
        BOOL success = FreeLibrary(this->libHandle);
        if (success == 0 || (ierr = GetLastError()) != 0) {
            throw Exception (true, Exception::ERR_OS, "Cannot free component library \"%s\". Return code: %d.", this->library, ierr);
        }
#endif

}



//------------------------------------------------------------------------------
static void printAbout (char * exeName) {
    printf ("\n\
%s \n\
Copyright (C)  Stichting Deltares, 2011-2024. \n\
GNU General Public License, see <http://www.gnu.org/licenses/>. \n\n\
sales@deltaressystems.nl \n", getfullversionstring_dimr_exe());
    printf("%s\n\n", geturlstring_dimr_exe());
 }



//------------------------------------------------------------------------------
static void printUsage (char * exeName) {

    printf ("\n\
Usage: \n\
    %s [<options>] <configurationFile> \n\
Options: \n\
    -d <int> \n\
        Specify debug/log level \n\
        Maximum output: 0 \n\
    -v \n\
                Verbose \n\
        Same as \"-d 0\", all logging levels \n\
    -l <filename>\n\
        Log debug/trace messages to the specified file instead of stdout \n\
    -i \n\
        Info: print version, contact, and other information about this program \n\
    -? \n\
        Print this usage synopsis \n\
Configuration file: \n\
    XML format, DTD or schema not yet available \n\
    If a \"-\" is specified the configuration is read from standard input \n\
    \n\
    \n", exeName);
}

