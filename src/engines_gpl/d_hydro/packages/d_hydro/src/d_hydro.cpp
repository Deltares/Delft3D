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
// $Id: d_hydro.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/d_hydro/packages/d_hydro/src/d_hydro.cpp $
//------------------------------------------------------------------------------
//  D_Hydro Main Program
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#define D_HYDRO_MAIN
//#define MEMCHECK

#include "d_hydro.h"
#include "d_hydro_version.h"

#if defined(HAVE_CONFIG_H)
#include "config.h"
#include <dlfcn.h>
#include <libgen.h>
#endif
#include <expat.h>
#include <limits.h>


#if defined (MEMCHECK)
#include <mcheck.h>
#endif


#include <typeinfo>
using namespace std;


#if defined (WIN32)
#   include "getopt.h"
#   include <Strsafe.h>
#   include <windows.h>
#  define strdup _strdup
#endif


static void printAbout          (char * exeName);
static void printUsage          (char * exeName);


#if defined (MONOLITHIC_FLOW2D3D)
bool
FLOW2D3D_MonolithicInit (
    DeltaresHydro * DH
    );
#endif


//------------------------------------------------------------------------------


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

int
main (
    int     argc,
    char *  argv [],
    char *  envp []
    ) {

#if defined (MEMCHECK)
    int rc = mcheck_pedantic (& memAbort);
    printf ("DEBUG: mcheck_pedantic returns %d\n", rc);
#endif

    try {
        DeltaresHydro * DH = new DeltaresHydro (argc, argv, envp);
        if (! DH->ready) return 1;
        DH->Run ();
        delete DH;
        return 0;
        }

    catch (exception& ex) {
        printf ("d_hydro ABORT: C++ Exception: %s\n", ex.what());
        return 1;
        }
    catch (Exception *ex) {
        printf ("d_hydro ABORT: %s\n", ex->message);
        return 1;
        }
    catch (char * str) {
        printf ("ABORT in FLOW2D3D: %s\n", str);
        return 1;
        }
    }


//------------------------------------------------------------------------------
//  Constructor.  Initialize the object, read the configuration file and load
//  and invoke the start component's entry function.  All of the work is done
//  by the start component, including loading other components if necessary.
//  ToDo: make load-and-start a generic function available to any component.


DeltaresHydro::DeltaresHydro (
    int     argc,
    char *  argv [],
    char *  envp []
    ) {

    this->exePath = strdup (argv[0]);

#if defined(HAVE_CONFIG_H)
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

    Log::Mask   logMask = Log::ALWAYS;  // selector of debugging/trace information
                                        // minLog: Log::SILENT  maxLog: Log::TRACE
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

    // Process command-line arguments

    int c;
    while ((c = getopt (argc, argv, (char *) "d:l:S:v?")) != -1) {
        switch (c) {
            case 'd': {
                if (sscanf (optarg, "%i", &logMask) != 1)
                    throw new Exception (true, "Invalid log mask (-d option)");
                break;
                }

            case 'l': {
                logFile = fopen (optarg, "w");
                if (logFile == NULL)
                    throw new Exception (true, "Cannot create log file \"%s\"", optarg);

                break;
                }

            case 'S': {
                this->slaveArg = optarg;
                break;
                }

            case 'v': {
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
                throw new Exception (true, "Invalid command-line argument.  Execute \"%s -?\" for command-line syntax", this->exeName);
                }
            }
        }

    if (argc - optind != 1)
        throw new Exception (true, "Improper usage.  Execute \"%s -?\" for command-line syntax", this->exeName);

    this->clock = new Clock ();
    this->log = new Log (logFile, this->clock, logMask);

    // Read XML configuration file into tree structure

    this->configfile = argv[optind];
    FILE * conf;
    if (strcmp (this->configfile, "-") == 0)
        conf = stdin;
    else {
        conf = fopen (this->configfile, "r");
        if (conf == NULL)
            throw new Exception (true, "Cannot open configuration file \"%s\"", this->configfile);
        }

    this->config = new XmlTree (conf);
    fclose (conf);

    // ToDo: check whether a core dump is requested on abort; if so set global variable for D_Hydro_CoreDump

    // Get the name and configuration subtree of the start component

    XmlTree * root = this->config->Lookup ("/deltaresHydro");
    if (root == NULL)
        throw new Exception (true, "Configuration file \"%s\" does not have a deltaresHydro root element", this->configfile);

    // ToDo: Process control element

    const char * startName = "flow2D3D";
    this->start = root->Lookup (startName);
    if (this->start == NULL)
        throw new Exception (true, "Configuration element for start component \"%s\" is missing", startName);

#if defined (MONOLITHIC_FLOW2D3D)
    //  ToDo: make sure the start component is Flow2D3D.
    //  The monolithic version is for the moment a possible work around for a Linux problem.

    this->ready = FLOW2D3D_MonolithicInit (this);

#else
    //  Load the library for the start component and invoke its entry function.  If the
    //  library attibute is specified and it contains a (back)slash or dot it is used
    //  as the actual file name of the library.  The library name defaults to the name
    //  of the start component, and is converted to a file name using platform-dependent
    //  conventions.
    //  ToDo: deal with envar paths

    XmlTree * libraryElement = this->start->Lookup ("library");
    if (libraryElement == NULL)
        throw new Exception (true, "The start component \"%s\" does not contain a library element", startName);

    const char * library = libraryElement->charData;

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (HAVE_CONFIG_H)
#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw new Exception (true, "ABORT: %s has not be ported to Apple Mac OS/X yet", this->exeName);

#else
    // LINUX
    char lib [strlen (library) + 3+3+1];
    if (strchr (library, '/') == NULL && strchr (library, '.') == NULL) {
        sprintf (lib, "lib%s%s", library, D3D_PLUGIN_EXT);
        library = lib;
        }

    this->log->Write (Log::DETAIL, "Loading start library \"%s\"", library);

    void * handle = dlopen (library, RTLD_LAZY);
    if (handle == NULL)
        throw new Exception (true, "Cannot load component library \"%s\": %s", library, dlerror ());

    StartComponentEntry entryPoint = (StartComponentEntry) dlsym (handle, this->startEntry);
    if (entryPoint == NULL)
        throw new Exception (true, "Cannot find function \"%s\" in library \"%s\"", this->startEntry, library);

    this->log->Write (Log::DETAIL, "Calling entry function in start library");
    this->ready = entryPoint (this);
#endif
#elif defined (WIN32)
    char * lib = new char[strlen (library) + 4+1];
    if (strchr (library, '/') == NULL && strchr (library, '\\') == NULL && strchr (library, '.') == NULL) {
        sprintf (lib, "%s.dll", library);
        library = lib;
        }

    this->log->Write (Log::DETAIL, "Loading start library \"%s\"", library);

    HINSTANCE dllhandle = LoadLibrary (LPCSTR(library));
    if (dllhandle == NULL)
        throw new Exception (true, "Cannot load component library \"%s\". Return code: %d", library, GetLastError());

    typedef int (__cdecl *MYPROC)(LPWSTR);
    MYPROC entryPoint = (MYPROC) GetProcAddress (dllhandle, this->startEntry);
    if (entryPoint == NULL)
        throw new Exception (true, "Cannot find function \"%s\" in library \"%s\". Return code: %d", this->startEntry, library, GetLastError());

    this->log->Write (Log::DETAIL, "Calling entry function in start library");
    this->ready = ((entryPoint) ((LPWSTR) this));
    delete [] lib;
#endif
#endif      // MONOLITHIC_FLOW2D3D
    }

//
//------------------------------------------------------------------------------
//  Destructor

DeltaresHydro::~DeltaresHydro (
    void
    ) {

    if (this->done)
        return;

    // to do:  (void) FreeLibrary(handle);

    delete this->startComponent;

#if defined(HAVE_CONFIG_H)
    free (this->exeName);
#else
    delete [] this->exeName;
#endif
    this->log->Write (Log::ALWAYS, "d_hydro shutting down normally");

   delete this->clock;
   delete this->config;
   free (this->exePath);
   delete this->log;
   delete [] this->mainArgs;
    }



//------------------------------------------------------------------------------


void
DeltaresHydro::Run (
    void
    ) {

    try {
        if (this->done == true) return;
        this->startComponent->Run ();
        }

    catch (Exception *ex) {
        printf ("#### d_hydro ABORT: %s\n", ex->message);
        }
    }


//------------------------------------------------------------------------------


static void
printAbout (
    char * exeName
    ) {
    char * fullversion = getfullversionstring_d_hydro ();
    printf ("\n\
%s \n\
Copyright (C)  Stichting Deltares, 2011-2024. \n\
GNU General Public License, see <http://www.gnu.org/licenses/>. \n\n\
delft3d.support@deltares.nl \n", fullversion);
    char* urlstr = geturlstring_d_hydro ();
    printf ("%s\n\n", urlstr);
    }


static void
printUsage (
    char * exeName
    ) {

    printf ("\n\
Usage: \n\
    %s [<options>] <configurationFile> \n\
Options: \n\
    -d <bitmask> \n\
        Specify debug/trace output as a bit mask in decimal or hex \n\
        Maximum output: 0xFFFFFFFF \n\
    -l <filename>\n\
        Log debug/trace messages to the specified file instead of stdout \n\
    -v \n\
        Print version, contact, and other information about this program \n\
    -? \n\
        Print this usage synopsis \n\
Configuration file: \n\
    XML format, DTD or schema not yet available \n\
    If a \"-\" is specified the configuration is read from standard input \n\
    \n\
    \n", exeName);
    }
