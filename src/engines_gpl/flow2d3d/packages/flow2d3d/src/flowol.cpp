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
// $Id: flowol.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/flowol.cpp $
//-------------------------------------------------------------------------------
//  d_hydro Flow2FLOW Component
//  Interface to DelftOnline - IMPLEMENTATION
//
//  Irv.Elshoff@deltares.NL
//  6 mar 13
//-------------------------------------------------------------------------------


#ifdef WIN32
#   define strcasecmp _stricmp
#endif


#include "flow2d3d.h"
#include "flowol.h"
#include <mpi.h>

//-------------------------------------------------------------------------------


FlowOL::FlowOL (
    DeltaresHydro * dh,
    XmlTree * config
    ) {

#if defined (WITH_DELFTONLINE)
    this->dh = dh;
    this->numSubdomains = 0;
	this->mpiInitialized = false;

    // Get DOL options from the configuration tree

    const char * urlFile = config->GetElement ("urlFile");
    if (urlFile == NULL)
        throw new Exception (true, "urlFile not specified in delftOnline section of the configuration file");

    bool allowStart = ! config->GetBoolElement ("waitOnStart", false);
    bool allowControl = config->GetBoolElement ("clientControl", true);
    allowControl |= ! allowStart;

    DOL::Verbosity verbosity = DOL::SILENT;
    const char * verb = config->GetElement ("verbosity");
    if (verb != NULL) {
        if (strcasecmp (verb, "silent") == 0)
            verbosity = DOL::SILENT;
        else if (strcasecmp (verb, "error") == 0)
            verbosity = DOL::ERROR;
        else if (strcasecmp (verb, "info") == 0)
            verbosity = DOL::INFO;
        else if (strcasecmp (verb, "trace") == 0)
            verbosity = DOL::TRACE;
        else
            throw new Exception (true, "Invalid verbosity value \"%s\" in DelftOnline section of the configuration file", verb);
        }

    uint16_t firstPort = 0;
    uint16_t lastPort = 0;

    const char * ports = config->GetElement ("port");
    if (ports != NULL) {
        const char * dash = strchr (ports, '-');
        if (dash == NULL)
            firstPort = atoi (ports);
        else if (sscanf (ports, "%hu-%hu", &firstPort, &lastPort) != 2)
            throw new Exception (true, "Invalid port range specification (\"%s\") in DelftOnline section of the configuration file", ports);

        if (firstPort < 1024 || firstPort > 65535)
            throw new Exception (true, "Invalid (first) port (%d) in DelftOnline section of the configuration file", firstPort);
        if (lastPort != 0 && (lastPort < 1024 || lastPort > 65535))
            throw new Exception (true, "Invalid last port (%d) in DelftOnline section of the configuration file", lastPort);
        if (lastPort != 0 && firstPort > lastPort)
            throw new Exception (true, "Invalid port range in DelftOnline section of the configuration file");
        }

    // Initialize DelftOnline and write handle to URL file

    try {
        this->dh->log->Write (Log::MINOR, "Creating DOL server");
        this->dol = new DOL::Server (allowStart, true, verbosity, NULL, firstPort, lastPort);

        char * url = this->dol->Handle ();
        this->dh->log->Write (Log::MAJOR, "DOL handle is \"%s\"", url);

		// When running parallel using MPI:
		// Each partition writes an URL file named "urlFile_rank"
		char * urlFileFull = new char [strlen(urlFile) + 10]; /* 10: _rank + EOL */
		strcpy(urlFileFull,urlFile);
		// MPI_Init call is needed here,
		// but will only be executed when DOL is switched on
		// This call is definetely before the mpi_init call in the Fortran part (dfinitmpi.F90)
		// In Fortran, check whether MPI_Init is already called (with DOL) or not (without DOL)
		MPI_Init(NULL, NULL);
		this->mpiInitialized = true;
		// Pass MPI_COMM_WORLD to "engine_comm_world" of Delft3D-FLOW using BMI interface
        MPI_Fint *fComm;
        get_var("engine_comm_world", &fComm);
        *fComm = MPI_Comm_c2f(MPI_COMM_WORLD);
		//
		int MPIsize;
		MPI_Comm_size (MPI_COMM_WORLD, &MPIsize);
		if (MPIsize > 1) {
			int rank;
			MPI_Comm_rank (MPI_COMM_WORLD, &rank);
			sprintf(urlFileFull, "%s_%d", urlFile, rank);
			}
        FILE * f = fopen (urlFileFull, "w");
        if (f == NULL)
            throw new Exception ("Cannot create URL file \"%s\": %s", urlFileFull, strerror (errno));
        fprintf (f, "%s\n", url);
        fclose (f);
		delete [] urlFileFull;
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "DOL initialization fails: %s", ex->message);
        }
#endif
    }


FlowOL::~FlowOL (
    void
    ) {

#if defined (WITH_DELFTONLINE)
    if (this->mpiInitialized)
		MPI_Finalize();
    this->dh->log->Write (Log::MINOR, "Shutting down DOL");
    delete this->dol;
#endif
    }


//-------------------------------------------------------------------------------


void
FlowOL::RegisterSubdomain (
    const char * name
    ) {

#if defined (WITH_DELFTONLINE)
    if (this->dol == NULL) return;

    try {
        this->dol->RegisterThread  (this->numSubdomains, name);
#ifdef WIN32
        this->dol->CreateDirectoryA (name);
#else
        this->dol->CreateDirectory (name);
#endif
        this->dol->ChangeDirectory (name);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "DOL subdomain registration fails: %s", ex->message);
        }
#endif
    }


void
FlowOL::UnregisterSubdomain (
    void
    ) {

    // No-op
    }


//-------------------------------------------------------------------------------
//  Fortran-to-C++ string conversion function


static char *
createCString (
    const char *    fortString,
    int             fortLen
    ) {

    // Look for the last non-space character in the Fortran string (padded with spaces)

    int end;
    for (end = fortLen-1 ; end >= 0 ; end--)
        if (fortString[end] != ' ')
            break;

    if (end < 0)
        end = 0;    // all spaces (or fortlen == 0) becomes empty string
    else
        end++;      // include last non-space char

    // Allocate a C buffer for the truncated string and copy Fortran string

    char * cString = new char [end + 1 /* for EOL */];
    for (int i = 0 ; i < end ; i++)
        cString[i] = fortString[i];

    cString[end] = '\0';
    return cString;
    }


//-------------------------------------------------------------------------------
//  Publication and timestep functions (called from Fortran)


extern "C" {    // closing brace at end of file

void STDCALL
FLOWOL_ArrayShape (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    int *           dimensionality,
    int             dimensions []
#else
    const char *    ftn_name,
    int *           dimensionality,
    int             dimensions [],
    int             len_name
#endif
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * name = createCString (ftn_name, len_name);
    const char * dir  = "";

    try {
        flowol->dol->PublishArrayShape (dir, name, *dimensionality, dimensions);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "DOL ArrayShape fails: %s", ex->message);
        }

    delete [] name;
#endif
    }


void STDCALL
FLOWOL_ChangeDirectory (
    const char *    ftn_dirname,
    int             len_dirname
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * dirname = createCString (ftn_dirname, len_dirname);

    try {
        flowol->dol->ChangeDirectory (dirname);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "Cannot set DOL description: %s", ex->message);
        }

    delete [] dirname;
#endif
    }


void STDCALL
FLOWOL_Publish_c (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    const char *    ftn_description,
    int             len_description,
    const char *    ftn_units,
    int             len_units,
    const char *    ftn_definedon,
    int             len_definedon,
    const char *    ftn_arrayshape,
    int             len_arrayshape,
    int *           basetype,
    void *          address,
    int *           inout
#else
    const char *    ftn_name,
    const char *    ftn_description,
    const char *    ftn_units,
    const char *    ftn_definedon,
    const char *    ftn_arrayshape,
    int *           basetype,
    void *          address,
    int *           inout,
    int             len_name,
    int             len_description,
    int             len_units,
    int             len_definedon,
    int             len_arrayshape
#endif
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * name        = createCString (ftn_name, len_name);
    char * description = createCString (ftn_description, len_description);
    char * units       = createCString (ftn_units, len_units);
    char * definedon   = createCString (ftn_definedon, len_definedon);
    char * arrayshape  = createCString (ftn_arrayshape, len_arrayshape);
    const char * dir = "";

    try {
        flowol->dol->Publish (dir, name, description, units, definedon, arrayshape, (DOL::BaseType) *basetype, address, (DOL::AccessMode) *inout);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "DOL Publish fails: %s", ex->message);
        }

    delete [] name;
    delete [] description;
    delete [] units;
    delete [] definedon;
    delete [] arrayshape;
#endif
    }


void STDCALL
FLOWOL_Publish_string_c (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    const char *    ftn_description,
    int             len_description,
    const char *    ftn_units,
    int             len_units,
    const char *    ftn_definedon,
    int             len_definedon,
    const char *    ftn_arrayshape,
    int             len_arrayshape,
    int *           basetype,
    void *          address,
    int             len_addres,
    int *           inout
#else
    const char *    ftn_name,
    const char *    ftn_description,
    const char *    ftn_units,
    const char *    ftn_definedon,
    const char *    ftn_arrayshape,
    int *           basetype,
    void *          address,
    int *           inout,
    int             len_name,
    int             len_description,
    int             len_units,
    int             len_definedon,
    int             len_arrayshape,
    int             len_address
#endif
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

#if defined(MIXED_STR_LEN_ARG)
    FLOWOL_Publish_c (ftn_name, len_name, ftn_description, len_description, ftn_units, len_units, ftn_definedon, len_definedon, ftn_arrayshape, len_arrayshape, basetype, address, inout);
#else
    FLOWOL_Publish_c (ftn_name, ftn_description, ftn_units, ftn_definedon, ftn_arrayshape, basetype, address, inout, len_name, len_description, len_units, len_definedon, len_arrayshape);
#endif
#endif
    }


void STDCALL
FLOWOL_PublishFunction (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_name,
    int             len_name,
    const char *    ftn_description,
    int             len_description,
    void *          address,
    void *          dataptr
#else
    const char *    ftn_name,
    const char *    ftn_description,
    void *          address,
    void *          dataptr,
    int             len_name,
    int             len_description
#endif
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * name = createCString (ftn_name, len_name);
    char * description = createCString (ftn_description, len_description);
    const char * dir = "";

    try {
        flowol->dol->PublishFunction (dir, name, description, DOL::FORTRAN, (int (STDCALL *)(void *, const int *)) address, dataptr);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "DOL PublishFunction fails: %s", ex->message);
        }

    delete [] name;
    delete [] description;
#endif
    }


void STDCALL
FLOWOL_SetDescription (
#if defined(MIXED_STR_LEN_ARG)
    const char *    ftn_prog,
    int             len_prog,
    const char *    ftn_model,
    int             len_model
#else
    const char *    ftn_prog,
    const char *    ftn_model,
    int             len_prog,
    int             len_model
#endif
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * prog = createCString (ftn_prog, len_prog);
    char * model = createCString (ftn_model, len_model);

    char * desc = new char [strlen (prog) + strlen (model) + 10];
    sprintf (desc, "%s :: %s", prog, model);

    delete [] prog;
    delete [] model;

    try {
        flowol->dol->SetDescription (desc);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "Cannot set DOL description: %s", ex->message);
        }

    delete [] desc;
#endif
    }


void STDCALL
FLOWOL_Timestep (
    int *   timestep
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    bool echoContinue = false;

    if (flowol == NULL || flowol->dol == NULL)
        return;

    if (flowol->dol->waitOnStart()) {
        echoContinue = true;
        printf("\tWaiting for Online Visualisation ...\n");
        fflush (stdout);
    }

    try {
        flowol->dol->PassMilestone ((DOL::Milestone) *timestep);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "DOL fails: %s", ex->message);
        }
    if (echoContinue) {
        printf("Continuing\n");
        fflush (stdout);
    }
#endif
    }


//-------------------------------------------------------------------------------
//  Retraction functions (called from Fortran)


void STDCALL
FLOWOL_Retract (
    const char *    ftn_name,
    int             len_name
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * name = createCString (ftn_name, len_name);

    try {
        flowol->dol->Retract (NULL, name);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "Cannot Retract DOL DataElement: %s", ex->message);
        }

    delete [] name;
#endif
    }


void STDCALL
FLOWOL_RetractArrayShape (
    const char *    ftn_name,
    int             len_name
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * name = createCString (ftn_name, len_name);

    try {
        flowol->dol->RetractArrayShape (NULL, name);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "Cannot Retract DOL ArrayShape: %s", ex->message);
        }

    delete [] name;
#endif
    }


void STDCALL
FLOWOL_RetractFunction (
    const char *    ftn_name,
    int             len_name
    ) {

#if defined (WITH_DELFTONLINE)
    FlowOL * flowol = FLOW2D3D->flowol;
    if (flowol == NULL)
        return;

    char * name = createCString (ftn_name, len_name);

    try {
        flowol->dol->RetractFunction (NULL, name);
        }

    catch (DOL::Exception * ex) {
        throw new Exception (true, "Cannot Retract DOL Function: %s", ex->message);
        }

    delete [] name;
#endif
    }

void STDCALL
FLOWOL_Nullify () {
	FLOW2D3D = new Flow2D3D(NULL);
    }
}
