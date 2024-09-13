//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
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
// $Id: flow2d3d.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/flow2d3d.cpp $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  29 jun 12
//------------------------------------------------------------------------------
//
/// \file
/// The dynamic library 'Delft3D-FLOW'.
/// Flow-related routines are in trisim.f90 and below.
///

// Content specifically for the Doxygen-index page:
/**
 * \mainpage Delft3D-FLOW API docs
 * \li <b> Main program</b>: d_hydro.cpp
 * \li <b> Dynamic library entrances</b>: flow2d3d_dll.f90
 * \li <b> DD and RemoteOLV preparations, start all processes</b>: hydra.cpp (Hydra::Execute)
 * \li <b> Global data</b>: globaldata.f90 (flow, geometry, times, parameters, ...)
 * \li <b> Subdomain calculation toplevel</b>: trisim.f90
 * \li <b> Preprocessor: Convert time related data</b>: tdatom.f90
 * \li <b> Timeloop</b>: tricom_step.F90
 * \li <b> Main routine within one time step</b>:
 * trisol.f90 (sigma layers)
 * z_trisol.f90 (z layers, hydrostatic)
 * z_trisol_nhfull.f90 (z layers, non-hydrostatic)
 */

/**
 * Perform a Delft3D-FLOW computation, given a set of configuration parameters.
 *
 * Phases:
 * - 1) Interpretation of the keys/values from the config file
 * - 2) Create runid, based on mdfFile/ddbFile
 * - 3) In dd_execute:
 *    - RemoteOLV initialization
 *    - DD initialization
 *    - Start all processes:
 */
//------------------------------------------------------------------------------


#define FLOW2D3D_MAIN

#include "flow2d3d.h"

#if defined(HAVE_CONFIG_H)
#define Sleep sleep
#endif
#if defined (WIN32)
#   include <windows.h>
#   define strdup _strdup
#endif


DllExport bool
DeltaresHydroEntry (
    DeltaresHydro * DH
    ) {

    try {
        DH->startComponent = new Flow2D3D (DH);
        return true;
        }
    catch (Exception * ex) {
        bool written = DH->log->Write (Log::ALWAYS, "Cannot start Flow2D3D: %s", ex->message);
        if (! written) printf ("ABORT: Cannot start Flow2D3D: %s\n", ex->message);
        return false;
        }
    }


bool
FLOW2D3D_MonolithicInit (
    DeltaresHydro * DH
    ) {

    return DeltaresHydroEntry (DH);
    }


//------------------------------------------------------------------------------
//  Constructor


Flow2D3D::Flow2D3D (
    DeltaresHydro * DH
    ) : Component (
        DH
        ) {

    FLOW2D3D = this;

	if (DH == NULL) {
		this->flowol = NULL;
		return;
	}

    this->config             = this->DH->start;
    this->dd                 = NULL;
    this->log                = DH->log;
    this->flowol             = NULL;
    this->mdfFile            = this->config->GetElement ("mdfFile");
    this->runid              = NULL;

    const char * ddbFile = this->config->GetElement ("ddbFile");

    if (this->mdfFile == NULL && ddbFile == NULL)
        throw new Exception (true, "Neither MDF file nor DD bounds file specified");
    if (this->mdfFile != NULL && ddbFile != NULL)
        throw new Exception (true, "Both MDF file nor DD bounds file specified");

    if (ddbFile != NULL)
        this->dd = new DD (this, this->config);

    // Set up DelftOnline if requested and not in slave mode.
    // Slaves will do it themselves at the right time.

    XmlTree * dolconfig = this->DH->config->Lookup ("/deltaresHydro/delftOnline");
    if (dolconfig != NULL && this->DH->slaveArg == NULL ) {
        // ToDo: Check enabled element
        this->DH->log->Write (Log::MAJOR, "Initializing DelftOnline...");
        this->flowol = new FlowOL (this->DH, dolconfig);
        }

    // Initialize ESM/FSM

    this->esm_flags = ESM_SILENT;

    /*
    ToDo: Process ESM/FSM XML element

    XmlTree * esmconfig = this->config->Lookup ("EsmFsm");
    if (esmconfig != NULL) {
        XmlTree * trace = esmconfig->Lookup ("Trace");
        if (trace != NULL) {
            this->esm_flags = ESM_TRACE;
            }
        }
    */

    ESM_Init (this->esm_flags);
    }


Flow2D3D::~Flow2D3D (
    void
    ) {

    if (this->dd)
        delete this->dd;

    if (this->runid != NULL)
        free (this->runid);

    this->DH->log->Write (Log::MAJOR, "Flow2D3D instance destroyed");
    }


//------------------------------------------------------------------------------


void
Flow2D3D::Run (
    void
    ) {

    // The following waitFile code is introduced for
    // debugging parallem runs.  It should NOT be used for any other purpose!

    const char * waitFile = this->config->GetElement ("waitFile");
    if (waitFile != NULL) {
        printf ("Waiting for file \"%s\" to appear...\n", waitFile);
        fflush (stdout);
        FILE * f;
        do {
            f = fopen (waitFile, "r");
            Sleep (1000);
            } while (f == NULL);

        fclose (f);
        }

    try {
        if (this->dd != NULL)
            this->dd->Run ();

        else {
            this->DH->log->Write (Log::MAJOR, "Flow2D3D running single-domain simulation...");

            // By convention the runid is the part of the MD file name before the extension

            this->runid = strdup (this->mdfFile);
            char * dot = strrchr (this->runid, '.'); // search last dot
            if (dot != NULL) *dot = '\0';

            if (this->flowol != NULL) {
                this->flowol->numSubdomains = 1;
                this->flowol->RegisterSubdomain (runid);
                }

            int numsubdomains = 0;
            int nummappers = 0;
            int fsm_flags = this->esm_flags;

            int context_id = ESM_Create (0, 0);
            if (context_id == 0)
                throw new Exception (true, "Cannot create memory context for Flow2D3D");

            this->DH->log->Write (Log::MAJOR, "Calling TRISIM (Fortran)");
            TRISIM (&numsubdomains, &nummappers, &context_id, &fsm_flags, runid, strlen (runid));
            this->DH->log->Write (Log::MAJOR, "TRISIM returns (Fortran)");

            if (this->flowol != NULL)
                this->flowol->UnregisterSubdomain ();
            int result = ESM_Delete(context_id);
            }
        }

    catch (Exception * ex) {
        this->DH->log->Write (Log::ALWAYS, "Exception in Flow2D3D::Run: %s", ex->message);
        }
    }
