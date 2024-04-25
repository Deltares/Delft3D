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
// $Id: flow2d3d.h 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/include/flow2d3d.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#pragma once

#include "d_hydro.h"

class Flow2D3D;


//------------------------------------------------------------------------------
//  Function names for FORTRAN-C interface.


#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define TRISIM FC_FUNC(trisim,TRISIM)
#   define TRISIM_UPDATE FC_FUNC(trisim_update,TRISIM_UPDATE)
#   define TRISIM_FINALIZE FC_FUNC(trisim_finalize,TRISIM_FINALIZE)
#   define TRISIM_GET_START_TIME FC_FUNC(trisim_get_start_time,TRISIM_GET_START_TIME)
#   define TRISIM_GET_END_TIME FC_FUNC(trisim_get_end_time,TRISIM_GET_END_TIME)
#   define TRISIM_GET_TIME_STEP FC_FUNC(trisim_get_time_step,TRISIM_GET_TIME_STEP)
#   define TRISIM_GET_CURRENT_TIME FC_FUNC(trisim_get_current_time,TRISIM_GET_CURRENT_TIME)
#   define TRISIM_GET_VAR FC_FUNC(trisim_get_var,TRISIM_GET_VAR)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define TRISIM TRISIM
#   define TRISIM_UPDATE TRISIM_UPDATE
#   define TRISIM_FINALIZE TRISIM_FINALIZE
#   define TRISIM_GET_START_TIME TRISIM_GET_START_TIME
#   define TRISIM_GET_END_TIME TRISIM_GET_END_TIME
#   define TRISIM_GET_TIME_STEP TRISIM_GET_TIME_STEP
#   define TRISIM_GET_CURRENT_TIME TRISIM_GET_CURRENT_TIME
#   define TRISIM_GET_VAR TRISIM_GET_VAR
#endif


extern "C" {
    void STDCALL
    TRISIM (
        int *   numSubdomains,
        int *   numMappers,
        int *   contextID,
        int *   fsmFlags,
        const char *  runID,
        int *   initOnly,
        void *  gdp,
        size_t  runIDLen
        );
    }

extern "C" {
    void STDCALL
    TRISIM_UPDATE (
        double  tstart,
        void *  gdp
        );
    }

extern "C" {
    void STDCALL
    TRISIM_FINALIZE (
        void *  gdp
        );
    }

extern "C" {
    void STDCALL
    TRISIM_GET_START_TIME (
        double *   tstart,
        void *  gdp
        );
    }

extern "C" {
    void STDCALL
    TRISIM_GET_END_TIME (
        double *   tend,
        void *  gdp
        );
    }

extern "C" {
    void STDCALL
    TRISIM_GET_TIME_STEP (
        double *   tstep,
        void *  gdp
        );
    }

extern "C" {
    void STDCALL
    TRISIM_GET_CURRENT_TIME (
        double *   tcurrent,
        void *  gdp
        );
    }

extern "C" {
    void STDCALL
    TRISIM_GET_VAR (
        const char * key,
        void *       ref,
        void *       gdp
        );
    }


//------------------------------------------------------------------------------


#include "dd.h"
#include "esm.h"
#include "flowol.h"
#include "precision.h"


#if (!defined(WIN32))
#define min(A,B)    (((A) <= (B)) ? (A) : (B))
#define max(A,B)    (((A) >= (B)) ? (A) : (B))
#endif

#ifdef WIN32
#   define DllExport   __declspec( dllexport )
#  define strdup _strdup
#else
#   define DllExport
#endif

// BMI interface
extern "C" {
    DllExport void set_logger(Log *);
    DllExport int  initialize(char *);
    DllExport void update    (double);
    DllExport void finalize  (void);
    DllExport void get_start_time (double *);
    DllExport void get_end_time (double *);
    DllExport void get_time_step (double *);
    DllExport void get_current_time (double *);
    DllExport void get_var (const char *, void *);
    DllExport void set_var (const char *, void *);
}


extern "C" {
    DllExport bool
    DeltaresHydroEntry (
        DeltaresHydro * DHI
        );
    }


//------------------------------------------------------------------------------


class Flow2D3D : public Component {
    public:
        Flow2D3D (
            DeltaresHydro * DHI
            );

        Flow2D3D (
            DeltaresHydro * DHI,
            char          * configfile
            );

        ~Flow2D3D (
            void
            );

        void
        Run (
            void
            );

    public:
        Log *       log;            // logging facility
        XmlTree *   config;         // top of Flow2D3D XML configuration tree
        const char * mdfFile;
        char *      runid;
        FlowOL *    flowol;         // Flow online (via DelftOnline)
        DD *        dd;             // domain decomposition object
        int         esm_flags;
        void *      gdp;

    };


//------------------------------------------------------------------------------


#ifdef FLOW2D3D_MAIN
    DeltaresHydro * DH       = NULL;
    Flow2D3D      * FLOW2D3D = NULL;    // global pointer to single object instance
#else
    extern DeltaresHydro * DH;
    extern Flow2D3D * FLOW2D3D;
#endif
