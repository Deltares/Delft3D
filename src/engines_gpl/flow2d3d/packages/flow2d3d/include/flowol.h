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
// $Id: flowol.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/include/flowol.h $
//-------------------------------------------------------------------------------
//  d_hydro Flow2FLOW Component -- Interface to DelftOnline
//  DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  26 apr 12
//-------------------------------------------------------------------------------


#pragma once

#include "flow2d3d.h"

#ifdef WITH_DELFTONLINE
#include "DelftOnline.h"
#include <mpi.h>
#endif

//-------------------------------------------------------------------------------


class FlowOL {
    public:
        FlowOL (
            DeltaresHydro * dh,
            XmlTree * config
            );

        ~FlowOL (
            void
            );

        void
        RegisterSubdomain (
            const char * name
            );

        void
        UnregisterSubdomain (
            void
            );

    public:
        int     numSubdomains;
		bool    mpiInitialized;
        DeltaresHydro * dh;     // DeltaresHydro object instance (only one)
#ifdef WITH_DELFTONLINE
        DOL::Server *   dol;    // DOL server object reference (only one)
#endif
    };


//-------------------------------------------------------------------------------
//  Fortran-C++ Bridge


#if defined (HAVE_CONFIG_H)
#include "config.h"
#   define STDCALL  /* nothing */
#   define FLOWOL_ArrayShape         FC_FUNC(flowol_arrayshape,FLOWOL_ARRAYSHAPE)
#   define FLOWOL_ChangeDirectory    FC_FUNC(flowol_changedirectory,FLOWOL_CHANGEDIRECTORY)
#   define FLOWOL_Publish_c          FC_FUNC(flowol_publish_c,FLOWOL_PUBLISH_C)
#   define FLOWOL_Publish_string_c   FC_FUNC(flowol_publish_string_c,FLOWOL_PUBLISH_STRING_C)
#   define FLOWOL_PublishFunction    FC_FUNC(flowol_publishfunction,FLOWOL_PUBLISHFUNCTION)
#   define FLOWOL_RetractArrayShape  FC_FUNC(flowol_retractarrayshape,FLOWOL_RETRACTARRAYSHAPE)
#   define FLOWOL_Retract            FC_FUNC(flowol_retract,FLOWOL_RETRACT)
#   define FLOWOL_RetractFunction    FC_FUNC(flowol_retractfunction,FLOWOL_RETRACTFUNCTION)
#   define FLOWOL_SetDescription     FC_FUNC(flowol_setdescription,FLOWOL_SETDESCRIPTION)
#   define FLOWOL_Timestep           FC_FUNC(flowol_timestep,FLOWOL_TIMESTEP)
#   define FLOWOL_Nullify            FC_FUNC(flowol_nullify,FLOWOL_NULLIFY)
#else
// WIN32
#   define FLOWOL_ArrayShape         FLOWOL_ARRAYSHAPE
#   define FLOWOL_ChangeDirectory    FLOWOL_CHANGEDIRECTORY
#   define FLOWOL_Publish_c          FLOWOL_PUBLISH_C
#   define FLOWOL_Publish_string_c   FLOWOL_PUBLISH_STRING_C
#   define FLOWOL_PublishFunction    FLOWOL_PUBLISHFUNCTION
#   define FLOWOL_RetractArrayShape  FLOWOL_RETRACTARRAYSHAPE
#   define FLOWOL_Retract            FLOWOL_RETRACT
#   define FLOWOL_RetractFunction    FLOWOL_RETRACTFUNCTION
#   define FLOWOL_SetDescription     FLOWOL_SETDESCRIPTION
#   define FLOWOL_Timestep           FLOWOL_TIMESTEP
#   define FLOWOL_Nullify            FLOWOL_NULLIFY
#endif

#if defined (WIN32)
#   define STDCALL
#else
#   define STDCALL  /* nothing */
#endif


extern "C" {
#if defined(MIXED_STR_LEN_ARG)
    void STDCALL FLOWOL_ArrayShape           (const char *, int, int *, int []);
    void STDCALL FLOWOL_ChangeDirectory      (const char *, int);
    void STDCALL FLOWOL_Publish_c            (const char *, int, const char *, int, const char *, int, const char *, int, const char *, int, int *, void *, int *);
    void STDCALL FLOWOL_Publish_string_c     (const char *, int, const char *, int, const char *, int, const char *, int, const char *, int, int *, void *, int, int *);
    void STDCALL FLOWOL_PublishFunction      (const char *, int, const char *, int, void *, void *);
    void STDCALL FLOWOL_Retract              (const char *, int);
    void STDCALL FLOWOL_RetractArrayShape    (const char *, int);
    void STDCALL FLOWOL_Retract_Function     (const char *, int);
    void STDCALL FLOWOL_SetDescription       (const char *, int, const char *, int);
    void STDCALL FLOWOL_Timestep             (int *);
    void STDCALL FLOWOL_Nullify              (void);
#else
    void STDCALL FLOWOL_ArrayShape           (const char *, int *, int [], int);
    void STDCALL FLOWOL_ChangeDirectory      (const char *, int);
    void STDCALL FLOWOL_Publish_c            (const char *, const char *, const char *, const char *, const char *, int *, void *, int *, int, int, int, int, int);
    void STDCALL FLOWOL_Publish_string_c     (const char *, const char *, const char *, const char *, const char *, int *, void *, int *, int, int, int, int, int, int);
    void STDCALL FLOWOL_PublishFunction      (const char *, const char *, void *, void *, int, int);
    void STDCALL FLOWOL_Retract              (const char *, int);
    void STDCALL FLOWOL_RetractArrayShape    (const char *, int);
    void STDCALL FLOWOL_Retract_Function     (const char *, int);
    void STDCALL FLOWOL_SetDescription       (const char *, const char *, int, int);
    void STDCALL FLOWOL_Timestep             (int *);
    void STDCALL FLOWOL_Nullify              (void);
#endif
}
