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
// $Id: rtccomm.h 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/rtccomm.h $
//------------------------------------------------------------------------------
//  d_hydro Flow2D3D Component
//  Inter-domain RTC module communication synchronization - DEFINITIONS
//
//  Irv.Elshoff@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#pragma once

#include "flow2d3d.h"

#include "precision.h"


#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define DD_RTCNOCOMMUNICATION    FC_FUNC(dd_rtcnocommunication,DD_RTCNOCOMMUNICATION)
#   define DD_RTCSTARTCOMMUNICATION FC_FUNC(dd_rtcstartcommunication,DD_RTCSTARTCOMMUNICATION)
#   define DD_RTCCOMMUNICATE        FC_FUNC(dd_rtccommunicate,DD_RTCCOMMUNICATE)
#   define DD_RTCCHARCOMMUNICATE    FC_FUNC(dd_rtccharcommunicate,DD_RTCCHARCOMMUNICATE)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define DD_RTCNOCOMMUNICATION    DD_RTCNOCOMMUNICATION
#   define DD_RTCSTARTCOMMUNICATION DD_RTCSTARTCOMMUNICATION
#   define DD_RTCCOMMUNICATE        DD_RTCCOMMUNICATE
#   define DD_RTCCHARCOMMUNICATE    DD_RTCCHARCOMMUNICATE
#endif


//-------------------------------------------------------------------------------


namespace RTC {
    enum {
        F2RC_init     = 108021,
        F2RC_values   = 108022,
        F2RC_strings  = 108023,
        RC2F_init     = 801021,
        RC2F_values   = 801022,
        RC2F_strings  = 801023,
        };
    };


//-------------------------------------------------------------------------------


void
Rtc_Function (
    Iterator *      self,
    const char *    name,
    Blob *          configblob
    );


extern "C" {
    void STDCALL
    RTCSTARTCOMMUNICATION (
        int * domainnumber,
        int * numdomains
        );

    void STDCALL
    RTCCOMMUNICATE (
        REAL_FP * values,
        int     * numelements
        );

    void STDCALL
    RTCCHARCOMMUNICATE (
        char    * strings,
        int     * numelements,
        int     numchar
        );

    void STDCALL
    RTCNOCOMMUNICATION (
        void
        );
    }
