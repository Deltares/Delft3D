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
// $Id: flow_nxtstp.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/flow_nxtstp.h $
//------------------------------------------------------------------------------
//  Module: 3dflwnxt    (DELFT3D-FLOW NextStep)
//  DELFT3D-FLOW / Hydra interface, local include file.
//
//  Stef.Hummel@Deltares.NL
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "flow_steps_c.h"        // step-enumerations for DELFT3D-FLOW and it's D3dFlow-Mapper
#include "context-flowside.h"    // Flow side part of distr. ctx.


/*----------------------------------------------------------------------
 *  Functions for externals (mapper.cpp)
 *----------------------------------------------------------------------*/

extern char * PrintNextStepName(int step);
extern MapDistribGroup DetermineDistribGroup(int step);

/*----------------------------------------------------------------------
 *  API-functions (flow_nextstep.cpp)
 *----------------------------------------------------------------------*/

/*
 *  Function names for FORTRAN-C interface.
 */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define COMMUNICATENEXTSTEPWITHMAPPER FC_FUNC(communicatenextstepwithmapper,COMMUNICATENEXTSTEPWITHMAPPER)
#   define GWSSLV FC_FUNC(gwsslv,GWSSLV)
#   define WRITEFMESSAGE FC_FUNC(writefmessage,WRITEFMESSAGE)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define COMMUNICATENEXTSTEPWITHMAPPER COMMUNICATENEXTSTEPWITHMAPPER
#   define GWSSLV GWSSLV
#   define WRITEFMESSAGE WRITEFMESSAGE
#endif


/*
 *  Function definitions
 */

#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

int     STDCALL COMMUNICATENEXTSTEPWITHMAPPER(int * curStep, int * driedOrNot);
void    STDCALL GWSSLV(int * leftToRight);

void    STDCALL WRITEFMESSAGE(const char * message, int messLen);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif

