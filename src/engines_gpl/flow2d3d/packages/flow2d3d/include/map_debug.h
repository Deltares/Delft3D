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
// $Id: map_debug.h 883 2011-10-07 16:32:16Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/map_debug.h $
//------------------------------------------------------------------------------
//  Debug flags for Mappers
//
//  Stef.Hummel@Deltares.NL
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once



typedef enum {
    HY_OK   = 3,      // Just a message, function ok
    HY_WAR  = 2,      // Warning message, function can go on
    HY_ERR  = 1,      // Fatal error, function not ok
    NR_HY_ERR_LEVEL
    }  DDMapErrorLevel;



#define B_MIN(a, b)     ((a) < (b) ? (a) : (b))
#define B_MAX(a, b)     ((a) > (b) ? (a) : (b))


//
// Fall-back: if POSIX not defined
//
#if !defined(_POSIX_PATH_MAX) && !defined (IRIX)
#define _POSIX_PATH_MAX 256
#endif

#define DO_DEBUG    1

//
// Print Function names and/or errors
//

#define PRT_F1  1
#define PRT_F2  2
#define PRT_F3  4

#define MAPDBG_FUN(name)    if(GetFuncPrintLevel(PRT_F1)){\
                    FLOW2D3D->dd->log->Write (Log::ITER_MAJOR, "FUN: %s",name); }

#define MAPDBG_FUN2(name)   if(GetFuncPrintLevel(PRT_F2)){\
                    FLOW2D3D->dd->log->Write (Log::ITER_MAJOR, "FUN: %s",name); }

#define MAPDBG_FUN3(name)   if(GetFuncPrintLevel(PRT_F3)){\
                    FLOW2D3D->dd->log->Write (Log::ITER_MAJOR, "FUN: %s",name); }

//
// Debug-levels, functions and macros
//

#define DBLEV1  1       // mapper's high level functions
#define DBLEV2  2       // build/check/adjust loops
#define DBLEV3  4       // mapper's refined loops
#define DBLEV4  8       // Loop start/end, epsilons, KF's, equations
#define DBLEV5  16      // Stencil info
#define DBLEV6  32      // Labels, conv.check info
#define DBLEV7  64      // context data
#define DBLEV8  128     // context data
#define DBLEV9  256     // dump ESM context
#define DBLEV10 512     // coupled stencils / getset
#define DBLEV11 1024    // print mapper cell loops
#define DBLEV12 2048    // print mapper layer loops

#define DBLEVMY 4096    // whatever programmer wants (level 13)

#if DO_DEBUG
#undef ON_DEBUG

#define ON_DEBUG(level,code)    if( GetDebugLevel(level) )  \
                    { code ; }
#else
#define ON_DEBUG(level,code)
#endif


//
// Enumerations for logging
//

typedef enum {
    Log_Context= 1,      // same numbers as in hymap-debug file
    Log_Mapper = 2,
    Log_Gaws   = 3,
    NR_LOG_OBJECT_TYPES
    } LogObjectType;


//
// functions for reading/returning debuglevels
//

void ReadDebugLevel(void);

bool DoLoggingFor(
    LogObjectType   objectType  // Context/Mapper/Gaws
    );

bool DoLoggingToFileFor(
    LogObjectType   objectType  // Context/Mapper/Gaws
    );

bool GetDebugLevel(
    int     callerLevel // debug-level of calling function
    );

bool GetFuncPrintLevel(
    int     callerLevel // print-level of calling function
    );

//
// functions for logging output
//

void MapLog(
    char      * format,     /* I: 'fprintf-format' for print of log */
    ...             /* I: arguments of log message      */
    );
