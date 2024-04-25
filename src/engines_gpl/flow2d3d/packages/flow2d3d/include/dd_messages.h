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
// $Id: dd_messages.h 883 2011-10-07 16:32:16Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/dd_messages.h $
//------------------------------------------------------------------------------
//  Flow2D3D Mapper definitions
//
//  Stef.Hummel@Deltares.NL
//  Adri.Mourits@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#define MAX_NR_DD_MESS  100   /* max # of messages for an object */
#define HY_MESS_TO_ALL  (-1)        /* special object ID for message */


typedef int ObjID;


/*
 *  Message Types
 */

typedef enum {
    HyMesg_Undefined,
    HyMesgFrom_DDproces,
    HyMesgFrom_DDmapper,
    HyMesg_TS_Done,     /* mapper to exec: timestep done */
    HyMesg_Sim_Done     /* exec to mapper: simulation done */
    }  HyMesgType;



/*
 *  Application (process) specific message body formats
 *  DELFT3D-FLOW (process. and mapper) messages.
 *  For the time being, each application uses a catch-all default.
 */

#define MAX_INT_MESS    2   /* max # of integer values in a message */
#define HY_DEFAULT_MESG_BODY \
    int     intval   [MAX_INT_MESS];


typedef struct D3dFlowMess_STR {    /* DELFT3D-FLOW */
    HY_DEFAULT_MESG_BODY
    }   D3dFlowMess;

typedef struct D3dWaqMess_STR {     /* DELFT3D-WAQ */
    HY_DEFAULT_MESG_BODY
    }   D3dWaqMess;


/*
 *  Hydra Message Structure
 *  Each process of mapper type can declare its own
 *  data-structure for message bodies, and incorporate it in the
 *  union.
 */

typedef struct HyMesg_STR {
    ObjID   objectId;       /* sender/recipient of the message */
    HyMesgType  type;
    union {             /* message contents */
    D3dFlowMess d3dFlow;
    D3dWaqMess  d3dWaq;
    } mesg;
    }   HyMesg;

