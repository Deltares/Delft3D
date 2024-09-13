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
// $Id: flow_in_hydra.h 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/flow_in_hydra.h $
//------------------------------------------------------------------------------
//  Flow2D3D mapper definitions
//
//  Erik.deGoede@deltares.nl
//  Adri.Mourits@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once


#include "precision.h"


//
//  Flow / Gaws Messages
//


// TODORE: make arrays for these sizes dynamic
#define MAX_NUM_MAPPERS 100
#define MAX_NUM_D3D_FLOW_PROCS 25



//
// Flow DD blobs:
//


typedef struct {
    unsigned int value;
    } BarrierMessage;


//
// Enumeration for memory types
//
typedef enum {
    Mem_Unknown,            // NOT SET
    Mem_Shared,             // In process
    Mem_Distributed,        // Distributed
    NUM_MEM_TYPES
} MemType;


//
// Define enumeration for two contexts in a Mapper
//
typedef enum
{
   C_0,         // first context
   C_1,         // second context
   NR_CNTXTS    // number of contexts (use for declatrions)
}  Cntxt;


typedef struct {
    int     mMax  ;
    int     nMax  ;
    int     nMaxus;
    int     kMax  ;
    int     lStsci;
    int     lTur  ;
    int     lSedtt;
    int     Zmodel;
    int     Roller;
    int     dDb   ;
    int     mMaxdb;
    int     nMaxdb;
    REAL_FP Hdt   ;
} Flow2MapperSizesFlags;


typedef struct {
    int     mMax  ;
    int     nMax  ;
    int     nMaxus;
    int     kMax  ;
    int     dDb   ;
    int     mMaxdb;
    int     nMaxdb;
    REAL_FP Hdt   ;
} Flow2GawsSizes;


const int startEndSize = 3;
typedef struct {
    int edgeType     ;
    int mStart[startEndSize];
    int nStart[startEndSize];
    int mEnd  [startEndSize];
    int nEnd  [startEndSize];
    int mStartMin    ;
    int nStartMin    ;
    int mStripSize   ;
    int nStripSize   ;
} Mapper2FlowMapperInfo;


//
//  DD Messages (flow / mapper)
//


typedef struct DDMesg_STR {
    int         value;
} DDMesg;


typedef struct UpdateHeader_STR{ // TODORE: remove _STR
    int         nextStep     ;   // Step to be performed by receiver
    int         distribGroup ;   // Group to be communicated
    int         numMessages  ;   // #messages for this step (==0 or 1)
    int         intValue     ;   // message value if #messages == 1
} UpdateHeader;


#define STEP_UNDEF (-99)   // undefined (next-)step


//
//  Flow / Gaws Messages
//

enum { FinishGaws = -1 };

typedef struct {
    int     intValue;
} GawsDirectionMesg;


typedef struct {
    int     result;  // 0: OK, /= 0: error
} GawsOKMesg;



//
// Enumeration for Identification of Exchanged Blobs
//
typedef enum {

F2M_Blob_ContextID          = 102000,
F2M_Blob_SubdomainSizesFlags= 102002,

M2F_Blob_InfoOnMapperStrips = 201001,

F2M_Blob_MapperStep         = 102011,
F2M_Blob_NumMessages        = 102012,
F2M_Blob_Messages           = 102013,
F2M_Blob_Update             = 102014,

M2F_Blob_FlowStep           = 201011,
M2F_Blob_NumMessages        = 201012,
M2F_Blob_Messages           = 201013,
M2F_Blob_Update             = 201014,

F2G_Blob_ContextID          = 103000,
F2G_Blob_SubdomainSizes     = 103001,
F2G_Blob_KCS_Array          = 103002,

G2F_Blob_NumCommPoints      = 301001,
G2F_Blob_CommPoints         = 301002,

F2G_Blob_Left2RightMessage  = 103011,
F2G_Blob_Update             = 103012,

G2F_Blob_OKMessage          = 301011,
G2F_Blob_Update             = 301012,

NUM_FLOW_DD_MESSAGES

} DDBlobID;
