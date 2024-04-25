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
// $Id: varinfocoll.h 883 2011-10-07 16:32:16Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/varinfocoll.h $
//------------------------------------------------------------------------------
//  Class for collection with information on Variables that have
//  to be communicated beteen Mapper<->Flow
//
//  Stef.Hummel@Deltares.NL
//  Menno.Genseberger@deltares.nl
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once



// For Debugging:

#define LOG_EXCHANGE    0


#if(LOG_EXCHANGE)
#define ON_LOG_EXCHANGE(code)   code
#else
#define ON_LOG_EXCHANGE(code)
#endif


//
// Enumeration for variable types
//
typedef enum{
    Type_Unknown = -1,
    FloatType,
    DoubleType,
    IntegerType,
    NUM_VAR_TYPES
} VarType;


//
// Enumeration for dimension types that are distinguished when communicating
// data blocks.
//
typedef enum{
    Block_Unknown = -1,
    Block_0D,
    Block_1D,
    Block_2D,
    Block_3D,
    Block_4D,
    NUM_BLOCK_TYPES
} BlockDim;


//
// Enumeration for action on filling or reading a buffer
//
typedef enum{
    BufferAction_Fill,
    BufferAction_Read,
    BufferAction_GetSize,
    NUM_BUFFER_ACTION_TYPES
} BufferAction;


//
// Administration on a communicated data block.
//
typedef struct BlockInfo_STR{
    BlockDim     blockDim;
    int          Offset;
    int          Size;
    int          Amount[NUM_BLOCK_TYPES];
    int          Stride[NUM_BLOCK_TYPES];
} BlockInfo;


//
// Administration on a communicated var.
//
typedef struct VarInfo_STR{
    int          varId;         // identification
    void        *varAddress;    // var.'s start adress in mem
    VarType      varType;       // type (double|float|int)
    BlockInfo   *blockInfo;     // data block admin for this var.
} VarInfo;


//
// Administration on a communicated group
//
typedef struct {
    int   groupId;         // identification
    int * varId;           // array of var id's
    int   numVars;         // #vars in this group
    int   numBytes;        // #bytes to be communicated for this group
} VarGroup;


//
//  VarInfoCollection Class
//


class VarInfoCollection
{
    ////////////////////////
    //
    // PUBLIC FUNCTIONS
    //

    public:

    //
    // Create / Destroy
    //

    VarInfoCollection(
        int maxBlockInfo,  // max #blockAdmin.s to be stored
        int maxVarInfo,    // max #var.s to be stored
        int maxVarGroup    // max #groups to be stored
        );

    ~VarInfoCollection(void);

    //
    // Add block administration ( for 1D, 2D, etc.)
    //

    BlockInfo * AddBlockInfo(void);

    BlockInfo * AddBlockInfo(
        int offset,
        int size,
        int amount_D1,
        int stride_D1);

    BlockInfo * AddBlockInfo(
        int offset,
        int size,
        int amount_D1,
        int stride_D1,
        int amount_D2,
        int stride_D2);

    BlockInfo * AddBlockInfo(
        int offset,
        int size,
        int amount_D1,
        int stride_D1,
        int amount_D2,
        int stride_D2,
        int amount_D3,
        int stride_D3);

    BlockInfo * AddBlockInfo(
        int offset,
        int size,
        int amount_D1,
        int stride_D1,
        int amount_D2,
        int stride_D2,
        int amount_D3,
        int stride_D3,
        int amount_D4,
        int stride_D4);

    //
    // Add var. administration ( for int, float )
    //

    void AddVarInfo(
        int varId,
        int *varAddress,
        VarType varType,
        BlockInfo * blockInfo);

    void AddVarInfo(
        int varId,
        float *varAddress,
        VarType varType,
        BlockInfo * blockInfo);

    void AddVarInfo(
        int varId,
        double *varAddress,
        VarType varType,
        BlockInfo * blockInfo);


    void AddGroup(
        int groupId    // group be added
        );


    void AddVarToGroup(
        int groupId,   // group to add var.to
        int varId      // var to be added
        );


    int GetNumBytes(
        int groupId    // group for which #bytes must be determined
        );

    int GetMaxNumBytes(void);


    //
    // For distributed by COPY
    //

    void CopyVar(
        int                varId,              // identification
        VarInfoCollection *m_OppositeVarInfo   // other var info collection
        );

    int BufferVar( // return: #bytes added or read
        int             varId,    // var to be added
        BufferAction    action,   // ( fill | read | getsize )
        char          * buffer,   // buffer array
        int             maxBytes  // max #num bytes that can be added.
        );

    int BufferGroup( // return: #bytes added or read
        int             groupId,  // GroupID
        BufferAction    action,   // ( fill | read | getsize )
        char          * buffer,   // buffer array
        int             maxBytes  // max #num bytes that can be added.
        );

    //
    // Find a var info
    //
    VarInfo * FindVar(
        int varId               // var to be found
        );


    //
    // Find a var. group
    //
    VarGroup * FindGroup(
        int groupId             // group to be found
        );


    //
    // Print info on a var
    //
    void PrintVarInfo(
        int varId               // var to be printed
        );


    //
    // Print info on a var.-group
    //
    void PrintGroupInfo(
        int groupId             // group to be printed
        );


    ////////////////////////
    //
    // PRIVATE DATA
    //

    private:


    BlockInfo * m_BlockInfo;    // Data block admininstration
    int         m_MaxBlockInfo; // max #blocks
    int         m_NumBlockInfo; // actual #blocks


    VarInfo   * m_VarInfo;      // communicated var administration
    int         m_MaxVarInfo;   // max #vars
    int         m_NumVarInfo;   // actual #blocks


    VarGroup  * m_VarGroup;     // communicated groups
    int         m_MaxVarGroup;  // max #groups
    int         m_NumVarGroup;  // #communicated groups


    ////////////////////////
    //
    // PRIVATE FUNCTIONS
    //

    //
    // Initialize data
    //
    void InitBlockInfo( BlockInfo * blockInfo );
    void InitVarInfo( VarInfo * varInfo );
    void InitVarGroup( VarGroup * varGroup );
    void DeleteVarGroups(void);

};


