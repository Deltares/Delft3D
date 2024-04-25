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
// $Id: varinfocoll.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/varinfocoll.cpp $
//------------------------------------------------------------------------------
//  Class: Var Info Collection
//  Collection with information on Variables that have
//  to be communicated beteen Mapper<->Flow
//
//  Stef.Hummel@deltares.nl
//  Menno.Genseberger@deltares.nl
//  Erik.deGoede@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define MAX_NUM_GROUP_VARS 100      // Max num vars in group
#define VAR_TO_BE_PRINTED(varId)    ( varId >= 7 && varId <= 9 )  // for number see context.h


//////////////////////////////////////////////////////////////////////
// Public functions
//


VarInfoCollection::VarInfoCollection(
    int maxBlockInfo,
    int maxVarInfo,
    int maxVarGroup
    )
{
    m_BlockInfo = new BlockInfo[maxBlockInfo];
    m_VarInfo   = new VarInfo  [maxVarInfo];
    m_VarGroup  = new VarGroup [maxVarGroup] ;

    m_MaxBlockInfo = maxBlockInfo;
    m_MaxVarInfo   = maxVarInfo;
    m_MaxVarGroup  = maxVarGroup;

    m_NumBlockInfo = 0;
    m_NumVarInfo   = 0;
    m_NumVarGroup  = 0;
}


VarInfoCollection::~VarInfoCollection(void)
{
    delete [] m_BlockInfo;

    delete [] m_VarInfo;

    this->DeleteVarGroups();
    delete [] m_VarGroup;
}


BlockInfo * VarInfoCollection::AddBlockInfo(void)
{
    BlockInfo * newBlockInfo = NULL;

    if ( m_NumBlockInfo < m_MaxBlockInfo )
    {
        newBlockInfo = &(m_BlockInfo[m_NumBlockInfo]);

        InitBlockInfo(newBlockInfo);

        newBlockInfo->blockDim = Block_0D;

        m_NumBlockInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddBlockInfo(void): max #BlockInfo's exceeded\n");
    }


    return newBlockInfo;

}


BlockInfo * VarInfoCollection::AddBlockInfo(
    int offset,
    int size,
    int amount_D1,
    int stride_D1
    )
{
    BlockInfo * newBlockInfo = NULL;

    if ( m_NumBlockInfo < m_MaxBlockInfo )
    {
        newBlockInfo = &(m_BlockInfo[m_NumBlockInfo]);

        InitBlockInfo(newBlockInfo);

        newBlockInfo->blockDim = Block_1D;
        newBlockInfo->Offset = offset;
        newBlockInfo->Size = size;
        newBlockInfo->Amount[Block_1D] = amount_D1;
        newBlockInfo->Stride[Block_1D] = stride_D1;

        m_NumBlockInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddBlockInfo(1D): max #BlockInfo's exceeded\n");
    }


    return newBlockInfo;

}


BlockInfo * VarInfoCollection::AddBlockInfo(
    int offset,
    int size,
    int amount_D1,
    int stride_D1,
    int amount_D2,
    int stride_D2
    )
{
    BlockInfo * newBlockInfo = NULL;

    if ( m_NumBlockInfo < m_MaxBlockInfo )
    {
        newBlockInfo = &(m_BlockInfo[m_NumBlockInfo]);

        InitBlockInfo(newBlockInfo);

        newBlockInfo->blockDim = Block_2D;
        newBlockInfo->Offset = offset;
        newBlockInfo->Size = size;
        newBlockInfo->Amount[Block_1D] = amount_D1;
        newBlockInfo->Stride[Block_1D] = stride_D1;
        newBlockInfo->Amount[Block_2D] = amount_D2;
        newBlockInfo->Stride[Block_2D] = stride_D2;

        m_NumBlockInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddBlockInfo(2D): max #BlockInfo's exceeded\n");
    }

    return newBlockInfo;

}


BlockInfo * VarInfoCollection::AddBlockInfo(
    int offset,
    int size,
    int amount_D1,
    int stride_D1,
    int amount_D2,
    int stride_D2,
    int amount_D3,
    int stride_D3
    )
{
    BlockInfo * newBlockInfo = NULL;

    if ( m_NumBlockInfo < m_MaxBlockInfo )
    {
        newBlockInfo = &(m_BlockInfo[m_NumBlockInfo]);

        InitBlockInfo(newBlockInfo);

        newBlockInfo->blockDim = Block_3D;
        newBlockInfo->Offset = offset;
        newBlockInfo->Size = size;
        newBlockInfo->Amount[Block_1D] = amount_D1;
        newBlockInfo->Stride[Block_1D] = stride_D1;
        newBlockInfo->Amount[Block_2D] = amount_D2;
        newBlockInfo->Stride[Block_2D] = stride_D2;
        newBlockInfo->Amount[Block_3D] = amount_D3;
        newBlockInfo->Stride[Block_3D] = stride_D3;

        m_NumBlockInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddBlockInfo(3D): max #BlockInfo's exceeded\n");
    }


    return newBlockInfo;

}


BlockInfo * VarInfoCollection::AddBlockInfo(
    int offset,
    int size,
    int amount_D1,
    int stride_D1,
    int amount_D2,
    int stride_D2,
    int amount_D3,
    int stride_D3,
    int amount_D4,
    int stride_D4
    )
{
    BlockInfo * newBlockInfo = NULL;

    if ( m_NumBlockInfo < m_MaxBlockInfo )
    {
        newBlockInfo = &(m_BlockInfo[m_NumBlockInfo]);

        InitBlockInfo(newBlockInfo);

        newBlockInfo->blockDim = Block_4D;
        newBlockInfo->Offset = offset;
        newBlockInfo->Size = size;
        newBlockInfo->Amount[Block_1D] = amount_D1;
        newBlockInfo->Stride[Block_1D] = stride_D1;
        newBlockInfo->Amount[Block_2D] = amount_D2;
        newBlockInfo->Stride[Block_2D] = stride_D2;
        newBlockInfo->Amount[Block_3D] = amount_D3;
        newBlockInfo->Stride[Block_3D] = stride_D3;
        newBlockInfo->Amount[Block_4D] = amount_D4;
        newBlockInfo->Stride[Block_4D] = stride_D4;

        m_NumBlockInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddBlockInfo(4D): max #BlockInfo's exceeded\n");
    }

    return newBlockInfo;
}


void VarInfoCollection::AddVarInfo(
    int         varId,
    int        *varAddress,
    VarType     varType,
    BlockInfo  *blockInfo
    )
{
    VarInfo * newVarInfo = NULL;

    if ( m_NumVarInfo < m_MaxVarInfo )
    {
        newVarInfo = &(m_VarInfo[m_NumVarInfo]);

        InitVarInfo(newVarInfo);

        newVarInfo->varId      = varId;
        newVarInfo->varAddress = (void *) varAddress;
        newVarInfo->varType    = varType;
        newVarInfo->blockInfo  = blockInfo;

        ON_LOG_EXCHANGE(
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "m_NumVarInfo=%d, varId=%d", m_NumVarInfo, varId);
        )

        m_NumVarInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddVarInfo(int): max #VarInfo's exceeded\n");
    }
}


void VarInfoCollection::AddVarInfo(
    int         varId,
    float      *varAddress,
    VarType     varType,
    BlockInfo  *blockInfo
    )
{
    VarInfo * newVarInfo = NULL;

    if ( m_NumVarInfo < m_MaxVarInfo )
    {
        newVarInfo = &(m_VarInfo[m_NumVarInfo]);
        InitVarInfo(newVarInfo);

        newVarInfo->varId      = varId;
        newVarInfo->varAddress = (void *) varAddress;
        newVarInfo->varType    = varType;
        newVarInfo->blockInfo  = blockInfo;

        ON_LOG_EXCHANGE(
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "m_NumVarInfo=%d, varId=%d", m_NumVarInfo, varId);
        )

        m_NumVarInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddVarInfo(float): max #VarInfo's exceeded\n");
    }
}


void VarInfoCollection::AddVarInfo(
    int         varId,
    double     *varAddress,
    VarType     varType,
    BlockInfo  *blockInfo
    )
{
    VarInfo * newVarInfo = NULL;

    if ( m_NumVarInfo < m_MaxVarInfo )
    {
        newVarInfo = &(m_VarInfo[m_NumVarInfo]);
        InitVarInfo(newVarInfo);

        newVarInfo->varId      = varId;
        newVarInfo->varAddress = (void *) varAddress;
        newVarInfo->varType    = varType;
        newVarInfo->blockInfo  = blockInfo;

        ON_LOG_EXCHANGE(
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "m_NumVarInfo=%d, varId=%d", m_NumVarInfo, varId);
        )

        m_NumVarInfo++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddVarInfo(double): max #VarInfo's exceeded\n");
    }
}


void VarInfoCollection::AddGroup(
    int groupId    // group be added
    )
{
    VarGroup * newGroup = NULL;

    if ( m_NumVarGroup < m_MaxVarGroup )
    {
        newGroup = &(m_VarGroup[m_NumVarGroup]);
        InitVarGroup(newGroup);
        newGroup->numVars=0;
        newGroup->groupId = groupId;

        m_NumVarGroup++;
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::AddGroup: max #VarGroups exceeded\n");
    }
}


int VarInfoCollection::GetNumBytes(
    int groupId    // group for which #bytes must be determined
    )
{
    VarGroup * varGroup = FindGroup(groupId);
    int numBytes = -1;

    if ( varGroup == NULL )
    {
        throw new Exception (true, "VarInfoCollection::GetNumBytes: group %d not found\n", groupId);
    }
    else
    {
        if ( varGroup->numBytes == -1 )
        {
            varGroup->numBytes = this->BufferGroup(
                            groupId, BufferAction_GetSize, NULL, 0);
        }
        numBytes = varGroup->numBytes;
    }
    return numBytes;
}


int VarInfoCollection::GetMaxNumBytes(void)
{
    int maxNumBytes=0;

    for (int groupIndex = 0; groupIndex < this->m_NumVarGroup ; groupIndex++)
    {
        int numBytes = this->GetNumBytes(m_VarGroup[groupIndex].groupId);

        if ( numBytes > maxNumBytes ) maxNumBytes = numBytes;
    }

    return maxNumBytes;
}


void VarInfoCollection::AddVarToGroup(
    int groupId,   // group to add var.to
    int varId      // var to be added
    )
{
    VarInfo  * varInfo = FindVar(varId);

    if ( varInfo == NULL )
    {
        throw new Exception (true, "VarInfoCollection::AddVarToGroup: var %d not found\n", groupId);
    }
    else
    {
        VarGroup * varGroup = FindGroup(groupId);

        if ( varGroup == NULL )
        {
            throw new Exception (true, "VarInfoCollection::AddVarToGroup: group %d not found\n", groupId);
        }
        else
        {
            if ( varGroup->numVars >= MAX_NUM_GROUP_VARS )
            {
                throw new Exception (true, "VarInfoCollection::AddVarToGroup: max #vars in group %d exceeded\n",
                        groupId);
            }
            else
            {
                varGroup->varId[varGroup->numVars] = varId;
                varGroup->numVars++;
            }
        }
    }
}


VarInfo * VarInfoCollection::FindVar(
    int varId            // var to be found
    )
{
    VarInfo * retVal = NULL;
    int varIndex;

    for (varIndex = 0; ( varIndex < this->m_NumVarInfo ) && ( retVal == NULL ) ; varIndex++)
    {
        if (m_VarInfo[varIndex].varId == varId)
        {
            retVal = &(m_VarInfo[varIndex]);
        }
    }
    return retVal;
}




//
//  Fill or Read Vars into buffer
//

int VarInfoCollection::BufferVar(   // return: #bytes added or read
    int             varId,          // var to be processed
    BufferAction    bufferAction,   // Fill or Read buffer
    char          * buffer,         // buffer array
    int             maxBytes        // max #num bytes that can be added.
    )
{
    int nAddedOrReadBytes  = 0;   // return value: #added bytes

    VarInfo *varInfo = this->FindVar(varId); // var.info for varId

    if ( varInfo == NULL )
    {
        //
        // Var not communicated
        //
        return 0;
    }

    BlockInfo *blockInfo = varInfo->blockInfo; // block.admin. info for var.
    BlockDim BlockDim = blockInfo->blockDim;   // block dimenstion for var

    int nBytesInVarType;                       // #bytes in double/float/int.
    switch( varInfo->varType )
    {
        case  IntegerType:
            nBytesInVarType = sizeof(int);
            break;
        case  FloatType:
            nBytesInVarType = sizeof(float);
            break;
        case  DoubleType:
            nBytesInVarType = sizeof(double);
            break;
        default:
            throw new Exception (true, "VarInfoCollection::BufferVar: varType not recognized.\n");
            break;
    }

    int mOffset, nOffset, kOffset, sOffset;    // Offset in 4 dir.s (M/N/K/LStci)

    int mEnd   =1, nEnd   =1, kEnd   =1, sEnd   =1; // End of copy loop (4 dir.s)
    int kStride=0, sStride=0, mStride=0, nStride=0; // Stride of copy loop (4 dir.s)
    int m, n, k, p;                                 // copy loop counters

    if (BlockDim > Block_3D)
    {
        sEnd = blockInfo->Amount[Block_4D];
        sStride = blockInfo->Stride[Block_4D];
    }

    if (BlockDim > Block_2D)
    {
        kEnd = blockInfo->Amount[Block_3D];
        kStride = blockInfo->Stride[Block_3D];
    }

    if (BlockDim > Block_1D)
    {
        mEnd = blockInfo->Amount[Block_2D];
        mStride = blockInfo->Stride[Block_2D];
    }

    if (BlockDim > Block_0D)
    {
        nEnd = blockInfo->Amount[Block_1D];
        nStride = blockInfo->Stride[Block_1D];
    }

    sOffset = blockInfo->Offset;

    for (p = 0; p < sEnd; p++)
    {
        kOffset = sOffset + p * sStride;

        for (k = 0; k < kEnd; k++)
        {
            mOffset = kOffset + k * kStride;

            for (m = 0; m < mEnd; m++)
            {
                nOffset = mOffset + m * mStride;

                for (n = 0; n < nEnd; n++)
                {
                    int index = nOffset + n * nStride; // total index to be copied.

                    void * varAddress;
                    switch( varInfo->varType )
                    {
                    case  IntegerType:
                        varAddress = (void *) (((int *) varInfo->varAddress + index));
                        break;
                    case  FloatType:
                        varAddress = (void *) (((float *) varInfo->varAddress + index));
                        break;
                    case  DoubleType:
                        varAddress = (void *) (((double *) varInfo->varAddress + index));
                        break;
                    default:
                        // test already done
                        break;
                    }

                    if ( bufferAction == BufferAction_GetSize )
                    {
                        nAddedOrReadBytes += nBytesInVarType;
                    }
                    else
                    {
                        if ( ( nAddedOrReadBytes + nBytesInVarType ) <= maxBytes )
                        {
                            if ( bufferAction == BufferAction_Fill )
                            {
                                memcpy( (void * ) (buffer + nAddedOrReadBytes), varAddress, nBytesInVarType);
                                nAddedOrReadBytes += nBytesInVarType;
                            }
                            else if ( bufferAction == BufferAction_Read )
                            {
                                memcpy( varAddress, (void * ) (buffer + nAddedOrReadBytes), nBytesInVarType);
                                nAddedOrReadBytes += nBytesInVarType;
                            }

                            ON_LOG_EXCHANGE(
                                const char * actionStr = (bufferAction == BufferAction_Fill) ? "Fill" : "Read";
                                if ( VAR_TO_BE_PRINTED(varId) )
                                {
                                    if (varInfo->varType == FloatType)
                                    {
                                        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, " %s idx%5d, ofs=%5d, m%3d, n%3d (vId%3d, %12.8f)",
                                                actionStr, index, blockInfo->Offset, m, n,
                                                varId, *((float *) varAddress) );
                                    }
                                    else if (varInfo->varType == DoubleType)
                                    {
                                        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, " %s idx%5d, ofs=%5d, m%3d, n%3d (vId%3d, %12.8g)",
                                                actionStr, index, blockInfo->Offset, m, n,
                                                varId, *((double   *) varAddress) );
                                    }
                                    else if (varInfo->varType == IntegerType)
                                    {
                                        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, " %s idx%5d, ofs=%5d, m%3d, n%3d (vId%3d, %12d)",
                                                actionStr, index, blockInfo->Offset, m, n,
                                                varId, *((int   *) varAddress) );
                                    }
                                    fflush(stdout);
                                }
                            )

                        }
                        else
                        {
                            throw new Exception (true, "VarInfoCollection::BufferVar: maxBytes exceed\n");
                        }
                    }
                }
            }
        }
    }

    return nAddedOrReadBytes;
}


//
//  Fill or Read Vars into buffer
//

int VarInfoCollection::BufferGroup(   // return: #bytes added or read
    int             groupId,        // group to be processed
    BufferAction    bufferAction,   // Fill or Read buffer
    char          * buffer,         // buffer array
    int             maxBytes        // max #num bytes that can be added.
    )
{
    int nAddedOrReadBytes = 0;      // #bytes added to buffer


    VarGroup * varGroup = FindGroup(groupId);
    if ( varGroup != NULL )
    {
        //
        // loop over all vars in group
        //

        for ( int v = 0 ; v < varGroup->numVars ; v++ )
        {
            nAddedOrReadBytes += this->BufferVar(varGroup->varId[v],
                                                bufferAction,
                                                buffer + nAddedOrReadBytes,
                                                maxBytes - nAddedOrReadBytes
                                                );
            ON_LOG_EXCHANGE(
                if ( VAR_TO_BE_PRINTED(varGroup->varId[v]) )
                {
                    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, " Processed var: %d, #bytes %d",
                        varGroup->varId[v], nAddedOrReadBytes );
                }
            )
        }
        // TODORE: check maxBytes == nAddedOrReadBytes (maxByte->numExpectedBtes)
    }
    else
    {
        throw new Exception (true, "VarInfoCollection::BufferGroup: varGroup %d not found\n", groupId);
    }

    return nAddedOrReadBytes;
}


void VarInfoCollection::PrintVarInfo(
    int varId            // var to be printed
    )
{
    VarInfo * varInfo = FindVar(varId);
    if ( varInfo != NULL )
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "VarInfo: varId=%d varAddress=0x%x varType=%d blockInfo=%x", varInfo->varId, varInfo->varAddress, varInfo->varType, varInfo->blockInfo);
}


//////////////////////
// Private functions
//


void VarInfoCollection::InitBlockInfo(
    BlockInfo * blockInfo // block admin. info to be inited
    )
{
    int dim;
    blockInfo->blockDim = Block_Unknown;
    blockInfo->Offset = 0;
    blockInfo->Size = 0;
    for (dim = Block_1D; dim < NUM_BLOCK_TYPES; dim++)
    {
        blockInfo->Amount[dim] = 0;
        blockInfo->Stride[dim] = 0;
    }
}


void VarInfoCollection::InitVarInfo(
    VarInfo * varInfo   // var-info to be inited
    )
{
    varInfo->varId = -1;
    varInfo->varAddress = NULL;
    varInfo->varType = Type_Unknown;
    varInfo->blockInfo  = NULL;
}


void VarInfoCollection::InitVarGroup(
    VarGroup * varGroup   // group to be inited
    )
{
    varGroup->groupId  = -1;
    varGroup->varId    = new int[MAX_NUM_GROUP_VARS];
    varGroup->numVars  = 0;
    varGroup->numBytes = -1;
}


void VarInfoCollection::DeleteVarGroups(void)
{
    int g;
    for ( g = 0 ; g < this->m_NumVarGroup ; g++ )
    {
        if (this->m_VarGroup[g].varId) delete [] this->m_VarGroup[g].varId;
    }
}


VarGroup * VarInfoCollection::FindGroup(
    int groupId            // group to be found
    )
{
    VarGroup * retVal = NULL;

    for (int groupIndex = 0; ( groupIndex < this->m_NumVarGroup ) && ( retVal == NULL ) ; groupIndex++)
    {
        if (m_VarGroup[groupIndex].groupId == groupId)
        {
            retVal = &(m_VarGroup[groupIndex]);
        }
    }
    return retVal;
}


void VarInfoCollection::PrintGroupInfo(
    int groupId            // group to be printed
    )
{
    VarGroup * varGroup = this->FindGroup(groupId);

    if ( varGroup != 0 )
    {
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "VarInfoColl. Group id = %d", varGroup->groupId);
        for (int varIndex = 0; varIndex < varGroup->numVars ; varIndex++)
        {
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "               Var id = %d", varGroup->varId[varIndex]);
        }
    }

}
