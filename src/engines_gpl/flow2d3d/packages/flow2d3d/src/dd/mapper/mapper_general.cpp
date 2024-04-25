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
// $Id: mapper_general.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/mapper_general.cpp $
//------------------------------------------------------------------------------
//  Class: D3dFlowMapper
//  Mapper for DELFT3D-FLOW Domain Decomposition
//
//  Stef.Hummel@deltares.nl
//  Erik.deGoede@deltares.nl
//  Adri.Mourits@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define LOG_INIT    0


//////////////////////////////////////////////////////////////////////
//
// D3dFlowMapper
//
// Mapper for DELFT3D-FLOW Domain Decomposition
//


//////////////////////
// Public functions
//


D3dFlowMapper::D3dFlowMapper(void)
{
    //
    // Create message object
    // Read debug information, and init this mappers logfile-handle
    //

    mapMess = new MapMess;

    ReadDebugLevel();

    MAPDBG_FUN("D3dFlowMapper::D3dFlowMapper");

    logFile  = NULL;
}


D3dFlowMapper::~D3dFlowMapper(void)
{
    int     ctx;           // context loop counter

    MAPDBG_FUN("D3dFlowMapper::~D3dFlowMapper");

    delete mapMess;
    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx ++ )
    {
        delete C[ctx];
    }
}


int D3dFlowMapper::Setup(
    Iterator * mapper,                // current mapper
    char     * configString,          // configuration string
    Iterator * neighbors[NR_CNTXTS],  // neighbor flow processes
    MemType    memType[NR_CNTXTS]     // SharedMem or Distributed dd-data
    )
{
    int retVal = HY_OK;         // return value
    int ctx, oCtx;              // current and other context index
    int eq;                     // equation loop counter


    MAPDBG_FUN("Mapper::Setup");

    strcpy(this->confFile, configString);

    //
    // read config file
    //

    retVal = InitAndParseConfigString(configString);

    if ( retVal != HY_ERR )
    {
        //
        // Initialize Mapper Loops
        //

        for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        {
            oCtx = 1 - ctx;
            for ( eq = 0 ; eq < NR_EQ ; eq++ )
            {
                if (this->Edge[ctx] == Edge_Left ||
                    this->Edge[ctx] == Edge_Right   )
                {
                    nStart   [ctx][eq] = GetStartCell(ctx , eq);
                    nEnd     [ctx][eq] = GetEndCell  (ctx , eq);
                    nOthStart[ctx][eq] = GetStartCell(oCtx, eq);
                    nOthEnd  [ctx][eq] = GetEndCell  (oCtx, eq);

                    mStart[ctx][eq] = mEnd[ctx][eq] =
                            GetNormalCell(ctx,  eq, CtxType_Target);
                    mOthStart[ctx][eq] = mOthEnd[ctx][eq] =
                            GetNormalCell(oCtx, eq, CtxType_Source);

                    l2r = 1;
                    b2t = 0;
                }
                else
                {
                    mStart   [ctx][eq] = GetStartCell(ctx , eq);
                    mEnd     [ctx][eq] = GetEndCell  (ctx , eq);
                    mOthStart[ctx][eq] = GetStartCell(oCtx, eq);
                    mOthEnd  [ctx][eq] = GetEndCell  (oCtx, eq);

                    nStart[ctx][eq] = nEnd[ctx][eq] =
                            GetNormalCell(ctx,  eq, CtxType_Target);
                    nOthStart[ctx][eq] = nOthEnd[ctx][eq] =
                            GetNormalCell(oCtx, eq, CtxType_Source);

                    l2r = 0;
                    b2t = 1;
                }
            }
        }
    }

    //
    // Initialize context objects and there data
    //
    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        C[ctx] = new D3dFlowContextMapSide();
        C[ctx]->Setup( mapper, neighbors[ctx],
                        memType[ctx], Edge[ctx],
                        mStart[ctx], nStart[ctx], mEnd[ctx], nEnd[ctx]);

        //
        // Receive Sizes / Flags
        // This must be done in the same loop as Setup
        // due to the blob order in case of 1 subdomain
        //

        if ( C[ctx]->memType == Mem_Distributed )
        {
            C[ctx]->ReceiveSizesAndFlagsFromFlow();
            C[ctx]->DetermineStripSize();
        }
    }

    //
    // Send info on mapper strips
    //

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        if ( C[ctx]->memType == Mem_Distributed )
        {
            C[ctx]->SendMapperInfoToFlow();
            C[ctx]->varInfoColl = new VarInfoCollection(NUM_BLOCK_INFO_S, NUM_VAR_IDS, NUM_MAP_DISTRIB_TYPES);
            C[ctx]->FillBlockAdmin();
            C[ctx]->CreateMapperStrips();
            C[ctx]->FillVarInfoCollection();
        }
    }

    //
    // Receive initial data
    //
    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        UpdateHeader updateHeader;
        updateHeader.nextStep    = STEP_UNDEF;
        updateHeader.distribGroup= -1;
        updateHeader.numMessages = -1;
        updateHeader.intValue    = -1;

        C[ctx]->UpdateMapperFromFlow(updateHeader);

        if ( updateHeader.numMessages != 0 )
        {
            throw new Exception (true, "D3dFlowMapper::Setup: # incoming messages must be zero");
        }

    }

    //
    // Let Contexts refer to each other, set refinement factors
    //
    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        oCtx = 1 - ctx;
        C[ctx]->SetOtherContext(C[oCtx]);
    }

#if LOG_INIT
    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        for ( eq = 0 ; eq < NR_EQ ; eq++ )
        {
            printf("ctx: %d, EQ: %d\n",ctx,eq);
            printf("OTH: %2d-%2d, %2d-%2d\n",
                mOthStart[ctx][eq] ,
                mOthEnd  [ctx][eq] ,
                nOthStart[ctx][eq] ,
                nOthEnd  [ctx][eq] );
            printf("Trg: %2d-%2d, %2d-%2d\n",
                mStart[ctx][eq] ,
                mEnd  [ctx][eq] ,
                nStart[ctx][eq] ,
                nEnd  [ctx][eq] );
        }
    }
#endif

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER \"%s\" Setup, retVal %d, Checking config", IteratorSelf()->name, retVal);

    retVal = this->CheckConfig();

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER \"%s\" Setup, Checked config, retVal %d", IteratorSelf()->name, retVal);

    return retVal;
}


int D3dFlowMapper::NextStep(
    int         myStep,     // I: Step caller just completed
    int         neighborStep,       // I: Step neighbor(s) should go to
    int         myNxtStep   // I: Next step
    )
{
    //
    // Next-Step called without Alternative for resulting NextStep.
    // Call Next-Step with STEP_UNDEF as alternative
    //

    return this->NextStep( myStep, neighborStep, myNxtStep, STEP_UNDEF);

}


int D3dFlowMapper::NextStep(
    int         myStep,     // I: Step caller just completed
    int         neighborStep,       // I: Step neighbor(s) should go to
    int         myNxtStep,  // I: Next step
    int         myNxtStepAlt    // I: Next step Alternative
    )
{
    //
    // Read outgoing messages from Mappers message store
    // Call Resume function
    // Check resulting step
    //

    DDMesg * outMess = NULL;   // Messages to neighbors
    int      outMesgValue;     // Value of messages to neighbors
    int      numOut=0;

    if ( ( outMess = mapMess->GetOutMess() ) != NULL )
    {
        outMesgValue = outMess->value;
        numOut=1;
    }
        else
        {
                outMesgValue = -1;
        }

    //
    // Send adjusted data and messages to flow side.
    // TODORE: Optimize groups (limited #vars per NextStep)
    //

    if ( myStep != STEP_UNDEF )
    {
        for ( int ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        {
            //
            // Send next neighbor step and messages to flow
            //

            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER NEXTSTEP to Flow \"%s\": myStep %s, neighborStep %s, numOut %d",
                                C[ctx]->flowIterator->name,
                                PrintNextStepName(myStep), PrintNextStepName(neighborStep), numOut);

            UpdateHeader updateHeader;
            updateHeader.nextStep    = neighborStep;
            updateHeader.distribGroup= DetermineDistribGroup(myStep);
            updateHeader.numMessages = numOut;
            updateHeader.intValue    = outMesgValue;

            C[ctx]->UpdateMapperToFlow(updateHeader);
        }
    }

    //
    // D3d flow Processes perform action, wait for result
    //

    DDMesg      inMess[NR_CNTXTS];
    int         totNumIn=0;
    int         nextStep = STEP_UNDEF;

    if ( myStep != STEP_UNDEF ) // TODO Select group on myStep
    {
        for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        {
            //
            // Receive next step and messages from flow
            //

            UpdateHeader updateHeader;
            updateHeader.nextStep    = STEP_UNDEF;
            updateHeader.distribGroup= -1;
            updateHeader.numMessages = -1;
            updateHeader.intValue    = -1;

            C[ctx]->UpdateMapperFromFlow(updateHeader);

            if ( updateHeader.numMessages < 0 )
            {
                throw new Exception (true, "D3dFlowMapper::NextStep: # incoming messages must >= 0");
            }

            nextStep = updateHeader.nextStep;

            if ( updateHeader.numMessages == 1 )
            {
                inMess[totNumIn].value = updateHeader.intValue;
                totNumIn++;
            }
        }
    }

    if ( nextStep == D3dFlowMap_Finish ) // TODORE: check if it can be removed
    {
        return nextStep;
    }

    if ( nextStep != myNxtStep )
    {
        if (    ( myNxtStepAlt == STEP_UNDEF   )
             || ( nextStep     != myNxtStepAlt ) )
        {
#if 0       // TODORE: CHECK
            throw new Exception (true, "Unexpected step (%d) after step %d; UNEXPECT %s after %s",
                            nextStep,
                            myStep,
                            PrintNextStepName(nextStep),
                            PrintNextStepName(myNxtStep) );
#endif
        }
    }

    //
    // Store incoming messages in Mappers message store
    //

    mapMess->InitInMess();

    if ( totNumIn > 0 )
    {
        if ( totNumIn != NR_CNTXTS )
        {
            throw new Exception (true, "D3dFlowMapper::NextStep: #inmessages inconsistent with #flow processes");
        }
        else
        {
            for ( int m = 0 ; m < totNumIn ; m++ )
            {
                mapMess->PutInMess(inMess[m]);
            }
        }
    }


    //
    // Reset outgoing messages before next step starts
    //

    mapMess->InitOutMess();

    return nextStep;

}


void D3dFlowMapper::FMapLog(
    char      * format,     // I: 'fprintf-format' for print of log
    ...                     // I: arguments of log message (should be
                            //  terminated with NULL)
    )
{
    char    name[100];  // filename
    va_list arguments;  // var-arg list

    if ( ! DoLoggingFor(Log_Mapper) )
    {
        return;
    }

    va_start ( arguments, format );

    if ( DoLoggingToFileFor(Log_Mapper) )
    {
        if (logFile == NULL)
        {
            sprintf(name, "mapuitvoer.%d", this->mapperObjID);
            logFile = fopen(name,"w");
            if (logFile == NULL)
            {
                throw new Exception (true, "Couldn't open logfile %s", name);
            }
        }
        if ( logFile != NULL )
        {
            vfprintf ( logFile, format, arguments );
            fflush(logFile);
        }
    }
    else
    {
    vprintf ( format, arguments );
    fflush(stdout);
    }

    va_end ( arguments );

}


void D3dFlowMapper::FMapLog2DIndex(
    char      * messStr,// message string
    int         ctx,    // context counter
    int         mIndex, // M index
    int         nIndex  // N index
    )
{
    if ( DoLoggingFor(Log_Mapper) )
    {
        FILE * logHandle = stdout;

        if ( DoLoggingToFileFor(Log_Mapper) ) logHandle = logFile;

        if ( logHandle != NULL )
        {
            printf("%s[%d]=%5d in %3d*%3d=%5d (m=%d + (mArrayOffset=%d)) * (nArraySize=%d) + (n=%d + (nArrayOffset==%d))\n",
                messStr, ctx,
                (mIndex + (C[ctx]->mArrayOffset)) * (C[ctx]->nArraySize) + (nIndex + (C[ctx]->nArrayOffset)),
                C[ctx]->mArraySize, C[ctx]->nArraySize, C[ctx]->mArraySize * C[ctx]->nArraySize,
                mIndex, C[ctx]->mArrayOffset , C[ctx]->nArraySize, nIndex, C[ctx]->nArrayOffset);

            fflush(logHandle);
        }

    }

}


void D3dFlowMapper::SendDataToFlow(
    int         myStep,       // I: Step caller just completed
    int         neighborStep  // I: Step neighbor(s) should go to
        )
{
    //
    // Read outgoing messages from Mappers message store
    // Call Resume function
    // Check resulting step
    //

    DDMesg * outMess = NULL;   // Messages to neighbors
    int      outMesgValue=0;   // Value of messages to neighbors
    int      numOut=0;

    if ( ( outMess = mapMess->GetOutMess() ) != NULL )
    {
        outMesgValue = outMess->value;
        numOut=1;
    }

    //
    // Send adjusted data and messages to flow side.
    // TODORE: Optimize groups (limited #vars per NextStep)
    //
    //printf("%20d <- mapper (%d)\n",myStep,neighborStep);
    if ( myStep != STEP_UNDEF )
    {
        for ( int ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        {
            //
            // Send next neighbor step and messages to flow
            //

            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER NEXTSTEP to Flow \"%s\": myStep %s, neighborStep %s, numOut %d",
                                C[ctx]->flowIterator->name,
                                PrintNextStepName(myStep), PrintNextStepName(neighborStep), numOut);

            UpdateHeader updateHeader;
            updateHeader.nextStep    = neighborStep;
            updateHeader.distribGroup= DetermineDistribGroup(myStep);
            updateHeader.numMessages = numOut;
            updateHeader.intValue    = outMesgValue;

            C[ctx]->UpdateMapperToFlow(updateHeader);
        }
    }
}

int D3dFlowMapper::ReceiveDataFromFlow(void)
{
    //
    // D3d flow Processes perform action, wait for result
    //

    DDMesg      inMess[NR_CNTXTS];
    int         totNumIn=0;
    int         nextStep = STEP_UNDEF;

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        //
        // Receive next step and messages from flow
        //

        UpdateHeader updateHeader;
        updateHeader.nextStep    = STEP_UNDEF;
        updateHeader.distribGroup= -1;
        updateHeader.numMessages = -1;
        updateHeader.intValue    = -1;

        C[ctx]->UpdateMapperFromFlow(updateHeader);

        if ( updateHeader.numMessages < 0 )
        {
            throw new Exception (true, "D3dFlowMapper::NextStep: # incoming messages must >= 0");
        }

        nextStep = updateHeader.nextStep;

        if ( updateHeader.numMessages == 1 )
        {
            inMess[totNumIn].value = updateHeader.intValue;
            totNumIn++;
        }
    }
    //printf("%20d -> mapper\n",nextStep);

    if ( nextStep == D3dFlowMap_Finish ) // TODORE: check if it can be removed
    {
        return nextStep;
    }

    //
    // Store incoming messages in Mappers message store
    //

    mapMess->InitInMess();

    if ( totNumIn > 0 )
    {
        if ( totNumIn != NR_CNTXTS )
        {
            throw new Exception (true, "D3dFlowMapper::NextStep: #inmessages inconsistent with #flow processes");
        }
        else
        {
            for ( int m = 0 ; m < totNumIn ; m++ )
            {
                mapMess->PutInMess(inMess[m]);
            }
        }
    }


    //
    // Reset outgoing messages before next step starts
    //

    mapMess->InitOutMess();

    return nextStep;
}
