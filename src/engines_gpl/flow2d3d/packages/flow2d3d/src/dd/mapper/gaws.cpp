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
// $Id: gaws.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/gaws.cpp $
//------------------------------------------------------------------------------
// Global ADI-Wang Solver
//
//  Stef.Hummel@deltares.nl
//  Erik.deGoede@deltares.nl
//  Adri.Mourits@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"



//
// Parse mapper config string
//

#define LOG_WANG        0
#define LOG_VERBOSE     0
#define LOG_VERBOSE2    0
#define LOG_VERBOSE3    0

#if(LOG_WANG)
#define ON_LOG_WANG(code)   code
#else
#define ON_LOG_WANG(code)
#endif

#if(LOG_VERBOSE)
#define ON_LOG_VERBOSE(code)    code
#else
#define ON_LOG_VERBOSE(code)
#endif

#if(LOG_VERBOSE2)
#define ON_LOG_VERBOSE2(code)   code
#else
#define ON_LOG_VERBOSE2(code)
#endif



//////////////////////////////////////////////////////////////////////
//
// Gaws
//
//


//////////////////////
// Public functions
//


Gaws::Gaws(void)
{
    //
    // Initialize pointers and values to zero
    //

    this->gawsIterator  = NULL;

    this->logFile       = NULL;

    this->contexts      = NULL;
    this->nProc         = 0;

    this->wangEquations_X = NULL;
    this->wangEquations_Y = NULL;

    this->numMappers        = 0;
    this->nSeries           = 0;
    this->maxRefineLevel[Wang_XDir] = 0;
    this->maxRefineLevel[Wang_YDir] = 0;

    this->eqSeries = (EqSeries *) malloc(sizeof(EqSeries)*EQ_SER_BLOCK_SIZE);

}


int Gaws::Setup(
    Iterator       * gaws,              // GAWS iterator
    char           * aConfigStr,        // configuration info
    int              aNumprocs,         // #processes
    GawsProcRelation procRelations[]    // gaws process info for each process
    )
{
    int retVal=HY_OK;    // return value

    //
    // Store Gaws (hyex) object ID (and configString, currently not used)
    //

    this->config       = aConfigStr;

    //
    // Store number of processes
    // Attach a  object for each Delft3d-Flow context,
    // Initialize the Gaws-Context
    //

    this->gawsIterator = gaws;
    this->nProc        = aNumprocs;

    //
    // Fill mapper cells with mapper configuration
    // (mapper cells info is used for creating the wang administration)
    //

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "GAWS \"%s\" SETUP", gaws->name);

    this->InitializeMapperInfo();

    {for ( int m = 0 ; ( m < this->numMappers ) && ( retVal == HY_OK ) ; m++ )
    {
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" fills mapper cells for mapper %d", gaws->name, m);

        retVal = this->FillMapperCells(&(this->mapCells[m]), this->mapInfo[m]);

        ON_LOG_VERBOSE(
            printf("%d Ncb %d Nce %d Mcb %d Mce %d Nnb %d Nne %d Mnb %d Mne %d \n",
                mapCells[m].leftToRight,
                mapCells[m].nCurrBegin , mapCells[m].nCurrEnd   ,
                mapCells[m].mCurrBegin , mapCells[m].mCurrEnd   ,
                mapCells[m].nNbrBegin  , mapCells[m].nNbrEnd    ,
                mapCells[m].mNbrBegin  , mapCells[m].mNbrEnd    );
        )
    }}

    //
    // Setup dd-data gaws side for each flow
    //

    this->contexts = new D3dFlowContextGawsSide * [aNumprocs];

    {for ( int p = 0 ; ( p < this->nProc ) && ( retVal == HY_OK ) ;  p++ )
    {

        this->contexts[p] = new D3dFlowContextGawsSide;
        this->contexts[p]->Setup(   procRelations[p].gawsBarrier,
                                    procRelations[p].flowIterator,
                                    procRelations[p].memType);
    }}

    //
    // Create the 'global wang' equation administration
    //

    retVal = CreateWangAdministration(Wang_YDir);
    if ( retVal != HY_ERR )
    {
        retVal = CreateWangAdministration(Wang_XDir);
        if (retVal)
        {
            RefineSeries();
        }
        else
        {
            throw new Exception (true, "CreateWangAdministration(Wang_YDir) UNSUCCESFULL");
        }
    }
    else
    {
        throw new Exception (true, "CreateWangAdministration(Wang_YDir) UNSUCCESFULL");
    }

    //
    // Create the data for communication with flow.
    //

    {for ( int p = 0 ; ( p < this->nProc ) && ( retVal == HY_OK ) ; p++ )
    {
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "GAWS Sets up communication for process %d", p);
        this->contexts[p]->SetupCommunication();
    }}

    //
    // Receive first update of dd-data on comm. points.
    //

    // TODORE REMOVE {for ( int p = 0 ; ( p < this->nProc ) && ( retVal == HY_OK ) ;  p++ )
    //    this->contexts[p]->UpdateGawsFromFlow(GawsDistrib_AllVars);
    // }

    //
    // Based on the administration, create the actual equations
    //

    if ( retVal != HY_ERR )
    {
        int rLev;   // loop counter

        this->wangEquations_X = new WangEquations * [maxRefineLevel[Wang_XDir]+1];
        this->wangEquations_Y = new WangEquations * [maxRefineLevel[Wang_YDir]+1];

        for ( rLev = 0 ; rLev <= maxRefineLevel[Wang_XDir] ; rLev++ )
        {
            this->wangEquations_X[rLev] = NULL;
        }

        for ( rLev = 0 ; rLev <= maxRefineLevel[Wang_YDir] ; rLev++ )
        {
            this->wangEquations_Y[rLev] = NULL;
        }

        for ( rLev = 0 ; rLev <= maxRefineLevel[Wang_XDir] && retVal != HY_ERR ; rLev++ )
        {
            this->wangEquations_X[rLev] = new WangEquations;
            retVal = CreateWangEquations(this->wangEquations_X[rLev],Wang_XDir,rLev);
        }


        if ( retVal != HY_ERR )
        {
            for ( rLev = 0 ; rLev <= maxRefineLevel[Wang_YDir] && retVal != HY_ERR ; rLev++ )
            {
                this->wangEquations_Y[rLev] = new WangEquations;
                retVal = CreateWangEquations(this->wangEquations_Y[rLev],Wang_YDir,rLev);
            }
        }
    }

    return retVal;
}


void Gaws::DoSolving(void)
{
    //
    // Loop, waiting for solve-LeftToRight, solve-TopToBottom to Finalize flag
    //
    bool finished = false;
    while (! finished)
    {
        //
        // Get LeftToRight|TopToBottom flags from Delft3D flow processes
        // Check if all flags are the same
        //

        GawsDirectionMesg inMesg[MAX_NUM_D3D_FLOW_PROCS];

        {for ( int p = 0 ; p < this->nProc ;  p++ )
        {
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS waiting for L2R message from flow \"%s\"",
                            this->contexts[p]->flowIterator->name);

            int incomingBlobID=0;
            Blob * leftToRightBlob = new Blob ( &(inMesg[p]), sizeof(GawsDirectionMesg));
            this->contexts[p]->flowIterator->Receive(leftToRightBlob, &incomingBlobID);  // DDD-COMM: FLOW->GAWS, left2right message
            delete leftToRightBlob;
            if ( incomingBlobID != F2G_Blob_Left2RightMessage )
            {
                throw new Exception (true, "Gaws::Setup: DoSolving blobID (%d /= %d)",
                            incomingBlobID, F2G_Blob_Left2RightMessage);
            }
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS received L2R message (%d) from flow \"%s\"",
                            inMesg[p].intValue, this->contexts[p]->flowIterator->name);

        }}

        {for ( int p = 0 ; p < this->nProc ;  p++ )
        {
            if ( inMesg[p].intValue != inMesg[0].intValue )
                throw new Exception (true, "GAWS barrier: flow processes out of L2R/T2B phase");
        }}

        if ( inMesg[0].intValue == FinishGaws )
        {
            finished = true; // TODORE: indeed send FinishGaws at FlowSide
                             // Alternatief is om gewoon te wachten
                             //     totdat iemand een master stop doet.
        }
        else
        {
            //
            // Receive DD data from flow side
            //
            {for ( int p = 0 ; p < this->nProc ;  p++ )
            {
                this->contexts[p]->UpdateGawsFromFlow(GawsDistrib_AllVars); // DDD-COMM: FLOW->GAWS, Solver Update
            }}

            //
            // perform solve action (inMess == 1 : leg
            // (solve receives and sends data to flow)
            //

            bool leftToRight = ( inMesg[0].intValue == 1 ) ? true : false;
            this->Solver( leftToRight );

            //
            // Send result message (OK) to all participants for shared mem.
            //

            GawsOKMesg okMesg;
            okMesg.result = 0;
            Blob * okMesgBlob = new Blob(&okMesg, sizeof(GawsOKMesg));

            {for ( int p = 0 ; p < this->nProc ;  p++ )
            {

                FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS sending OK message to flow \"%s\"",
                                this->contexts[p]->flowIterator->name);

                this->contexts[p]->flowIterator->Send(okMesgBlob, G2F_Blob_OKMessage); // DDD-COMM: GAWS->FLOW, ok message

                FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS sent OK message to flow \"%s\"",
                                this->contexts[p]->flowIterator->name);

            }}

            delete okMesgBlob;

            //
            // Send DD data to flow
            //
            {for ( int p = 0 ; p < this->nProc ;  p++ )
            {
                this->contexts[p]->UpdateGawsToFlow(GawsDistrib_AllVars); // DDD-COMM: GAWS->FLOW, Solver Update
            }}

        }

    }

}


void Gaws::Solver(
    bool        leftToRight    // boolean L->R or Top->Bottom
    )
{
    int rLev;                  // refinement level

    //
    // Solve system of equations that has been built up in Gaws::Set_up
    //

    if ( leftToRight )
    {
        ON_LOG_WANG(
            printf("Start Wang-X (%x) solver\n", this->wangEquations_X);
            fflush(stdout);
        )

        for ( rLev = 0 ; rLev <= maxRefineLevel[Wang_XDir] ; rLev++ )
        {
            ON_LOG_WANG(
                printf("Refinement level: %d\n", rLev);
            )

            this->wangEquations_X[rLev]->PreEliminate();
            this->wangEquations_X[rLev]->TridiagSolve();
        }

        for ( rLev = maxRefineLevel[Wang_XDir] ; rLev >=0 ; rLev-- )
        {
            ON_LOG_WANG(
                printf("X-Tridiag-Refinement level: %d\n", rLev);
            )

            this->wangEquations_X[rLev]->CopyTridiagSol();
        }
    }
    else
    {
        ON_LOG_WANG(
            printf("Start Wang-Y (%x) solver\n", this->wangEquations_Y);
            fflush(stdout);
            )

        for ( rLev = 0 ; rLev <= maxRefineLevel[Wang_YDir] ; rLev++ )
        {
            ON_LOG_WANG(
                printf("Refinement level: %d\n", rLev);
            )

            this->wangEquations_Y[rLev]->PreEliminate();
            this->wangEquations_Y[rLev]->TridiagSolve();
        }

        for ( rLev = maxRefineLevel[Wang_YDir] ; rLev >=0 ; rLev-- )
        {
            ON_LOG_WANG(
                printf("Y-Tridiag-Refinement level: %d\n", rLev);
            )

            this->wangEquations_Y[rLev]->CopyTridiagSol();
        }
    }

}


//////////////////////
// Protected functions
//


int Gaws::FillMapperCells(
    MapperCells    * cells,         // mapper cells info
    GawsMapInfo     mapper          // gaws mapper Info for this mapper
    )
{
    int         retVal=HY_OK;           // return value

    EdgeType    edgeType[NR_CNTXTS];    // Edgetype current and neighbor domain
    int         firstCell[NR_CNTXTS];   // First cell of coupling
    int         lastCell[NR_CNTXTS];    // Last cell of coupling
    int         normalCell[NR_CNTXTS];  // Normal cell of coupling
    int         refine[NR_CNTXTS];      // refinement of contexts
    int         ctx;                    // Context loop counter

    //
    // Init cells, read (m|n)(start|end) from config line
    //

    cells->mCurrBegin = YET_TO_INIT;
    cells->mCurrEnd   = YET_TO_INIT;
    cells->nCurrBegin = YET_TO_INIT;
    cells->nCurrEnd   = YET_TO_INIT;
    cells->mNbrBegin  = YET_TO_INIT;
    cells->mNbrEnd    = YET_TO_INIT;
    cells->nNbrBegin  = YET_TO_INIT;
    cells->nNbrEnd    = YET_TO_INIT;

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        firstCell[ctx]    = YET_TO_INIT;
        lastCell[ctx]     = YET_TO_INIT;
        normalCell[ctx]   = YET_TO_INIT;
        refine[ctx]       = YET_TO_INIT;
    }

    //
    // If succesfully read, express the 'couple cells' in the current
    // domain and the related 'couple cells' in the neighbor domain
    // in M- and N-coordinates.
    //

    if ( ParseMapperConfigString(mapper.configStr, edgeType,
                                    firstCell, lastCell,
                                    normalCell, refine, 0) != HY_OK )
    {
        throw new Exception (true, "Call to configStringParser failed for configString \"%s\"", mapper.configStr);
    }
    else
    {
        for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        {
            if (    firstCell[ctx] == YET_TO_INIT
                 || firstCell[ctx] == 0
                 || lastCell[ctx]   == YET_TO_INIT
                 || lastCell[ctx]   == 0
                 || normalCell[ctx]== YET_TO_INIT
                 || normalCell[ctx]== 0           )
            {
                throw new Exception (true, "Could not read required info from  configString \"%s\"", mapper.configStr);
            }
        }

        switch ( edgeType[C_0] )
        {
            case Edge_Top:

                cells->leftToRight = 0;

                cells->mCurrBegin = firstCell [C_0] + 1;
                cells->mCurrEnd   = lastCell  [C_0];
                cells->nCurrBegin = normalCell[C_0] + 1;
                cells->nCurrEnd   = normalCell[C_0] + 1;

                cells->mNbrBegin  = firstCell [C_1] + 1;
                cells->mNbrEnd    = lastCell  [C_1];
                cells->nNbrBegin  = normalCell[C_1];
                cells->nNbrEnd    = normalCell[C_1];

                cells->mapInfo.flowIterator[C_0] = mapper.flowIterator[C_0];
                cells->mapInfo.flowIterator[C_1] = mapper.flowIterator[C_1];

                cells->curRefine  = refine[C_0];
                cells->nbrRefine  = refine[C_1];

                break;

            case Edge_Bottom:

                cells->leftToRight = 0;

                cells->mCurrBegin = firstCell [C_1] + 1;
                cells->mCurrEnd   = lastCell  [C_1];
                cells->nCurrBegin = normalCell[C_1] + 1;
                cells->nCurrEnd   = normalCell[C_1] + 1;

                cells->mNbrBegin  = firstCell [C_0] + 1;
                cells->mNbrEnd    = lastCell  [C_0];
                cells->nNbrBegin  = normalCell[C_0];
                cells->nNbrEnd    = normalCell[C_0];

                cells->mapInfo.flowIterator[C_0] = mapper.flowIterator[C_0];
                cells->mapInfo.flowIterator[C_1] = mapper.flowIterator[C_1];

                cells->curRefine  = refine[C_1];
                cells->nbrRefine  = refine[C_0];

                break;

            case Edge_Left:

                cells->leftToRight = 1;

                cells->nCurrBegin = firstCell [C_1] + 1;
                cells->nCurrEnd   = lastCell  [C_1];
                cells->mCurrBegin = normalCell[C_1];
                cells->mCurrEnd   = normalCell[C_1];

                cells->nNbrBegin  = firstCell [C_0] + 1;
                cells->nNbrEnd    = lastCell  [C_0];
                cells->mNbrBegin  = normalCell[C_0] + 1;
                cells->mNbrEnd    = normalCell[C_0] + 1;

                cells->mapInfo.flowIterator[C_0] = mapper.flowIterator[C_0];
                cells->mapInfo.flowIterator[C_1] = mapper.flowIterator[C_1];

                cells->curRefine  = refine[C_1];
                cells->nbrRefine  = refine[C_0];

                break;

            case Edge_Right:

                cells->leftToRight = 1;

                cells->nCurrBegin = firstCell [C_0] + 1;
                cells->nCurrEnd   = lastCell  [C_0];
                cells->mCurrBegin = normalCell[C_0] + 1;
                cells->mCurrEnd   = normalCell[C_0] + 1;

                cells->nNbrBegin  = firstCell [C_1] + 1;
                cells->nNbrEnd    = lastCell  [C_1];
                cells->mNbrBegin  = normalCell[C_1];
                cells->mNbrEnd    = normalCell[C_1];

                cells->mapInfo.flowIterator[C_0] = mapper.flowIterator[C_0];
                cells->mapInfo.flowIterator[C_1] = mapper.flowIterator[C_1];

                cells->curRefine  = refine[C_0];
                cells->nbrRefine  = refine[C_1];

                break;

            default:

            throw new Exception (true, "Wrong Edge Type in FillMapperCells");
            break;

        }
        ON_LOG_VERBOSE(
            printf("edgeType[C_0] %d edgeType[C_1] %d\n",
                        edgeType[C_0], edgeType[C_1]);
            printf("Ncb %d Nce %d Mcb %d Mce %d Nnb %d Nne %d Mnb %d Mne %d \n",
            cells->nCurrBegin ,
            cells->nCurrEnd   ,
            cells->mCurrBegin ,
            cells->mCurrEnd   ,

            cells->nNbrBegin  ,
            cells->nNbrEnd    ,
            cells->mNbrBegin  ,
            cells->mNbrEnd    );
        )
    }
    return retVal;
}


int Gaws::FindNeighborPoint(
    bool    leftToRight,    // boolean L->R or Top->Bottom
    D3dFlowContextGawsSide
          * curCntxt,       // context of current domain
    int     curCell,        // Cell-index in current  domain
    int     curLine,        // Line-index  "     "      "
    int   * nbrCell,        // Cell-index in neighbor domain
    int   * nbrLine,        // Line-index  "     "      "
    D3dFlowContextGawsSide
         ** nbrCntxt        // context of neighbor domain
    )
{
    int     retVal=HY_ERR;      // return value
    int     map,p;              // loop counter

    Iterator * nbrFlowIterator; // interator for neighbor domain

    //
    // Search in MapperCell info for a mapper that is of the right
    // direction type, and that contains the current M/N point in
    // it's source domain.
    // If found, determine the M/N point in the neighbor domain.
    //

    *nbrCntxt = NULL;

    ON_LOG_VERBOSE( printf("FindNeighborPoint(%d, %d, (%d,%d))\n",
                    leftToRight, curCntxt->GetContextId(), curCell, curLine);         )

    for ( map = 0 ; map < this->numMappers ; map ++ )
    {
        ON_LOG_VERBOSE(
            printf("ml2r: %d\n", mapCells[map].leftToRight);
        )

        if ( mapCells[map].leftToRight == leftToRight )
        {
            if ( curCntxt->flowIterator->id ==
                    mapCells[map].mapInfo.flowIterator[C_0]->id )
            {
                int cM, cN;
                if ( leftToRight )
                {
                    cM = curCell;
                    cN = curLine;
                }
                else
                {
                    cM = curLine;
                    cN = curCell;
                }
                if (    (    cM >= mapCells[map].mCurrBegin
                      && cM <= mapCells[map].mCurrEnd   )
                     && (    cN >= mapCells[map].nCurrBegin
                      && cN <= mapCells[map].nCurrEnd   ) )
                {
                    if ( leftToRight )
                    {
                        *nbrCell =   mapCells[map].mNbrEnd;
                        if ( mapCells[map].curRefine >=
                            mapCells[map].nbrRefine )
                        {
                            *nbrLine = mapCells[map].nNbrBegin +
                                (curLine - mapCells[map].nCurrBegin) /
                                mapCells[map].curRefine ;
                        }
                        else
                        {
                            *nbrLine = mapCells[map].nNbrBegin +
                                (curLine - mapCells[map].nCurrBegin) *
                                mapCells[map].nbrRefine ;
                        }
                    }
                    else
                    {
                        *nbrCell =   mapCells[map].nNbrEnd;
                        if ( mapCells[map].curRefine >=
                            mapCells[map].nbrRefine )
                        {
                            *nbrLine = mapCells[map].mNbrBegin +
                                (curLine - mapCells[map].mCurrBegin) /
                                mapCells[map].curRefine ;
                        }
                        else
                        {
                            *nbrLine = mapCells[map].mNbrBegin +
                                (curLine - mapCells[map].mCurrBegin) *
                                mapCells[map].nbrRefine ;
                        }
                    }
                    nbrFlowIterator = mapCells[map].mapInfo.flowIterator[C_1];
                    retVal = HY_OK;
                }
            }
        }
    }

    if ( retVal == HY_OK )
    {
        //
        // set pointer to the neighbor context by finding its Id
        //

        for ( p = 0 ; ( p < this->nProc ) && ( *nbrCntxt == NULL ) ;  p++ )
        {
            if ( this->contexts[p]->flowIterator->id == nbrFlowIterator->id )
            {
                *nbrCntxt = this->contexts[p];
            }
        }
    }

    ON_LOG_VERBOSE(
        int nbrCID = ( *nbrCntxt != NULL ) ? (*nbrCntxt)->GetContextId() : -999 ;
        printf("FN(%3d): %6d(%3d,%3d) -> %6d(%3d,%3d)\n", retVal,
                            curCntxt->GetContextId(), curCell, curLine,
                            nbrCID, *nbrCell, *nbrLine);
    )

    return retVal;
}


int Gaws::CreateWangAdministration(
    WangDir wangDir         // Wang direction (X or Y)
)
{
    int     retVal=HY_OK;   // return value
    int     p;              // processes loop counter
    int     kcs, kcsDown;   // Deflt3d-flow KCS flag
    int     cell,line;      // Cell / line index in domain with id
    int     cellMax,lineMax;// dimensions of subdomain:
                            // - in case of leftToRight:
                            //   cellMax= MMAX, lineMax= NMAX
                            //   (equations in x-direction)
                            // - in case of  not leftToRight:
                            //   cellMax= NMAX, lineMax= MMAX
                            // (equations in y-dir.)
    int     level = 0;      // Refinement level
    int     eqSerie;        // equation serie number
    int     lineMin,cellMin;// start of loops
    int     ddbound;        // size of domdec boundary
    D3dFlowContextGawsSide
          * curCntxt;       // pointer to context
    bool    leftToRight;    // boolean L->R or Top->Bottom

    //
    // Create equations object
    // For all process-domain perform the algorithm:
    // Loop over all N rows
    //     Loop over all M cells
    //         If the KCS is 0 for the previous cell and 1 or 2 for the
    //         current cell, it is the start of a row. Follow this
    //         row until a 0 (end of row) or a 3 (couple point) is met.
    //         If it is a 0, restart the search for the start of a row.
    //         If it is a 3, store the equation next to the couple point,
    //         and continue in the neighbor domain.
    //

    ON_LOG_WANG(
        printf("Begin routine create wang-administration\n");
        fflush(stdout);
    )

    leftToRight = ( wangDir == Wang_XDir ) ? true : false;

    for ( p = 0 ; ( p < this->nProc ) && ( retVal == HY_OK ) ;  p++ )
    {
        curCntxt = this->contexts[p];

        ddbound = curCntxt->dDb;
        cellMin = 1 - ddbound;
        lineMin = 1 - ddbound;

        ON_LOG_WANG( printf("ddbound %d\n", ddbound); )

        if ( leftToRight )
        {
            cellMax = curCntxt->mMax;
            lineMax = curCntxt->nMax;
        }
        else
        {
            cellMax = curCntxt->nMax;
            lineMax = curCntxt->mMax;
        }

        ON_LOG_VERBOSE(
            printf("Max[%d]=%d,%d\n", p, cellMax, lineMax);
            fflush(stdout);
        )

        for ( line = lineMin ; line <= lineMax ;  line++ )
        {
            for ( cell = cellMin ; cell <= cellMax ; cell++ )
            {
                kcsDown = curCntxt->Get_KCS_XorY(cell-1,line,leftToRight);
                kcs     = curCntxt->Get_KCS_XorY(cell,  line,leftToRight);

                if ( ( kcs == 1 || kcs == 2 ) && ((kcsDown == 0)||(kcsDown == 2)) )
                {
                    for ( cell=cell+1 ; ( cell<=cellMax ) && ( kcs==1 || kcs==2 ) ;  cell++ )
                    {
                        kcs = curCntxt->Get_KCS_XorY(cell, line, leftToRight);
                        if ( kcs == 3 )
                        {
                            eqSerie = NewEquationSerie(leftToRight,level);

                            ON_LOG_VERBOSE(
                                printf("newSerie (kcs=3): %d\n", eqSerie);
                            )

                            retVal = ProcessCouplePoint(curCntxt, leftToRight,
                                        eqSerie, cell, line);
                        }
                    }
                    cell--;
                }
            }
        }
    }

    ON_LOG_WANG(
        printf("CreateWangAdministration(%d) RetVal: %d\n", wangDir, retVal);
        )

    return retVal;
}


int Gaws::ContinueInNextContext(
    int             eqSerie,    // equation serie number
    int             curCell,    // cell-index in this next domain
    int             curLine,    // line-index  "  "    "      "
    D3dFlowContextGawsSide
                  * curCntxt    // context of  "    "      "
    )
{
    bool        leftToRight;    // boolean L->R or Top->Bottom
    int         retVal=HY_OK;   // return value
    int         cell,cellMax;   // M index/maximum in domain
    int         kcs;            // Delft3d-flow KCS flag

    leftToRight = eqSeries[eqSerie].leftToRight;

    // Store the equation at the beginning of the row (needed for elimination)
    // Continue in this the search for the end of a row
    // Loop over the M cells from the current cell on
    //     Find the first KCS that not equal to 1 or 2.
    //     If it is a 0, store the last equation of this line.
    //     If it is a 3, store the equation next to the couple point,
    //     and continue in the neighbor domain.

    if ( leftToRight )
    {
        cellMax  = curCntxt->mMax;
    }
    else
    {
        cellMax  = curCntxt->nMax;
    }

    kcs = curCntxt->Get_KCS_XorY(curCell, curLine, leftToRight);

    ON_LOG_VERBOSE2(
        printf("KCS: %d at line=%d,cell=%d\n", kcs, curLine, curCell);
        fflush(stdout);
    )

    if (kcs != 3)
    {
        throw new Exception (true, "Inconsistent Coupling indices");
    }
    else
    {
        curCell++;

        kcs = 1;
        for ( cell = curCell ; ( cell <= cellMax ) && ( kcs !=0 ) ; cell++ )
        {
            kcs = curCntxt->Get_KCS_XorY(cell, curLine, leftToRight);

            if ( kcs == 1 || kcs == 2 )
            {
                for ( cell=cell+1 ; ( cell<=cellMax ) && ( kcs==1 || kcs==2 ) ;  cell++ )
                {
                    kcs = curCntxt->Get_KCS_XorY(cell, curLine, leftToRight);

                    if ( kcs == 3 )
                    {
                        retVal = ProcessCouplePoint(curCntxt, leftToRight,
                                        eqSerie, cell, curLine);
                    }
                    else if ( kcs == 0 || kcs == 2 )
                    {
                        AddEqPoint( eqSerie,
                                curCntxt, (kcs == 0) ? cell-1 : cell, curLine,
                                NULL,     0,   0,
                                Pnt_EndLine, 0);
                    }
                }
                cell--;
            }
        }
    }

    return retVal;
}


int Gaws::CreateWangEquations(
    WangEquations * Wang_Eq_XorY,   // pointer Wang Equations context
    WangDir         wangDir,        // Wang direction (X or Y)
    int             level           // Refinement level
)
{
    int     retVal=HY_OK;           // return value
    int     p;                      // EQ_PNT loop counter
    int     s;                      // EQ_SER loop counter
    int     curM, curN;             // Index for get - function
    int     coupCurM, coupCurN;     // Index for get - function of
                                    //   neighbour (is a coupling point)
    int     nbrM, nbrN;             // Index for get - function
                                    //   in neighbor domain
    int     coupNbrM, coupNbrN;     // Index for get - function of
                                    //   neighbour point in neighbor domain
                                    //   (is a coupling point)
    bool    BeginOfLine;            // Flag for begin/end of column or row
    D3dFlowContextGawsSide   * curCntxt; // pointer to context
    D3dFlowContextGawsSide   * nbrCntxt; // pointer to neighbor context
    bool            leftToRight;    // boolean L->R or Top->Bottom

    //
    // Create equations object
    // For all equation points in all equation series build a wang equation
    //

    ON_LOG_WANG(
        printf("Begin routine create wang-equations \n");
        fflush(stdout);
    )

    leftToRight = ( wangDir == Wang_XDir ) ? true : false;

    for ( s = 0 ; s<nSeries ; s++ )
    {
        if ( ( eqSeries[s].leftToRight == leftToRight ) &&
             ( eqSeries[s].level       == level       )    )
        {
            for ( p = 0 ; p < eqSeries[s].nPnt ; p++ )
            {
                EqPoint * pnt = &(eqSeries[s].pnt[p]);

                if ( leftToRight )
                {
                    curM = pnt->cell;
                    curN = pnt->line;
                    nbrM = pnt->nbrCell+1;
                    nbrN = pnt->nbrLine;

                    coupCurM = curM + 1;
                    coupCurN = curN;
                    coupNbrM = nbrM - 1;
                    coupNbrN = nbrN;
                }
                else
                {
                    curM = pnt->line;
                    curN = pnt->cell;
                    nbrM = pnt->nbrLine;
                    nbrN = pnt->nbrCell+1;

                    coupCurM = curM;
                    coupCurN = curN + 1;
                    coupNbrM = nbrM;
                    coupNbrN = nbrN - 1;
                }
                curCntxt = pnt->curCntxt;
                nbrCntxt = pnt->nbrCntxt;

                //
                // build equation for equation point
                //
                BeginOfLine = false;
                Wang_Eq_XorY->AddEquation(
                               leftToRight,
                               curM,     curN,
                               coupCurM, coupCurN,
                               curCntxt, BeginOfLine );

                //
                // build equation for corresponding neighbour
                //
                if ( pnt->type != Pnt_EndLine )
                {
                    BeginOfLine = true;
                    Wang_Eq_XorY->AddEquation(
                                   leftToRight,
                                   nbrM,     nbrN,
                                   coupNbrM, coupNbrN,
                                   nbrCntxt, BeginOfLine );
                }

                //
                //  additional dummy equation
                //  in case of refinement at end of line
                //
                if ( pnt->type == Pnt_EndRefine )
                {
                    BeginOfLine = false;
                    Wang_Eq_XorY->AddEquation(
                                   leftToRight,
                                   nbrM,     nbrN,
                                   coupNbrM, coupNbrN,
                                   nbrCntxt, BeginOfLine );

                }
            }
        }
    }
    return retVal;
}


void Gaws::GawsLog(
    char      * format, // I: 'fprintf-format' for print of log
    ...                 // I: arguments of log message (should be
                        //  terminated with NULL)
    )
{
    char    name[100];  // filename
    va_list arguments;  // var-arg list

    //
    // print the log-message to file or standard-out
    //

    va_start ( arguments, format );

    if ( DoLoggingToFileFor(Log_Gaws) )
    {
        if (logFile == NULL)
        {
            sprintf(name, "gaws.%s", this->gawsIterator->name);
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


Gaws::~Gaws(void)
{
    int e;
    //
    // Clean up
    //

    if ( this->wangEquations_X != NULL )
    {
        for ( e = 0 ; e <= maxRefineLevel[1] ; e++ )
        {
            if ( this->wangEquations_X[e] != NULL )
            {
                delete this->wangEquations_X[e];
            }
        }
        delete [] this->wangEquations_X;
    }

    if ( this->wangEquations_Y != NULL )
    {
        for ( e = 0 ; e <= maxRefineLevel[0] ; e++ )
        {
            if ( this->wangEquations_Y[e] != NULL )
            {
                delete this->wangEquations_Y[e];
            }
        }
        delete [] this->wangEquations_Y;
    }

    for ( int s = 0 ; s < this->nSeries ; s++ )
    {
        free(this->eqSeries[s].pnt);
    }
    free(this->eqSeries);

    if (this->logFile != NULL)
    {
        fclose(this->logFile);
    }
}


int Gaws::CheckDependents(
    D3dFlowContextGawsSide  * curCntxt,
    D3dFlowContextGawsSide  * nbrCntxt,
    bool        leftToRight,
    int         myLine,
    int       * centreLine
    )
{
    int retVal = -999;
    Iterator * curIterator; // Flow iterator of current domain
    Iterator * nbrIterator; // Flow iterator of neighbor domain
    int     map;            // loop counter

    /*
     * Return the refinement factor compared to prev Context
     *  0:      no refinement
     *  n (n>0):    myCid   is refined with n=2m+1
     * -n (n>0):    nextCid is refined with n=2m+1
     * If nbrCntxt is the refined one, return the centre line of that context
     * If curCntxt is the refined one, return the centre line of this context
     */
    curIterator = curCntxt->flowIterator;
    nbrIterator = nbrCntxt->flowIterator;

    for ( map = 0 ; map < this->numMappers ; map ++ )
    {
        if ( mapCells[map].leftToRight == leftToRight )
        {
            int curMax = leftToRight ? mapCells[map].nCurrEnd : mapCells[map].mCurrEnd;
            int curMin = leftToRight ? mapCells[map].nCurrBegin : mapCells[map].mCurrBegin;
            int nbrMax = leftToRight ? mapCells[map].nNbrEnd : mapCells[map].mNbrEnd;
            int nbrMin = leftToRight ? mapCells[map].nNbrBegin : mapCells[map].mNbrBegin;

            if ( ( curIterator->id == mapCells[map].mapInfo.flowIterator[C_0]->id ) &&
                 ( nbrIterator->id == mapCells[map].mapInfo.flowIterator[C_1]->id ) &&
                 ( myLine >= curMin && myLine <= curMax)                                  )
            {
                if ( mapCells[map].curRefine > mapCells[map].nbrRefine )
                {
                    *centreLine = GetCentreLine(map,myLine,C_0);
                    if ( *centreLine >= curMin && *centreLine <= curMax )
                    {
                        retVal = mapCells[map].curRefine;
                        break;
                    }
                }
                else if (mapCells[map].curRefine < mapCells[map].nbrRefine)
                {
                    *centreLine = GetCentreLine(map,myLine,C_1);
                    if ( *centreLine >= nbrMin && *centreLine <= nbrMax )
                    {
                        retVal =  -mapCells[map].nbrRefine;
                        break;
                    }
                }
                else
                {
                    *centreLine = myLine;
                    retVal = 0;
                    break;
                }
            }
        }
    }

    if (retVal == -999)
    {
        throw new Exception (true, "ERROR returned in GAWS::CheckDependents.\n");
    }

    ON_LOG_VERBOSE(
        printf("mapCells[%2d].refines for line %d: %d,%d -> %d\n",
                map, myLine, mapCells[map].curRefine,
                mapCells[map].nbrRefine, retVal);
    )

    return retVal;
}


int Gaws::GetCentreLine(
    int         map,
    int         myLine,
    int         ctx
    )
{
    int     line,refine,start[NR_CNTXTS];

    refine = (ctx == C_0) ? mapCells[map].curRefine :
                mapCells[map].nbrRefine;

    if ( mapCells[map].leftToRight )
    {
        start[C_0] = mapCells[map].nCurrBegin;
        start[C_1] = mapCells[map].nNbrBegin ;
    }
    else
    {
        start[C_0] = mapCells[map].mCurrBegin;
        start[C_1] = mapCells[map].mNbrBegin ;
    }

    if ( ctx == C_0 )
    {
        line = (myLine - start[C_0]) / refine;
        line = line * refine + refine / 2 + start[C_0];
    }
    else
    {
        line = start[C_1] + (myLine - start[C_0]) * refine + refine/2;
    }

    ON_LOG_VERBOSE2(
        printf("mapCells[%2d].centreLine for line %d = %d (ctx=%d)\n",
                map, myLine, line, ctx);
    )

    return line;
}


int Gaws::NewEquationSerie(
    bool        leftToRight,
    int         level
    )
{
    if ( nSeries % EQ_SER_BLOCK_SIZE == 0)
    {
        int nNewSeries = nSeries + EQ_SER_BLOCK_SIZE;
        eqSeries = (EqSeries*) realloc( eqSeries, sizeof(EqSeries) * nNewSeries );
        if ( eqSeries == NULL )
        {
            throw new Exception (true, "NewEquationSerie: could not realloc %d series", nNewSeries);
        }
    }
    //
    // Initialize new Equations Series of type leftToRight and current level
    //
    eqSeries[nSeries].nPnt          = 0;
    eqSeries[nSeries].pnt           = (EqPoint*) malloc( sizeof(EqPoint)*EQ_PNT_BLOCK_SIZE );
    eqSeries[nSeries].leftToRight   = leftToRight;
    eqSeries[nSeries].level         = level;
    eqSeries[nSeries].hasDependents = false;
    nSeries++;
    return (nSeries - 1);
}


void Gaws::AddEqPoint(
    int             eqSerie,
    D3dFlowContextGawsSide   * curCntxt,
    int             curCell,
    int             curLine,
    D3dFlowContextGawsSide   * nbrCntxt,
    int             nbrCell,
    int             nbrLine,
    PntType         type,
    int             numDependents
    )
{
    //
    // Add point to the current serie
    // (store its values, and incr. #points for the serie).
    //

    EqPoint * eqPoint;

    if ( eqSeries[eqSerie].nPnt % EQ_PNT_BLOCK_SIZE == 0)
    {
        int nNewPoints = eqSeries[eqSerie].nPnt + EQ_PNT_BLOCK_SIZE;
        eqSeries[eqSerie].pnt = (EqPoint*) realloc( eqSeries[eqSerie].pnt,
                    sizeof(EqPoint) * nNewPoints );
        if ( eqSeries[eqSerie].pnt == NULL )
        {
            throw new Exception (true, "AddEqPoint: could not realloc %d points", nNewPoints);
        }
    }

    eqPoint = &(eqSeries[eqSerie].pnt[eqSeries[eqSerie].nPnt]);

    eqPoint->curCntxt = curCntxt;

    eqPoint->cell     = curCell;
    eqPoint->line     = curLine;

    eqPoint->nbrCntxt = nbrCntxt;
    eqPoint->nbrCell  = nbrCell;
    eqPoint->nbrLine  = nbrLine;

    eqPoint->type     = type;

    eqPoint->numDependents = numDependents;

    ON_LOG_VERBOSE(
        printf("\nEqPnt added(%3d):  Cid %5d, C=%2d, L=%2d, #dep=%d\n", eqSerie,
                curCntxt->GetContextId(), curCell, curLine, numDependents );
        printf  ("EqPnt added     : nCid %5d,nC=%2d,nL=%2d, type=%d\n\n",
                ( nbrCntxt != NULL ) ? nbrCntxt->GetContextId() : -999 ,
                nbrCell, nbrLine, type );
    )

    //
    // Add end point as 'to be communicated'
    //
    int mIndex=-1    , nIndex    =-1;
    int mVirtIndex=-1, nVirtIndex=-1;
    if (eqSeries[eqSerie].leftToRight)
    {
        mIndex     = curCell;
        nIndex     = curLine;
        mVirtIndex = curCell+1;
        nVirtIndex = curLine;
    }
    else
    {
        mIndex     = curLine;
        nIndex     = curCell;
        mVirtIndex = curLine;
        nVirtIndex = curCell+1;
    }
    curCntxt->AddCommPointAdmin(mIndex    , nIndex    , eqSeries[eqSerie].leftToRight);
    if ( type != Pnt_EndLine )
        curCntxt->AddCommPointAdmin(mVirtIndex, nVirtIndex, eqSeries[eqSerie].leftToRight);


    if ( nbrCntxt != NULL )
    {
        //
        // Add begin point in new domain as 'to be communicated'
        //
        int mIndex=-1, nIndex=-1;
        if (eqSeries[eqSerie].leftToRight)
        {
            mIndex     = nbrCell;
            nIndex     = nbrLine;
            mVirtIndex = nbrCell+1;
            nVirtIndex = nbrLine;
        }
        else
        {
            mIndex = nbrLine;
            nIndex = nbrCell;
            mVirtIndex = nbrLine;
            nVirtIndex = nbrCell+1;
        }
        nbrCntxt->AddCommPointAdmin(mIndex    , nIndex    , eqSeries[eqSerie].leftToRight);
        if ( type != Pnt_EndLine )
            nbrCntxt->AddCommPointAdmin(mVirtIndex, nVirtIndex, eqSeries[eqSerie].leftToRight);
    }

    //
    // Increase #points for this equation series
    //
    eqSeries[eqSerie].nPnt ++;

}


void Gaws::SetDependentLevels(
    int         curSer      // Current equation Series
    )
{
    int             othSer;     // loop counter other series
    int             found = 0;  // boolean to stop if series found
    int             p,oP;       // loopvars for points in other series
    int             numDep,ref; // #dependentPoints
    D3dFlowContextGawsSide   * curCntxt;   // context of current point
    D3dFlowContextGawsSide   * oCntxt;     // context of current point
    int             centreLine,curLine,curCell;
    int             oLine,oCell, refSerie;
        int             success = 1;

    //
    // Recursively set all refinement levels for all series
    // (Function is called for all lines with refinement level 0,
    //  and then calls itself).
    //
    // For all points in the current equation point series do
    //    Get the dependent points.
    //    For every dependent point do
    //       Find the equation series that contains that point
    //       Set the refinement level of that series to the
    //                caller's level plus one
    //       Call the SetDependentLevels function for that series
    //    enddo
    // enddo
    //

    for ( p = 0 ; (p < eqSeries[curSer].nPnt) ; p++ )
    {
        numDep =  eqSeries[curSer].pnt[p].numDependents;

        ON_LOG_VERBOSE(
            printf("serie[%d], #dep[%d]=%d\n", curSer, p, numDep);
        )

        if ( numDep != 0 )
        {
            if (numDep > 0 )
            {
                curCell     = eqSeries[curSer].pnt[p].cell;
                centreLine  = eqSeries[curSer].pnt[p].line;
                curCntxt    = eqSeries[curSer].pnt[p].curCntxt;
            }
            else
            {
                curCell     = eqSeries[curSer].pnt[p].nbrCell;
                centreLine  = eqSeries[curSer].pnt[p].nbrLine;
                curCntxt    = eqSeries[curSer].pnt[p].nbrCntxt;
            }

            ON_LOG_VERBOSE(
                printf("%d(%d,%d) %s\n", curCntxt->GetContextId(),
                        curCell, centreLine, (numDep > 0) ? "F2C" : "C2F" );
            )

            ref = (numDep >= 0) ? numDep : -numDep;

            for ( curLine = centreLine - ref/2 ;
                        curLine <= centreLine + (ref-1)/2 ; curLine ++ )
            {
                if ( curLine != centreLine )
                {
                    found    = 0;
                    refSerie = YET_TO_INIT;
                    for ( othSer = 0 ; (othSer<nSeries) && !found ; othSer++ )
                    {
                        ON_LOG_VERBOSE(
                            printf("   OS1: %d\n", othSer);
                        )

                        if ( ( othSer != curSer                  ) &&
                             ( eqSeries[othSer].level       != 0 ) &&
                             ( eqSeries[othSer].leftToRight ==
                               eqSeries[curSer].leftToRight   )  )
                        {
                            ON_LOG_VERBOSE(
                                printf("   OS2: %d\n", othSer);
                            )

                            for ( oP=0 ; (oP<eqSeries[othSer].nPnt)
                                        && !found         ; oP++ )
                            {
                                if (numDep > 0 )
                                {
                                    oCell  =
                                        eqSeries[othSer].pnt[oP].cell;
                                    oLine  =
                                        eqSeries[othSer].pnt[oP].line;
                                    oCntxt =
                                        eqSeries[othSer].pnt[oP].curCntxt;
                                }
                                else
                                {
                                    oCell  =
                                        eqSeries[othSer].pnt[oP].nbrCell;
                                    oLine  =
                                        eqSeries[othSer].pnt[oP].nbrLine;
                                    oCntxt =
                                        eqSeries[othSer].pnt[oP].nbrCntxt;
                                }
                                if ( oCell  == curCell &&
                                     oLine  == curLine &&
                                     oCntxt == curCntxt )
                                {
                                    found = 1;
                                    refSerie = othSer;

                                    ON_LOG_VERBOSE(
                                        printf("         %d(%d,%d)\n",
                                             (oCntxt != NULL) ?
                                             oCntxt->GetContextId() : -999,
                                             oCell, oLine);
                                    )
                                }
                            }
                        }
                    }
                    if ( found )
                    {
                        eqSeries[refSerie].level = eqSeries[curSer].level + 1;
                        maxRefineLevel[ eqSeries[refSerie].leftToRight ] =
                        B_MAX(maxRefineLevel[ eqSeries[refSerie].leftToRight ],
                                eqSeries[refSerie].level);

                        ON_LOG_VERBOSE(
                            printf("OtherS to refine: %d (level %d)\n", refSerie,
                                eqSeries[refSerie].level);
                        )

                        SetDependentLevels(refSerie);
                    }
                    else
                    {
                        fprintf(stderr,
                            "ERROR in SetDependentLevels (serie: %d)\n",
                                curSer);
                                                success = 0;
                    }
                }
            }
        }
    }
        if ( success == 0 )
        {
        throw new Exception (true, "Aborting due to errors in GAWS::SetDependentLevels.\n");
        }
}


void Gaws::RefineSeries(void)
{
    int s;          // serie loop counter

    ON_LOG_VERBOSE2(
        int l;
        int p;
    )

    //
    // For all series of level 0, that have children, refine their children.
    //

    for ( s = 0 ; (s < nSeries) ; s++ )
    {
        ON_LOG_VERBOSE(
            printf("Refine Serie[%3d], level %d (has dep: %d)\n", s,
                        eqSeries[s].level, eqSeries[s].hasDependents);
        )

        if ( eqSeries[s].level == 0 ) // TODO: && eqSeries[s].hasDependents )
        {
            SetDependentLevels(s);
        }
    }

    ON_LOG_WANG(
        printf("maxRefineLevels: LTR %d B2T %d \n",
                maxRefineLevel[1], maxRefineLevel[0]);
    )

    ON_LOG_VERBOSE2(
        for ( l = 0 ; l < B_MAX(maxRefineLevel[1], maxRefineLevel[0]) ; l++ )
        {
            for ( s = 0 ; (s < nSeries) ; s++ )
            {
                if ( eqSeries[s].level == l )
                {
                    printf("Ready Serie[%3d], level %d (%s)\n", s, l,
                            eqSeries[s].leftToRight ? "L2R" : "B2T");
                    for ( p = 0 ; (p < eqSeries[s].nPnt) ; p++ )
                    {
                        printf("           %5d(%3d,%3d) -> %5d(%3d,%3d)\n",
                        (eqSeries[s].pnt[p].curCntxt != NULL ) ?
                            eqSeries[s].pnt[p].curCntxt->GetContextId() : -999,
                        eqSeries[s].pnt[p].cell,
                        eqSeries[s].pnt[p].line,
                        (eqSeries[s].pnt[p].nbrCntxt != NULL ) ?
                            eqSeries[s].pnt[p].nbrCntxt->GetContextId() : -999,
                        eqSeries[s].pnt[p].nbrCell,
                        eqSeries[s].pnt[p].nbrLine);
                    }
                }
            }
        }
    )
}



int Gaws::ProcessCouplePoint(
    D3dFlowContextGawsSide       * curCntxt,
    bool        leftToRight,
    int         eqSerie,
    int         curCell,
    int         curLine
    )
{
    D3dFlowContextGawsSide   * nbrCntxt;
    int             nbrCell;
    int             nbrLine;
    int             centreLine;
    int             numDep;
    int             ll;
    int             retVal=HY_OK;

    retVal = this->FindNeighborPoint(
                        leftToRight,
                        curCntxt, curCell, curLine,
                        &nbrCell, &nbrLine, &nbrCntxt);

    if ( retVal != HY_OK )
    {
        throw new Exception (true, "ERROR returned in GAWS::ProcessCouplePoint on calling FindNeighborPoint.\n");
    }

    numDep = CheckDependents(  curCntxt,nbrCntxt,
                   leftToRight, curLine, &centreLine);

    if ( numDep >= 0 )
    {
        //
        // curCntxt is fine One, or 1:1
        //
        if ( curLine == centreLine )
        {
            AddEqPoint( eqSerie,
                curCntxt, curCell-1, curLine,
                nbrCntxt, nbrCell, nbrLine,
                Pnt_Regular, numDep);

            retVal = this->ContinueInNextContext(
                eqSerie, nbrCell, nbrLine, nbrCntxt);
            if ( numDep > 0 )
            {
                eqSeries[eqSerie].hasDependents = true;
            }
        }
        else
        {
            AddEqPoint( eqSerie,
                curCntxt, curCell-1, curLine,
                nbrCntxt, nbrCell, nbrLine,
                Pnt_EndRefine, 0);
            eqSeries[eqSerie].level = -1;
        }

        if ( retVal != HY_OK )
        {
            throw new Exception (true, "ERROR returned in GAWS::ProcessCouplePoint on calling AddEqPoint.\n");
        }
    }
    else
    {
        //
        // nbrContext is fine One
        //
        for ( ll = centreLine + numDep/2 ;
            ll <= centreLine - (numDep+1)/2 ; ll++ )
        {
            if ( ll == centreLine )
            {
                AddEqPoint( eqSerie,
                            curCntxt, curCell-1, curLine,
                            nbrCntxt, nbrCell, ll,
                            Pnt_Regular, numDep);
                retVal = ContinueInNextContext(eqSerie, nbrCell, ll, nbrCntxt);
                eqSeries[eqSerie].hasDependents = true;
            }
            else
            {
                int newSerie = NewEquationSerie(leftToRight,-1);

                ON_LOG_VERBOSE(
                    printf("newSerie (nbrFine): %d\n", newSerie);
                )

                AddEqPoint( newSerie,
                            curCntxt, curCell-1, curLine,
                            nbrCntxt, nbrCell, ll,
                            Pnt_StartRefine, 0);

                retVal = ContinueInNextContext(newSerie,
                        nbrCell, ll, nbrCntxt);
            }
        }
    }
    return retVal;
}


void Gaws::InitializeMapperInfo(void)
{
    //
    // find all mappers by looping over flow neighbors
    // stored required info for each mapper
    //

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "GAWS \"%s\" intializes mapper info", this->gawsIterator->name);

    Category * d3dFlowProcs = LookupCategory (DD::processCategoryName);
    Category * mappers      = LookupCategory (DD::mapperCategoryName);

    if (mappers == NULL)
        throw new Exception (true, "\"mapper\" category does not exist (GAWS: \"%s\")", this->gawsIterator->name);

    if (d3dFlowProcs == NULL)
        throw new Exception (true, "\"process\" category does not exist for (GAWS: \"%s\")", this->gawsIterator->name);

    int numD3dFlowProcs = this->gawsIterator->NeighborCount (d3dFlowProcs);

    if (numD3dFlowProcs >= MAX_NUM_D3D_FLOW_PROCS)
        throw new Exception (true, "GAWS iterator \"%s\" has too many d3dflow processes", this->gawsIterator->name);

    this->gawsIterator->RewindNeighbors(d3dFlowProcs);

    for ( int p = 0 ; p < numD3dFlowProcs ; p++ )
    {
        Iterator * flow = this->gawsIterator->NextNeighbor(d3dFlowProcs);
        int numFlowMappers = flow->NeighborCount(mappers);

        flow->RewindNeighbors(mappers);
        {for ( int m = 0 ; m < numFlowMappers; m++ )
        {
            Iterator * mapper = flow->NextNeighbor(mappers);

            bool allReadyInMapInfo = false;

            {for ( int map = 0 ; ( map < this->numMappers ) && (! allReadyInMapInfo ) ; map++ )
                if ( this->mapInfo[map].mapperIterator->id == mapper->id )
                {
                    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\": mapper \"%s\" already added", this->gawsIterator->name, mapper->name);
                    allReadyInMapInfo = true;
                }
            }

            if ( ! allReadyInMapInfo )
            {

                if ( this->numMappers >= MAX_GAWS_MAP )
                {
                    throw new Exception (true, "GAWS iterator \"%s\" found too many mappers", this->gawsIterator->name);
                }
                else
                {

                    int curM = this->numMappers;

                    this->mapInfo[curM].mapperIterator    = mapper;

                    char * mapperConfig = (char *) mapper->configblob->Address();
                    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS: mapper \"%s\", configString \"%s\"",
                                mapper->name, mapperConfig);

                    strcpy(this->mapInfo[curM].configStr, mapperConfig);

                    mapper->RewindNeighbors(d3dFlowProcs);
                    this->mapInfo[curM].flowIterator[C_0] = mapper->NextNeighbor(d3dFlowProcs);
                    this->mapInfo[curM].flowIterator[C_1] = mapper->NextNeighbor(d3dFlowProcs);

                    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "GAWS \"%s\" added mapper \"%s\" (flow \"%s\" -> \"%s\"), configString \"%s\"",
                            this->gawsIterator->name, mapper->name,
                            this->mapInfo[curM].flowIterator[C_0]->name,
                            this->mapInfo[curM].flowIterator[C_1]->name,
                            this->mapInfo[curM].configStr );

                    this->numMappers++;
                }
            }
        }}
    }

    if (this->numMappers < 1)
        throw new Exception (true, "GAWS iterator \"%s\" did not find any mappers", this->gawsIterator->name);
    else
        FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "GAWS \"%s\" found %d mappers", this->gawsIterator->name, this->numMappers);
}
