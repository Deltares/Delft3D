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
// $Id: mapper_config.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/mapper_config.cpp $
//------------------------------------------------------------------------------
// Configuration object for Flow2D3D Domain Decomposition
//
//  Stef.Hummel@deltares.nl
//  Adri.Mourits@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define LOG_CELLS   0


//////////////////////////////////////////////////////////////////////
//
// Parse configuration string
//


int
ParseMapperConfigString (
    char      * configString,           // config string in DD-Bound file
    EdgeType    edgeType  [NR_CNTXTS],  // Out: edge types left / right domein
    int         firstCell [NR_CNTXTS],  // Out: first cell left / right domein
    int         lastCell  [NR_CNTXTS],  // Out: last cell left / right domein
    int         normalCell[NR_CNTXTS],  // Out: normal cell left / right domein
    int         refine    [NR_CNTXTS],  // Out: refinement of contexts
    int         echoRefinement          // In : 1: echo refinement to file
    )
{
    int         iii;                // auxiliary integer variable
    int         retVal = HY_OK;     // return value
        FILE      * refinementFile;     // file to pass refinement factor to flow
    char      * refFileName = "TMP_refinement";

    int mStartLeft , nStartLeft , mEndLeft , nEndLeft;
    int mStartRight, nStartRight, mEndRight, nEndRight;
    char dumNameLeft[200], dumNameRight[200];

    int numRead = sscanf(configString, "%s %d %d %d %d %s %d %d %d %d",
                            dumNameLeft , &mStartLeft , &nStartLeft , &mEndLeft , &nEndLeft,
                            dumNameRight, &mStartRight, &nStartRight, &mEndRight, &nEndRight );
    if (numRead != 10)
        throw new Exception (true, "Cannot parse configString \"%s\" (numRead = %d)", configString, numRead);
    else
    {
        if ( mStartLeft == mEndLeft )
        {
            if ( mStartRight == mStartRight )
            {
                edgeType[C_0] = Edge_Right;
                edgeType[C_1] = Edge_Left;
            }
            else
            {
                throw new Exception (true, "Inconstistent Mapper direction configString \"%s\"", configString);
            }
        }
        else if ( nStartLeft == nEndLeft )
        {
            if ( nStartRight == nStartRight )
            {
                edgeType[C_0] = Edge_Top;
                edgeType[C_1] = Edge_Bottom;
            }
            else
            {
                throw new Exception (true, "Inconstistent napper direction configString \"%s\"", configString);
            }
        }
        else
        {
            throw new Exception (true, "M1/M2 or N1/N2 must be equal in configString \"%s\"", configString);
        }
    }

    //
    // Set mapper start/end/normal cell indices
    //

    switch ( edgeType[C_0] )
    {
        case Edge_Top:
            //
            // top->bottom mapper
            //
            normalCell[C_0] = nStartLeft ;
            normalCell[C_1] = nStartRight;

            firstCell [C_0] = mStartLeft ;
            lastCell  [C_0] = mEndLeft   ;
            firstCell [C_1] = mStartRight;
            lastCell  [C_1] = mEndRight  ;

            break;

        case Edge_Right:
            //
            // right->left mapper
            //
            normalCell[C_0] = mStartLeft ;
            normalCell[C_1] = mStartRight;

            firstCell [C_0] = nStartLeft ;
            lastCell  [C_0] = nEndLeft   ;
            firstCell [C_1] = nStartRight;
            lastCell  [C_1] = nEndRight  ;

            break;

        default:

            throw new Exception (true, "Error: Determined wrong edgeType from configString \"%s\"", configString);
            break;
    }

    //  if firstCell>lastCell then switch coordinates of firstCell and lastCell
    {
        if ( firstCell [C_0] > lastCell [C_0] )
        {
            iii = firstCell [C_0];
            firstCell [C_0] = lastCell [C_0];
            lastCell  [C_0] = iii;
            printf("switch coordinates-1: (%2d,%2d)\n", firstCell[C_0],lastCell[C_0]);
            fflush (stdout);
        }
        if ( firstCell [C_1] > lastCell [C_1] )
        {
            iii = firstCell [C_1];
            firstCell [C_1] = lastCell [C_1];
            lastCell  [C_1] = iii;
            printf("switch coordinates-2: (%2d,%2d)\n", firstCell[C_1],lastCell[C_1]);
            fflush (stdout);
        }
    }
        if ( echoRefinement == 1) {
        if ((refinementFile = fopen (refFileName, "a")) == NULL) {
            throw new Exception (true, "Cannot open refinement file \"%s\" for writing.", refFileName);
        }
        }

    for ( int ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        int oCtx = 1 - ctx;
        if ( lastCell[oCtx] == firstCell[oCtx] )
        {
            throw new Exception (true, "Error in determining refinements, configString\"%s\"", configString);
        }
        refine[ctx] = B_MAX ( 1,  ( lastCell[ ctx] - firstCell[ ctx] ) /
                                  ( lastCell[oCtx] - firstCell[oCtx] )   );
        if ( echoRefinement == 1 ) {
                    fprintf(refinementFile, "%d :%s", refine[ctx], configString);
                }
    }

    if ( echoRefinement == 1 ) {
        fclose (refinementFile);
        }

        return retVal;
}


//////////////////////////////////////////////////////////////////////
//
// D3dFlowMapper Configuration functions
//


int D3dFlowMapper::InitAndParseConfigString(
    char      * configString        // config string
    )
{
    int         retVal = HY_OK;     // return value

    //
    // Set Valid defaults
    //

    // Values for Block Jacobi convergence criterion:
    this->EpsMap[Eq_U]    = 1.0e-4;
    this->EpsMap[Eq_V]    = 1.0e-4;
    this->EpsMap[Eq_Zeta] = 1.0e-2;   // used for concentrations and not for the
                               // water elevation because of Wang Algorithm
    this->MaxIter_Vel     = 5;
    this->MaxIter_Conc    = 5;
    this->MaxIter_2DAD    = 5;

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        //
        // Valid defaults
        //

        this->Ref[ctx]       = 1;

        //
        // Defaults indicating "Not Read from config file"
        //

        this->Edge[ctx]      = NR_EDGETYPES;
        this->FirstCell[ctx] = YET_TO_INIT;
        this->LastCell[ctx]  = YET_TO_INIT;
        this->NormalCell[ctx]= YET_TO_INIT;
    }


    retVal = ParseMapperConfigString(   configString, this->Edge,
                                        this->FirstCell, this->LastCell,
                                        this->NormalCell, this->Ref, 1);

    if ( retVal != HY_OK )
    {
        throw new Exception (true, "Call to configStringParser failed for configString \"%s\"", configString);
    }

    //
    // TODORE: Check for config errors (see old conf-parser)
    //

    return retVal;

}


int D3dFlowMapper::CheckConfig()
{
    int     retVal           = HY_OK;
    int     check[NR_CNTXTS] = { HY_OK, HY_OK };
    int     checkBoth        = HY_OK;

    MAPDBG_FUN2("D3dFlowMapper::CheckConfig");

    //
    // Check configuration information
    //

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        check[ctx] = CheckConfigContext(ctx);
    }

    checkBoth = CheckConfigBothContexts();

    if (    check[C_0] == HY_ERR
         || check[C_1] == HY_ERR
         || checkBoth  == HY_ERR  )
    {
        throw new Exception (true, "Errors found in config file %s", this->confFile);
        retVal = HY_ERR;
    }

    return retVal;
}


int D3dFlowMapper::GetStartCell(
    int         aCtx,   // current context
    int         eq      // equation type
    )
{
    int     startCell=YET_TO_INIT;   // last map-cell-nr for this aCtx/eq
    Vel     orient;                  // orientation indicating Norm.-Vel.
                                     // or Tang.-Vel., or Zeta

    orient = GetVelocityOrientation(aCtx, eq);

    switch (orient)
    {
    case Vel_Zeta:
    case Vel_Norm:
    case Vel_Tang:
            startCell = FirstCell[aCtx] + 1;
            break;
    default:
            throw new Exception (true, "Unexpected case (%d) in GetStartCell", orient);
            break;
    }
#if LOG_CELLS
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "StartCell for eq %d: %3d", eq, startCell);
#endif
    return startCell;
}


int D3dFlowMapper::GetEndCell(
    int         aCtx,   // current context
    int         eq      // equation type
    )
{
    int     endCell=YET_TO_INIT; // last map-cell-nr for this aCtx/eq
    Vel     orient;              // orientation indicating Norm.-Vel.
                                 // or Tang.-Vel., or Zeta

    orient = GetVelocityOrientation(aCtx, eq);

    switch (orient)
    {
    case Vel_Zeta:
    case Vel_Norm:
            endCell = LastCell[aCtx];
            break;
    case Vel_Tang:
            endCell = LastCell[aCtx] - 1;
            break;
    default:
            throw new Exception (true, "Unexpected case (%d) in GetEndCell", orient);
            break;
    }
#if LOG_CELLS
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "EndCell for eq %d: %3d", eq, endCell);
#endif
    return endCell;
}


int D3dFlowMapper::GetNormalCell(
    int         aCtx,   // current context
    int         eq, // equation type
    CtxType     type    // source or target
    )
{
    int     normCell=YET_TO_INIT; // cell-nr in normal direction
    Vel     orient;               // orientation indicating Norm.-Vel.
                                  // or Tang.-Vel.

    MAPDBG_FUN2("D3dFlowMapper::GetNormalCell");

    orient = GetVelocityOrientation(aCtx, eq);

    if ( orient == Vel_Norm )
    {
        // set virtual point on Right/Top edge *ON* the interface
        if (    Edge[aCtx] == Edge_Left
             || Edge[aCtx] == Edge_Bottom   )
        {
            if ( type == CtxType_Source  )
            {
                normCell = NormalCell[aCtx];
            }
            else // target
            {
                normCell = NormalCell[aCtx] - 1;
            }
        }
        else // Right or Top Edge
        {
            if ( type == CtxType_Source  )
            {
                normCell = NormalCell[aCtx] - 1;
            }
            else // target
            {
                normCell = NormalCell[aCtx];
            }
        }
    }
    else if ( orient == Vel_Tang || orient == Vel_Zeta )
    {
        if (    (    (    Edge[aCtx] == Edge_Left
                   || Edge[aCtx] == Edge_Bottom   )
                  && (    type == CtxType_Source   ) )
                 ||
            (    (    Edge[aCtx] == Edge_Right
                   || Edge[aCtx] == Edge_Top      )
                  && (    type == CtxType_Target   ) ) )
        {
            normCell = NormalCell[aCtx] + 1;
        }
        else
        {
            normCell = NormalCell[aCtx];
        }
    }

    if ( normCell == YET_TO_INIT )
    {
        throw new Exception (true, "Invalid NormalCell in GetNormalCell");
    }

    return normCell;
}


int D3dFlowMapper::CheckConfigContext(
    int     aCtx            // current context
    )
{
    int     retVal = HY_OK;
    int     domMaxCell;     // domains maximum cell for mapper (mMax/nMax)

    MAPDBG_FUN2("D3dFlowMapper::CheckConfigContext");

    if ( Edge[aCtx] == NR_EDGETYPES )
    {
        retVal = HY_ERR;
        FLOW2D3D->dd->log->Write (Log::WARN, "Edge type not available in config file (ctx %d)", aCtx);
    }

    if (    ( FirstCell[aCtx] < 1 )
         || ( LastCell[aCtx]  < 1 ) )
    {
        retVal = HY_ERR;
        FLOW2D3D->dd->log->Write (Log::WARN, "Couldn't get First or Last cell from config file (ctx %d)", aCtx);
    }

    if (    Edge[aCtx] == Edge_Bottom
         || Edge[aCtx] == Edge_Top    )
    {
        domMaxCell = C[aCtx]->mMax;
    }
    else
    {
        domMaxCell = C[aCtx]->nMax;
    }


    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "Checking configuration for Context %d", aCtx );

    if (   (Edge[aCtx] < 0) || (Edge[aCtx] >= NR_EDGETYPES) )
    {
        retVal = HY_ERR;
        FLOW2D3D->dd->log->Write (Log::WARN, "Couldn't get Edge type  from config file (ctx %d)", aCtx);
    }

    if ( FirstCell[aCtx] >= LastCell[aCtx] )
    {
        retVal = HY_ERR;
        FLOW2D3D->dd->log->Write (Log::WARN, "FirstCell must be < LastCell");
    }

    if ( NormalCell[aCtx] == YET_TO_INIT )
    {
        retVal = HY_ERR;
        FLOW2D3D->dd->log->Write (Log::WARN, "NormalCell must be Specified");
    }

    if ( FirstCell[aCtx] < 1 )
    {
        FLOW2D3D->dd->log->Write (Log::WARN, "First Cell (%d) < 1", FirstCell[aCtx]);
        retVal = HY_ERR;
    }

    if ( LastCell[aCtx] > domMaxCell )
    {
        FLOW2D3D->dd->log->Write (Log::WARN, "Last Cell (%d) for \"%s\"-\"%s\" exceeds maximum (%d)", LastCell[aCtx],
                            this->C[aCtx]->mapperIterator->name,
                            this->C[aCtx]->flowIterator->name,
                            domMaxCell );
        retVal = HY_ERR;
    }

    return retVal;

}


int D3dFlowMapper::CheckConfigBothContexts(void)
{
    int     retVal = HY_OK;     // return value

    MAPDBG_FUN2("D3dFlowMapper::CheckConfigBothContexts");

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MAJOR, "Checking configuration between Contexts");

    if ( (Ref[C_0] != 1) && (Ref[C_1] != 1) )
    {
        FLOW2D3D->dd->log->Write (Log::WARN, "One of the Refinement factors must be 1");
        retVal = HY_ERR;
    }

    if (    ( (Edge[C_0] == Edge_Left) && (Edge[C_1] != Edge_Right ) )
     || ( (Edge[C_0] == Edge_Right) && (Edge[C_1] != Edge_Left ) )
     || ( (Edge[C_0] == Edge_Top) && (Edge[C_1] != Edge_Bottom ) )
     || ( (Edge[C_0] == Edge_Bottom) && (Edge[C_1] != Edge_Top ) )
       )
    {
        FLOW2D3D->dd->log->Write (Log::WARN, "Inconsistent Edges");
        retVal = HY_ERR;
    }

    return retVal;

}


Vel D3dFlowMapper::GetVelocityOrientation(
    int         aCtx,   // current context
    int         eq  // equation type
    )
{
    Vel retVal;

    MAPDBG_FUN2("D3dFlowMapper::GetVelocityOrientation");

    //
    // Determine the orientation of the equation to be solved
    //
    // Left or Right:   U:  tangential
    //          V:  normal
    //
    // Top or Bottom:   U:  normal
    //          V:  tangential
    //

    if ( eq == Eq_Zeta )
    {
        retVal = Vel_Zeta;
    }
    else
    {
        if ( Edge[aCtx] == Edge_Left || Edge[aCtx] == Edge_Right )
        {
             retVal = ( eq == Eq_U ) ? Vel_Norm : Vel_Tang ;
        }
        else
        {
             retVal = ( eq == Eq_U ) ? Vel_Tang : Vel_Norm ;
        }
    }

    return retVal;

}


void D3dFlowMapper::CentreCellOtherContext(
    int         aCtx,       // current context
    int         curCentre,  // current centre cell
    int           * otherCentre // O: centre cell other context
    )
{

    MAPDBG_FUN2("D3dFlowMapper::CentreCellOtherContext");

    //
    // Determine centre cells (zeta direction and M/N direction)
    // in other context
    //

    *otherCentre = Other_MN_Centre( aCtx, curCentre);
}


int D3dFlowMapper::Other_MN_Centre(
    int         aCtx,       // current context
    int         curCentre   // current centre cell
    )
{
    int     oCtx = 1 - aCtx;       // other context
    int     relCell;        // relative number of current cell
                    // in other context
                    // (i.e. as offset from start cell)
    int     otherCentre;        // centre cell in other context
    int     shift = 0 ;     // should indices be shifted?
                    // (Yes in case of refinement)
    Vel     orient;         // orientation indicating Norm.-Vel.
                    // or Tang.-Vel.

    MAPDBG_FUN2("D3dFlowMapper::Other_MN_Centre");

    // Determine centre cell (M/N-direction) in other context
    // If there is a refinement, the cells for Norm.-Vel. or
    // for zeta points are shifted with half the refinement
    // factor, to take care that the relevant 'neigbor-cells'
    // are delivered.
    //

    orient = GetVelocityOrientation(aCtx, Eq_Zeta);

    assert( orient == Vel_Zeta );

    shift = 1;

    relCell     = curCentre - FirstCell[aCtx];

    if ( Ref[oCtx] > Ref[aCtx] )
    {
    //
    // Other context is fine one
    // Centre Cell in other context is found by multiplication.
    // If Norm.-Vel. or Zeta stencil is required,
    // shift stencil down with half of the Refinement factor.
    //

    otherCentre =    FirstCell[oCtx]  +  relCell * Ref[oCtx];

    if ( shift )
    {
        otherCentre -= Ref[oCtx] / 2;
    }

    }
    else if ( Ref[oCtx] < Ref[aCtx] )
    {
    //
    // other context is coarse one
    //
    // If Norm.-Vel. or Zeta stencil is required,
    // shift incoming centre cell with half of the Refinement factor.
    // Centre Cell in other context is found by devision.
    //

    if ( shift )
    {
        relCell -= Ref[aCtx] / 2 ;
    }
    otherCentre = FirstCell[oCtx]  +  relCell / Ref[aCtx];

    }
    else
    {
    //
    // One to one
    // Centre Cell in current context is equivelent to cell in
    // other context
    //
    otherCentre = FirstCell[oCtx]  +  relCell;
    }

    return otherCentre;
}

