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
// $Id: mapper_uvz.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/mapper_uvz.cpp $
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


#define fabs(a) ((a) <= 0. ? -(a) : (a))

#define EPSILON  (1.0e-10)      // for (in)equality tests with zero


////////////////////////////////////////////////////////////////////////////
//
// Protected Functions for KS/KF/Label arrays
//


void D3dFlowMapper::InitKc(
    Eq          eq      // equation type
    )
{
    MAPDBG_FUN("D3dFlowMapper::InitKc");

    //
    // If there are no coupling points for this equation type,
    // don't do anything.
    //
    if ( ( mEnd[C_0][eq] < mStart[C_0][eq] ) ||
         ( nEnd[C_0][eq] < nStart[C_0][eq] )    )
    {
        FLOW2D3D->dd->log->Write (Log::WARN, "Map-length <= 0 for equation %d: m1=%d > m2=%d or n1=%d > n2=%d",
                     eq, mStart[C_0][eq], mEnd[C_0][eq], nStart[C_0][eq], nEnd[C_0][eq] );
        return;
    }

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        //
        // Set KC?-flags to 3 to 1 for current equation
        //

        SET_OTHER_CTX;

        MAP_CELLS_LOOP(ctx,eq)
        {
            switch(eq)
            {
            case Eq_U:
                // printf("KCU=3: (%2d,%2d) %d\n", m, n,ctx);
                cI2D(ctx,m,n,kcu) = 3;
                break;
            case Eq_V:
                // printf("KCV=3: (%2d,%2d) %d\n", m, n,ctx);
                cI2D(ctx,m,n,kcv) = 3;
                break;
            case Eq_Zeta:
                // printf("KCS=3: (%2d,%2d) %d\n", m, n,ctx);
                cI2D(ctx,m,n,kcs) = 3;
                break;
            default:
                throw new Exception (true, "unexpected case (%d) InitKc", eq);
            }
        }

        //
        // Set KC?-flags and KF?-flags to 1 *on* interface line
        // for Normal Velocity equation.
        //

        if (    ( eq == Eq_U             )
             && ( Edge[ctx] == Edge_Left ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
                // printf("KCU=1: (%2d,%2d) %d\n", m+1, n, ctx);
                cI2D(ctx,m+1, n, kcu) = 1;
            }
        }

        if (    ( eq == Eq_U              )
             && ( Edge[ctx] == Edge_Right ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
                // printf("KCU=3: (%2d,%2d) %d\n", m+1, n, ctx);
                cI2D(ctx,m+1, n, kcu) = 3;
            }
        }

    //
    // Reset KCU in case of grid refinement
    //
    if (    ( eq == Eq_U             )
         && ( Edge[ctx] == Edge_Left )
         && ( Ref[ctx] < Ref[oCtx] ) )
    {
        MAP_CELLS_LOOP(ctx,eq)
        {
        // printf("reset coarse KCU=3: (%2d,%2d) %d\n", m+1, n, ctx);
        cI2D(ctx,m+1, n, kcu) = 3;
        }
    }

    if (    ( eq == Eq_U             )
         && ( Edge[ctx] == Edge_Right )
         && (  Ref[ctx] > Ref[oCtx] ) )
    {
        MAP_CELLS_LOOP(ctx,eq)
        {
        // printf("reset fine KCU=1: (%2d,%2d) %d\n", m, n, ctx);
        cI2D(ctx,m, n, kcu) = 1;
        }
    }
        if (    ( eq == Eq_V               )
             && ( Edge[ctx] == Edge_Bottom ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
                // printf("KCV=1: (%2d,%2d) %d\n", m, n+1, ctx);
                cI2D(ctx,m, n+1, kcv) = 1;
            }
        }

        if (    ( eq == Eq_V            )
             && ( Edge[ctx] == Edge_Top ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
                // printf("KCV=3: (%2d,%2d) %d\n", m, n+1, ctx);
                cI2D(ctx,m, n+1, kcv) = 3;
            }
        }
        //
        // Reset KCV in case of grid refinement
        //
        if (    ( eq == Eq_V               )
             && ( Edge[ctx] == Edge_Bottom )
             && (  Ref[ctx] < Ref[oCtx] ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
            // printf("reset coarse KCV=3: (%2d,%2d) %d\n", m, n+1, ctx);
            cI2D(ctx,m, n+1, kcv) = 3;
            }
        }

        if (    ( eq == Eq_V             )
             && ( Edge[ctx] == Edge_Top )
             && (  Ref[ctx] > Ref[oCtx] ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
            // printf("reset fine KCV=1: (%2d,%2d) %d\n", m, n, ctx);
            cI2D(ctx,m, n, kcv) = 1;
            }
        }

#if 1
        //
        //  Ad Hoc corrections for "inner corner" problem
        //
        if (    ( eq == Eq_Zeta             )
             && ( Edge[ctx] == Edge_Left )
             && ( nStart[ctx][Eq_Zeta] < nEnd[ctx][Eq_Zeta] ) )
        {
           cI2D(ctx, mStart[ctx][Eq_Zeta]-1, nStart[ctx][Eq_Zeta], kcs) = 3;
           cI2D(ctx, mStart[ctx][Eq_Zeta]-1, nEnd[ctx][Eq_Zeta]  , kcs) = 3;
           // printf ("extra kcs=3 x1 %3d %3d\n",mStart[ctx][Eq_Zeta]-1, nStart[ctx][Eq_Zeta]);
           // printf ("extra kcs=3 x2 %3d %3d\n",mStart[ctx][Eq_Zeta]-1, nEnd[ctx][Eq_Zeta]  );
        }
        if (    ( eq == Eq_Zeta             )
             && ( Edge[ctx] == Edge_Bottom )
             && ( mStart[ctx][Eq_Zeta] < mEnd[ctx][Eq_Zeta] ) )
        {
           cI2D(ctx, mStart[ctx][Eq_Zeta], nStart[ctx][Eq_Zeta]-1, kcs) = 3;
           cI2D(ctx, mEnd[ctx][Eq_Zeta]  , nStart[ctx][Eq_Zeta]-1, kcs) = 3;
           // printf ("extra kcs=3 y1 %3d %3d\n",mStart[ctx][Eq_Zeta], nStart[ctx][Eq_Zeta]-1);
           // printf ("extra kcs=3 y2 %3d %3d\n",mEnd[ctx][Eq_Zeta]  , nStart[ctx][Eq_Zeta]-1);
        }
#endif

    }
}


void D3dFlowMapper::CheckDDbounds(
    Eq          eq      // equation type
    )
{
    int                 KcsLeft;        // KCS value of left/bottom neighbour
    int                 KcsRight;       // KCS value of right/top neighbour
    char * iniErrorMessage = "\n1) Error during initialization of subdomains: check tri-diag files.\nOR\n2) ";

    MAPDBG_FUN("D3dFlowMapper::CheckDDbounds");

    //
    // Check whether the coupling points (i.e. points with KCS=3)
    // are at a correct location.
    // This is checked as follows: there should be a neighbour with kcs=1,
    // whereas for the coupling point itself the kcs value should be equal to 0
    //
    // Noted that the equation type is always "Eq_Zeta"
    //
    // Noted that CheckDDbound is done before InitKc

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        SET_OTHER_CTX;


        MAP_CELLS_LOOP(ctx,eq)
        {

            if ( Edge[ctx] == Edge_Left )
            {
                KcsLeft  = cI2D(ctx,m  , n, kcs);
                KcsRight = cI2D(ctx,m+1, n, kcs);
                if ( (KcsLeft != 0) || (KcsRight != 1 ) )
                {
                    throw new Exception (true, "%s\"%s\": Wrong coupling point at (%2d,%2d) Kcs=%d KcsInside=%d",
                                iniErrorMessage, this->C[ctx]->flowIterator->name,
                                m, n, KcsLeft, KcsRight);
                }
            }

            if ( Edge[ctx] == Edge_Right )
            {
                KcsLeft  = cI2D(ctx,m-1, n, kcs);
                KcsRight = cI2D(ctx,m  , n, kcs);
                if ( (KcsLeft != 1) || (KcsRight != 0 ) )
                {
                    throw new Exception (true, "%s\"%s\": Wrong coupling point at (%2d,%2d) Kcs=%d KcsInside=%d",
                                iniErrorMessage,this->C[ctx]->flowIterator->name,
                                m, n, KcsLeft, KcsRight);
                }
            }

            if ( Edge[ctx] == Edge_Bottom )
            {
                KcsLeft  = cI2D(ctx,m, n  , kcs);
                KcsRight = cI2D(ctx,m, n+1, kcs);
                if ( (KcsLeft != 0) || (KcsRight != 1 ) )
                {
                     throw new Exception (true, "%s\"%s\": Wrong coupling point at (%2d,%2d) Kcs=%d KcsInside=%d",
                                iniErrorMessage, this->C[ctx]->flowIterator->name,
                                m, n, KcsLeft, KcsRight);
                }
            }

            if ( Edge[ctx] == Edge_Top )
            {
                KcsLeft  = cI2D(ctx,m, n-1, kcs);
                KcsRight = cI2D(ctx,m, n  , kcs);
                if ( (KcsLeft != 1) || (KcsRight != 0 ) )
                {
                    throw new Exception (true, "%s\"%s\": Wrong coupling point at (%2d,%2d) Kcs=%d KcsInside=%d",
                                iniErrorMessage, this->C[ctx]->flowIterator->name,
                                m, n, KcsLeft, KcsRight);
                }
            }
            // fflush(stdout);
        }
    }
}


void D3dFlowMapper::CheckDepthUV(
    Eq          eq      // equation type (U or V)
    )
{
    float               threshold;     // For warnings w.r.t. inconsistent depths

    MAPDBG_FUN("D3dFlowMapper::CheckDepthUV");

    assert ( eq == Eq_U || eq == Eq_V );

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        SET_OTHER_CTX;

        if ( Ref[ctx] == Ref[oCtx] )
        {
            threshold = 0.5;
        }
        else
        {
            threshold = 5.0;
        }

        if ( ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right )
             && ( eq == Eq_U            )
             && ( Ref[ctx] <= Ref[oCtx] ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
                int m2 = m;
                int oM2 = oM;
                if ( Edge[ctx] == Edge_Left )
                {
                    m2 ++;
                    oM2 ++;
                }

                MapScalar thisDpu  = cI2D(ctx,m2,n,dpu);
                MapScalar otherDpu = cI2D(oCtx,oM2,oN,dpu);
                if ( fabs(thisDpu - otherDpu) > threshold )
                {
                    printf("WARNING: Depth difference on couple boundary above %f\n",threshold);
                    printf("         context[%2d].DPU(%2d,%2d) = %f , context[%2d].DPU(%2d,%2d) = %f\n",
                        ctx, m2, n, thisDpu, oCtx, oM2, oN, otherDpu);
                    fflush(stdout);
                }
                // printf("QXK(%2d,%2d): %f other QXK(%2d,%2d): %f\n",
                        // m2, n, qxk, oM2,oN, otherqxk);
                // printf("U1 (%2d,%2d): %f other U1 (%2d,%2d): %f\n",
                        // m2, n, uu , oM2,oN, otheruu );
                // printf("GUU (%2d,%2d): %f other GUU (%2d,%2d): %f\n",
                    // m2, n, guu , oM,oN, otherguu );
            }
        }

        if ( ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top )
             && ( eq == Eq_V            )
             && ( Ref[ctx] <= Ref[oCtx] ) )
        {
            MAP_CELLS_LOOP(ctx,eq)
            {
                int n2 = n;
                int oN2 = oN;
                if ( Edge[ctx] == Edge_Bottom )
                {
                    n2 ++;
                    oN2 ++;
                }

                MapScalar thisDpv  = cI2D(ctx,m,n2,dpv);
                MapScalar otherDpv = cI2D(oCtx,oM,oN2,dpv);
                if ( fabs(thisDpv - otherDpv) > threshold )
                {
                    printf("WARNING: Depth difference on couple boundary above %f\n",threshold);
                    printf("context[%2d].DPV(%2d,%2d) = %f , context[%2d].DPV(%2d,%2d) = %f\n",
                        ctx, m, n2, thisDpv, oCtx, oM, oN2, otherDpv);
                    fflush(stdout);
                }
            }
        }
    }
}


////////////////////////////////////////////////////////////////////////////
//
// Protected Functions for Setting/Copying Variables
//

void D3dFlowMapper::InitFlowVariables(void)
{
    int         eq;     // equation loop counter
    MapScalar   valueR0;  // value to copy
    MapScalar   valueR1;  // value to copy
    MapScalar   horAverageR0; // sum of refinement cells to be avaraged
    MapScalar   horAverageR1; // sum of refinement cells to be avaraged

    MAPDBG_FUN("D3dFlowMapper::InitFlowVariables");


    //
    // Exchange time independent FlowScalar data
    // between neigbours.


    for ( eq = 0 ; eq < NR_EQ ; eq++ )
    {
        for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
        {
            ON_DEBUG( DBLEV1,
                FMapLog((char*)"Exchange FLOW VARS (ctx %d, eqType: %d)\n", ctx, eq);
            )

            oCtx = 1 - ctx;

            //
            // Exchange U/V/Zeta initial data,
            // to ensure correct initial condition
            //

            switch(eq)
            {
            case Eq_U:
                if ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right )
                {
                    MAP_CELLS_LOOP(ctx,eq)
                    {
                        if ( Edge[ctx] == Edge_Right )
                        {
                            if (cI2D(ctx,m+1,n,kcu) == 3)
                            {
                                cI3D(ctx,m+1,n,1,cfurou) = cI3D(oCtx,oM+1,oN,1,cfurou);
                                cI3D(ctx,m+1,n,2,cfurou) = cI3D(oCtx,oM+1,oN,2,cfurou);
                                cI2D(ctx,m+1,n,z0urou) = cI2D(oCtx,oM+1,oN,z0urou);
                                MAP_LAYERS_LOOP(ctx)
                                {
                                    cI3D(ctx,m+1,n,k,u0) = cI3D(oCtx,oM+1,oN,oK,u0);
                                    cI3D(ctx,m+1,n,k,u1) = cI3D(oCtx,oM+1,oN,oK,u1);
                                }
                            }
                        }
                        //
                        if (cI2D(ctx,m,n,kcu) == 3)
                        {
                            cI3D(ctx,m,n,1,cfurou) = cI3D(oCtx,oM,oN,1,cfurou);
                            cI3D(ctx,m,n,2,cfurou) = cI3D(oCtx,oM,oN,2,cfurou);
                            cI2D(ctx,m,n,z0urou) = cI2D(oCtx,oM,oN,z0urou);
                            MAP_LAYERS_LOOP(ctx)
                            {
                                cI3D(ctx,m,n,k,u0) = cI3D(oCtx,oM,oN,oK,u0);
                                cI3D(ctx,m,n,k,u1) = cI3D(oCtx,oM,oN,oK,u1);
                            }
                        }
                    }
                    MAP_CELLS_LOOP(ctx,Eq_V)
                    {
                        if (cI2D(ctx,m,n,kcv) == 3 && nEnd[oCtx][Eq_V] >= nStart[oCtx][Eq_V] )
                        {
                            // copy also for tangential direction
                            cI3D(ctx,m,n,1,cfvrou) = cI3D(oCtx,oM,oN,1,cfvrou);
                            cI3D(ctx,m,n,2,cfvrou) = cI3D(oCtx,oM,oN,2,cfvrou);
                            MAP_LAYERS_LOOP(ctx)
                            {
                                cI3D(ctx,m,n,k,v0) = cI3D(oCtx,oM,oN,oK,v0);
                                cI3D(ctx,m,n,k,v1) = cI3D(oCtx,oM,oN,oK,v1);
                            }
                            //
                            // Exchange gvv's
                            //
                            cI2D(ctx,m,n,gvv) = cI2D(oCtx,oM,oN,gvv);
                            // printf("l2r: %d,%d->%d,%d: %10.4f\n",
                            //            oM,oN,m,n,cI2D(oCtx,oM,oN,gvv) );
                        }
                    }
                }
                break;

            case Eq_V:

                if ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top )
                {
                    MAP_CELLS_LOOP(ctx,eq)
                    {
                        if ( Edge[ctx] == Edge_Top )
                        {
                            if (cI2D(ctx,m,n+1,kcv) == 3)
                            {
                                cI3D(ctx,m,n+1,1,cfvrou) = cI3D(oCtx,oM,oN+1,1,cfvrou);
                                cI3D(ctx,m,n+1,2,cfvrou) = cI3D(oCtx,oM,oN+1,2,cfvrou);
                                cI2D(ctx,m,n+1,z0vrou) = cI2D(oCtx,oM,oN+1,z0vrou);
                                MAP_LAYERS_LOOP(ctx)
                                {
                                    cI3D(ctx,m,n+1,k,v0) = cI3D(oCtx,oM,oN+1,oK,v0);
                                    cI3D(ctx,m,n+1,k,v1) = cI3D(oCtx,oM,oN+1,oK,v1);
                                }
                            }
                        }
                        //
                        if (cI2D(ctx,m,n,kcv) == 3)
                        {
                            cI3D(ctx,m,n,1,cfvrou) = cI3D(oCtx,oM,oN,1,cfvrou);
                            cI3D(ctx,m,n,2,cfvrou) = cI3D(oCtx,oM,oN,2,cfvrou);
                            cI2D(ctx,m,n,z0vrou) = cI2D(oCtx,oM,oN,z0vrou);
                            MAP_LAYERS_LOOP(ctx)
                            {
                                cI3D(ctx,m,n,k,v0) = cI3D(oCtx,oM,oN,oK,v0);
                                cI3D(ctx,m,n,k,v1) = cI3D(oCtx,oM,oN,oK,v1);
                            }
                        }
                    }
                    MAP_CELLS_LOOP(ctx,Eq_U)
                    {
                        if (cI2D(ctx,m,n,kcu) == 3 && mEnd[oCtx][Eq_U] >= mStart[oCtx][Eq_U] )
                        {
                            // copy also for tangential direction
                            cI3D(ctx,m,n,1,cfurou) = cI3D(oCtx,oM,oN,1,cfurou);
                            cI3D(ctx,m,n,2,cfurou) = cI3D(oCtx,oM,oN,2,cfurou);
                            MAP_LAYERS_LOOP(ctx)
                            {
                                cI3D(ctx,m,n,k,u0) = cI3D(oCtx,oM,oN,oK,u0);
                                cI3D(ctx,m,n,k,u1) = cI3D(oCtx,oM,oN,oK,u1);
                            }
                            //
                            // Exchange guu's
                            //
                            cI2D(ctx,m,n,guu) = cI2D(oCtx,oM,oN,guu);
                            // printf("b2t: %d,%d->%d,%d: %10.4f\n",
                            //        oM,oN,m,n,cI2D(oCtx,oM,oN,guu) );
                        }
                    }
                }
                break;

            case Eq_Zeta:

                int k1 = C[ctx]->kMax + 1;
                int k2 = C[oCtx]->kMax + 1;
                int k3 = C[ctx]->kMax + 2;
                int k4 = C[oCtx]->kMax + 2;
                MAP_CELLS_LOOP(ctx,eq)
                {
                    cI2D(ctx,m,n,s0) = cI2D(oCtx,oM,oN,s0);
                    cI2D(ctx,m,n,dps) = cI2D(oCtx,oM,oN,dps);
                    // printf("copy-dps: (%2d,%2d) %10.5f\n", m, n,cI2D(ctx,m,n,dps));
                    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "copy-dps: (%2d,%2d) %10.5f", m, n,cI2D(ctx,m,n,dps));
                    cI3D(ctx,m,n,k1,vicuv) = cI3D(oCtx,oM,oN,k2,vicuv);
                    cI3D(ctx,m,n,k3,vicuv) = cI3D(oCtx,oM,oN,k4,vicuv);
                    if (C[ctx]->lStsci>0)
                    {
                        cI3D(ctx,m,n,k1,dicuv) = cI3D(oCtx,oM,oN,k2,dicuv);
                        cI3D(ctx,m,n,k3,dicuv) = cI3D(oCtx,oM,oN,k4,dicuv);
                        MAP_LAYERS_LOOP(ctx)
                        {
                            cI3D(ctx,m,n,k,vicuv) = cI3D(oCtx,oM,oN,k,vicuv);
                            cI3D(ctx,m,n,k,dicuv) = cI3D(oCtx,oM,oN,k,dicuv);

                            for ( l = 1 ; l <= C[ctx]->lStsci ; l++ )
                            {
                                horAverageR0 = 0.0;
                                horAverageR1 = 0.0;
                                MAP_REFINED_LOOP(oCtx,oM,oN)
                                {
                                    horAverageR0 += cI4D(oCtx,fM,fN,oK,l,r0);
                                    horAverageR1 += cI4D(oCtx,fM,fN,oK,l,r1);
                                }
                                valueR0 = horAverageR0 / nHorRef;
                                valueR1 = horAverageR1 / nHorRef;
                                cI4D(ctx,m,n,k,l,r0) = (float) valueR0;
                                cI4D(ctx,m,n,k,l,r1) = (float) valueR1;
                                // printf("InitFlowVariables:copyflow-Conc: (%2d,%2d,%2d,%2d,%2d) %10.5f %10.5f\n", ctx, m, n,k,l,cI4D(ctx,m,n,k,l,r1),cI4D(ctx,m,n,k,l,r0));
                                // printf("InitFlowVariables:copyflow-Conc: (%2d,%2d,%2d,%2d)\n", oCtx,oM,oN,oK);
                            }
                        }
                    }
#if 1
                    //
                    //  Ad Hoc corrections for "inner corner" problem
                    //
                    if ( Edge[ctx] == Edge_Left )
                    {
                        cI2D(ctx,m-1,n,s0) = cI2D(oCtx,oM-1,oN,s0);
                        cI2D(ctx,m-1,n,dps) = cI2D(oCtx,oM-1,oN,dps);
                        cI3D(ctx,m-1,n,k1,vicuv) = cI3D(oCtx,oM-1,oN,k2,vicuv);
                        cI3D(ctx,m-1,n,k3,vicuv) = cI3D(oCtx,oM-1,oN,k4,vicuv);
                        cI3D(ctx,m-1,n,k1,dicuv) = cI3D(oCtx,oM-1,oN,k2,dicuv);
                        cI3D(ctx,m-1,n,k3,dicuv +2) = cI3D(oCtx,oM-1,oN,k4,dicuv);
                        MAP_LAYERS_LOOP(ctx)
                        {
                            cI3D(ctx,m-1,n,k,vicuv) = cI3D(oCtx,oM-1,oN,k,vicuv);
                            cI3D(ctx,m-1,n,k,dicuv) = cI3D(oCtx,oM-1,oN,k,dicuv);
                            for ( l = 1 ; l <= C[ctx]->lStsci ; l++ )
                            {
                                cI4D(ctx,m-1,n,k,l,r0) = cI4D(oCtx,oM-1,oN,oK,l,r0);
                                cI4D(ctx,m-1,n,k,l,r1) = cI4D(oCtx,oM-1,oN,oK,l,r1);
                            }
                        }
                    }
                    if ( Edge[ctx] == Edge_Bottom )
                    {
                        cI2D(ctx,m,n-1,s0) = cI2D(oCtx,oM,oN-1,s0);
                        cI2D(ctx,m,n-1,dps) = cI2D(oCtx,oM,oN-1,dps);
                        cI3D(ctx,m,n-1,k1,vicuv) = cI3D(oCtx,oM,oN-1,k2,vicuv);
                        cI3D(ctx,m,n-1,k3,vicuv) = cI3D(oCtx,oM,oN-1,k4,vicuv);
                        cI3D(ctx,m,n-1,k1,dicuv) = cI3D(oCtx,oM,oN-1,k2,dicuv);
                        cI3D(ctx,m,n-1,k3,dicuv) = cI3D(oCtx,oM,oN-1,k4,dicuv);
                        MAP_LAYERS_LOOP(ctx)
                        {
                            cI3D(ctx,m,n-1,k,vicuv) = cI3D(oCtx,oM,oN-1,k,vicuv);
                            cI3D(ctx,m,n-1,k,dicuv) = cI3D(oCtx,oM,oN-1,k,dicuv);

                            for ( l = 1 ; l <= C[ctx]->lStsci ; l++ )
                            {
                                cI4D(ctx,m,n-1,k,l,r0) = cI4D(oCtx,oM,oN-1,oK,l,r0);
                                cI4D(ctx,m,n-1,k,l,r1) = cI4D(oCtx,oM,oN-1,oK,l,r1);
                            }
                        }
                    }
#endif
                }
                break;
            } // end switch
        } // end for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    } // end for ( eq = 0 ; eq < NR_EQ ; eq++ )
}


////////////////////////////////////////////////////////////////////////////
//
// Protected Functions for the subsequent Mapper Steps
//


void D3dFlowMapper::CopyFlowVariables(
    Seq         seq                     // Type of variable
    )
{
    Eq          eq = location[seq];// type of equation location
    MapScalar       value;      // value to copy

    MAPDBG_FUN("D3dFlowMapper::CopyFlowVariables");

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
    oCtx = 1 - ctx;

    switch(seq)
    {
        case Seq_U:

          if ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right )
          {
            // copy for U(m,n)
            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m,n,kcu) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tCopy-U (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;

                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        value = this->InterpolateNormalU1(ctx, oCtx, n, oM, oN, oK);
                    }
                    cI3D(ctx,m,n,k,u1) = (float) value;
                    // printf("copy-u: (%2d,%2d,%2d) %10.5f\n", m, n,k,value);
                    cI2D(ctx,m,n,umean) = cI2D(oCtx,oM,oN,umean);

                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax)
                    {
                       cI3D(ctx,m,n,k,kfuz0) = cI3D(oCtx,oM,oN,k,kfuz0);
                       // printf("copyflow kfuz0 for Z-model: (%2d,%2d,%2d) \n",m,n,k);
                    }
                }
            }

            // copy for U(m+1,n)
            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m+1,n,kcu) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM+1,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                                FMapLog((char*)"\t\tCopy-U (%d,%d)<-(%d,%d)\n",
                                            m+1,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;
                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        value = this->InterpolateNormalU1(ctx, oCtx, n, oM+1, oN, oK);
                    }
                    cI3D(ctx,m+1,n,k,u1) = (float) value;
                    // printf("copy-u: (%2d,%2d,%2d) %10.5f\n", m+1, n,k,value);
                    cI2D(ctx,m+1,n,umean) = cI2D(oCtx,oM+1,oN,umean);
                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax )
                    {
                        cI3D(ctx,m+1,n,k,kfuz0) = cI3D(oCtx,oM+1,oN,k,kfuz0);
                        // printf("copyflow kfuz0 voor Z-model: (%2d,%2d) \n",m+1,n);
                    }
                }
            }

            MAP_CELLS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m,n,kcu) == 3)
                {
                    cI2D(ctx,m,n,kfu)= cI2D(oCtx,oM,oN,kfu);
                    // printf("copy-kfu: (%2d,%2d) %2d\n", m, n,cI2D(oCtx,oM,oN,kfu));
                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax)
                    {
                        cI2D(ctx,m,n,kfumx0)= cI2D(oCtx,oM,oN,kfumx0);
                        // cI2D(ctx,m,n,kfumin)= cI2D(oCtx,oM,oN,kfumin);
                        // printf("copyflow kfu voor Z-model: (%2d,%2d) \n",m,n);
                    }
                }
            }

            MAP_CELLS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m+1,n,kcu) == 3)
                {
                    cI2D(ctx,m+1,n,kfu)= cI2D(oCtx,oM+1,oN,kfu);
                    // printf("copy-kfu: (%2d,%2d) (%2d,%2d) %2d\n", m+1, n,oM+1,oN,cI2D(oCtx,oM+1,oN,kfu));
                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax )
                    {
                        cI2D(ctx,m+1,n,kfumx0)= cI2D(oCtx,oM+1,oN,kfumx0);
                        // cI2D(ctx,m+1,n,kfumin)= cI2D(oCtx,oM+1,oN,kfumin);
                        // printf("copyflow kfu voor Z-model: (%2d,%2d) \n",m+1,n);
                    }
                }
            }
            MAP_CELLS_LOOP(ctx,Eq_V)
            {
                if (cI2D(ctx,m,n,kcv) == 3 && nEnd[oCtx][Eq_V] > nStart[oCtx][Eq_V] )
                {
                    cI2D(ctx,m,n,kfv)= cI2D(oCtx,oM,oN,kfv);
                    // printf("copy-kfv: (%2d,%2d) (%2d,%2d) %2d\n", m, n,oM,oN,cI2D(oCtx,oM,oN,kfv));
                }
            }
          }

        break;

        case Seq_V:

          if ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top )
          {
            // copy for V(m,n)
            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m,n,kcv) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                        FMapLog((char*)"\t\tCopy-V (%d,%d)<-(%d,%d)\n",
                                    m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;
                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        value = this->InterpolateNormalV1(ctx, oCtx, m, oM, oN, oK);
                    }
                    cI3D(ctx,m,n,k,v1) = (float) value;
                    cI2D(ctx,m,n,vmean) = cI2D(oCtx,oM,oN,vmean);
                    // printf("copy-v: (%2d,%2d) %10.7f\n", m, n,value);

                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax )
                    {
                        cI3D(ctx,m,n,k,kfvz0) = cI3D(oCtx,oM,oN,k,kfvz0);
                        // printf("copyflow kfvz0 for Z-model: (%2d,%2d,%2d) \n",m,n,k);
                    }
                }
            }

            // copy for V(m,n+1)
            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m,n+1,kcv) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN+1)
                    {
                        ON_DEBUG ( DBLEV3,
                        FMapLog((char*)"\t\tCopy-V (%d,%d)<-(%d,%d)\n",
                                    m,n+1,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;

                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        value = this->InterpolateNormalV1(ctx, oCtx, m, oM, oN+1, oK);
                    }
                    cI3D(ctx,m,n+1,k,v1) = (float) value;
                    cI2D(ctx,m,n+1,vmean) = cI2D(oCtx,oM,oN+1,vmean);
                    // printf("copy-v: (%2d,%2d) %10.7f\n", m, n+1,value);
                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax )
                    {
                        cI3D(ctx,m,n+1,k,kfvz0) = cI3D(oCtx,oM,oN+1,k,kfvz0);
                        // printf("copyflow kfvz0 voor Z-model: (%2d,%2d,%2d) \n",m,n+1,k);
                    }
                }
            }

            MAP_CELLS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m,n,kcv) == 3)
                {
                    cI2D(ctx,m,n,kfv)= cI2D(oCtx,oM,oN,kfv);

                    // printf("copy-kfv: (%2d,%2d)  %2d (%2d,%2d) %2d\n", m, n,cI2D(ctx,m,n,kfv),oM,oN,cI2D(oCtx,oM,oN,kfv));
                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax)
                    {
                        cI2D(ctx,m,n,kfvmx0)= cI2D(oCtx,oM,oN,kfvmx0);
                        // cI2D(ctx,m,n,kfvmin)= cI2D(oCtx,oM,oN,kfvmin);
                        // printf("copyflow kfv voor Z-model: (%2d,%2d) \n",m,n);
                    }
                }
            }

            MAP_CELLS_LOOP(ctx,eq)
            {
                if (cI2D(ctx,m,n+1,kcv) == 3)
                {
                    cI2D(ctx,m,n+1,kfv)= cI2D(oCtx,oM,oN+1,kfv);
                    // printf("copy-kfv: (%2d,%2d) %2d\n", m, n+1,cI2D(oCtx,oM,oN+1,kfv));
                    if ( C[ctx]->Zmodel && C[ctx]->kMax == C[oCtx]->kMax )
                    {
                        cI2D(ctx,m,n+1,kfvmx0)= cI2D(oCtx,oM,oN+1,kfvmx0);
                        // cI2D(ctx,m,n+1,kfvmin)= cI2D(oCtx,oM,oN+1,kfvmin);
                        // printf("copyflow kfv voor Z-model: (%2d,%2d) \n",m,n+1);
                    }
                }
            }
            MAP_CELLS_LOOP(ctx,Eq_U)
            {
                if (cI2D(ctx,m,n,kcu) == 3 && mEnd[oCtx][Eq_U] > mStart[oCtx][Eq_U] )
                {
                    cI2D(ctx,m,n,kfu)= cI2D(oCtx,oM,oN,kfu);
                    // printf("copy-kfu: (%2d,%2d) (%2d,%2d) %2d\n", m, n,oM,oN,cI2D(oCtx,oM,oN,kfu));
                }
            }
          }

        break;

        case Seq_Zeta:

        MAP_CELLS_LOOP(ctx,eq)
        {
            //
            // copy water levels
            //
            horAverage = 0.0;
            MAP_REFINED_LOOP(oCtx,oM,oN)
            {
                ON_DEBUG ( DBLEV3,
                FMapLog((char*)"\t\tCopy-V (%d,%d)<-(%d,%d)\n",
                            m,n,fM,fN);
                )
                horAverage += cI2D(oCtx,fM,fN,s1);
            }
            value = horAverage / nHorRef;
            //
            // Interpolation for S1 not aplied yet
            //
            // if ( Ref[ctx] > Ref[oCtx] )
            // {
            //     value = this->InterpolateS1(ctx, oCtx, m, n, oM, oN, oK);
            // }
            cI2D(ctx,m,n,s1) = (float) value;
            // printf("copyflow-s1: (%2d,%2d) %12.8f\n", m, n,cI2D(ctx,m,n,s1));
        }

        break;

        case Seq_Conc:

        MAP_CELLS_LOOP(ctx,eq)
        {
            //
            // copy 2D_viscosity/diffusivity for HLES
            //
            int k1 = C[ctx]->kMax + 1;
            int k2 = C[oCtx]->kMax + 1;
            int k3 = C[ctx]->kMax + 2;
            int k4 = C[oCtx]->kMax + 2;
            cI3D(ctx,m,n,k1,vicuv) = cI3D(oCtx,oM,oN,k2,vicuv);
            cI3D(ctx,m,n,k3,vicuv) = cI3D(oCtx,oM,oN,k4,vicuv);
            cI3D(ctx,m,n,k1,dicuv) = cI3D(oCtx,oM,oN,k2,dicuv);
            cI3D(ctx,m,n,k3,dicuv) = cI3D(oCtx,oM,oN,k4,dicuv);
            MAP_LAYERS_LOOP(ctx)
            {
                cI3D(ctx,m,n,k,vicuv) = cI3D(oCtx,oM,oN,k,vicuv);
                cI3D(ctx,m,n,k,dicuv) = cI3D(oCtx,oM,oN,k,dicuv);

                for ( l = 1 ; l <= C[ctx]->lStsci ; l++ )
                {
                    cI4D(ctx,m,n,k,l,r1) = cI4D(oCtx,oM,oN,oK,l,r1);
                    // printf("copyflow-Conc: (%2d,%2d,%2d,%2d) %10.5f\n", m, n,k,l,cI4D(ctx,m,n,k,l,r1));
                }
#if 1
                //
                //  Ad Hoc corrections for "inner corner" problem
                //
                if ( Edge[ctx] == Edge_Left )
                {
                    cI2D(ctx,m-1,n,s1) = cI2D(oCtx,oM-1,oN,s1);
                    for ( l = 1 ; l <= C[ctx]->lStsci ; l++ )
                    {
                        cI4D(ctx,m-1,n,k,l,r1) = cI4D(oCtx,oM-1,oN,oK,l,r1);
                    }
                    cI3D(ctx,m-1,n,k,dicuv) = cI3D(oCtx,oM-1,oN,k,dicuv);
                }
                if ( Edge[ctx] == Edge_Bottom )
                {
                   cI2D(ctx,m,n-1,s1) = cI2D(oCtx,oM,oN-1,s1);
                    for ( l = 1 ; l <= C[ctx]->lStsci ; l++ )
                    {
                        cI4D(ctx,m,n-1,k,l,r1) = cI4D(oCtx,oM,oN-1,oK,l,r1);
                    }
                    cI3D(ctx,m,n-1,k,dicuv) = cI3D(oCtx,oM,oN-1,k,dicuv);

                }
#endif
            }
        }

        break;

        default:

        FLOW2D3D->dd->log->Write (Log::WARN, "unexpected case CopyFlowVariables");
        break;

        }
    }
}


void D3dFlowMapper::CopyOuterNormalVelocities(
    Seq         seq                     // Type of variable
    )

//
//   Copy velocity values to the coupling points that are located
//   outside the model domain (thus not for the velocity points at the
//   interface. This is done at the end of subroutine SUD.
//

{
    Eq          eq = location[seq];// type of equation location
    MapScalar       value;      // value to copy

    MAPDBG_FUN("D3dFlowMapper::CopyOuterNormalVelocities");

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        oCtx = 1 - ctx;

        switch(seq)
        {
        case Seq_U:

            if ( Edge[ctx] == Edge_Left )
            {
                // copy for U(m,n)
                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                if (cI2D(ctx,m,n,kcu) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tCopy-U (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;

                    cI3D(ctx,m,n,k,u1) = (float) value;
                    // printf("copy-outerN-u-l: (%2d,%2d,%2d) %10.5f\n", m, n,k,value);
                    cI2D(ctx,m,n,umean) = cI2D(oCtx,oM,oN,umean);
                }
                }
            }

            if ( Edge[ctx] == Edge_Right )
            {
                // copy for U(m+1,n)
                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                if (cI2D(ctx,m+1,n,kcu) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM+1,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tCopy-U (%d,%d)<-(%d,%d)\n",
                                        m+1,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;

                    cI3D(ctx,m+1,n,k,u1) = (float) value;
                    // printf("copy-outerN-u-r: (%2d,%2d,%2d) %10.5f\n", m+1, n,k,value);
                    cI2D(ctx,m+1,n,umean) = cI2D(oCtx,oM+1,oN,umean);
                }
                }
            }

            break;

        case Seq_V:

            if ( Edge[ctx] == Edge_Bottom )
            {
                // copy for V(m,n)
                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                if (cI2D(ctx,m,n,kcv) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tCopy-V (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;

                    cI3D(ctx,m,n,k,v1) = (float) value;
                    // printf("copy-outerN-v-b: (%2d,%2d,%2d) %10.5f\n", m, n,k,value);
                    cI2D(ctx,m,n,vmean) = cI2D(oCtx,oM,oN,vmean);
                }
                }
            }

            if ( Edge[ctx] == Edge_Top )
            {
                // copy for V(m,n+1)
                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                if (cI2D(ctx,m,n+1,kcv) == 3)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN+1)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tCopy-V (%d,%d)<-(%d,%d)\n",
                                        m,n+1,fM,fN+1);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    value = horAverage / nHorRef;

                    cI3D(ctx,m,n+1,k,v1) = (float) value;
                    // printf("copy-outerN-v-t: (%2d,%2d,%2d) %10.5f\n", m, n+1, k,value);
                    cI2D(ctx,m,n+1,vmean) = cI2D(oCtx,oM,oN+1,vmean);
                }
                }
            }
            break;

        default:
            FLOW2D3D->dd->log->Write (Log::WARN, "unexpected case CopyOuterNormalVelocities");
            break;

        }
    }
}




void D3dFlowMapper::CopyOuterTangentVelocities(
    Seq         seq                     // Type of variable
    )

//
//   Copy velocity values to the coupling points that are located
//   outside the model domain (thus not for the velocity points at the
//   interface. This is done at the end of subroutine SUD.
//

{
    Eq          eq = location[seq];// type of equation location
    MapScalar       value;      // value to copy

    MAPDBG_FUN("D3dFlowMapper::CopyOuterTangentVelocities");

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        oCtx = 1 - ctx;

        switch(seq)
        {
        case Seq_U:

          if ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right )
          {
            // copy for V(m,n)
            MAP_CELLS_AND_LAYERS_LOOP(ctx,Eq_V)
            {
                horAverage = 0.0;
                MAP_REFINED_LOOP(oCtx,oM,oN)
                {
                    ON_DEBUG ( DBLEV3,
                        FMapLog((char*)"\t\tCopy-V (%d,%d)<-(%d,%d)\n",
                                    m,n,fM,fN);
                    )
                    V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                    horAverage += vertAverage;
                }
                value = horAverage / nHorRef;

                if (cI2D(ctx,m,n,kcv) == 3 && nEnd[oCtx][Eq_V] > nStart[oCtx][Eq_V] )
                {
                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        value = this->InterpolateTangentialV1(ctx, oCtx, n, oM, oN, oK);
                    }
                    // printf("copy-outerT-v: (%2d,%2d,%2d) (%2d %2d) %10.5f\n", m, n,k,oM,oN,value);
                    cI3D(ctx,m,n,k,v1) = (float) value;
                    cI2D(ctx,m,n,vmean) = cI2D(oCtx,oM,oN,vmean);
                }
            }
          }
            break;

        case Seq_V:

          if ( Edge[ctx] == Edge_Top || Edge[ctx] == Edge_Bottom )
          {
            // copy for U(m,n)
            MAP_CELLS_AND_LAYERS_LOOP(ctx,Eq_U)
            {
                horAverage = 0.0;
                MAP_REFINED_LOOP(oCtx,oM,oN)
                {
                    ON_DEBUG ( DBLEV3,
                        FMapLog((char*)"\t\tCopy-U (%d,%d)<-(%d,%d)\n",
                                    m,n,fM,fN);
                    )
                    V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                    horAverage += vertAverage;
                }
                value = horAverage / nHorRef;

                if (cI2D(ctx,m,n,kcu) == 3 && mEnd[oCtx][Eq_U] > mStart[oCtx][Eq_U] )
                {
                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        value = this->InterpolateTangentialU1(ctx, oCtx, m, oM, oN, oK);
                    }
                    // printf("copy-outerT-u: (%2d,%2d,%2d) (%2d %2d) %10.5f\n", m, n,k,oM,oN,value);
                    cI3D(ctx,m,n,k,u1) = (float) value;
                    cI2D(ctx,m,n,umean) = cI2D(oCtx,oM,oN,umean);
                }
            }
          }
            break;

        default:
            FLOW2D3D->dd->log->Write (Log::WARN, "unexpected case CopyOuterTangentVelocities");
            break;

        }
    }
}




void D3dFlowMapper::Build(
    Seq     seq,            // Variable
    int     anL         // concentration loop counter
    )
{
    Eq          eq;     // Location of variable

    MAPDBG_FUN("D3dFlowMapper::Build");

    eq = location[seq];

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        SET_OTHER_CTX;

        ON_DEBUG ( DBLEV1,
            FMapLog((char*)"\nBUILD DD-equations for %s (ctx %d, cntxt-Id %d)\n",
                            seqTxt[seq], ctx, C[ctx]->GetContextId());
        )

        switch (seq)
        {
        case Seq_U:

          if ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right )
          {
            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                //
                // Build equation for U(m,n)
                //
                if (cI2D(ctx,m,n,kcu) == 3)
                {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Building U for %d,%d,%d (ctx %d)\n",
                                    m,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }

                    cI3D(ctx,m,n,k,wrkb4) = (float) horAverage / (float) nHorRef ;
                    // printf("Build eq. for normal U-coup-point (%2d %2d) %2d\n",m,n,cI2D(ctx,m,n,kcu));
                }

                //
                // Also build equation for U(m+1,n)
                //
                if (cI2D(ctx,m+1,n,kcu) == 3)
                {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Building U for %d,%d,%d (ctx %d)\n",
                                    m+1,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM+1,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        m+1,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    cI3D(ctx,m+1,n,k,wrkb4) = (float) horAverage / (float) nHorRef ;
                    // printf("Build eq. for outer U-point (%2d %2d)\n",m+1,n);
                }
            }
          }
            break;

        case Seq_V:

          if ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top )
          {
            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                //
                // Build equation for V(m,n)
                //
                if ( cI2D(ctx,m,n,kcv) == 3 )
                {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Building V for %d,%d,%d (ctx %d)\n",
                                    m,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    cI3D(ctx,m,n,k,wrkb4) = (float) horAverage / (float) nHorRef ;
                    // printf("Build eq. for normal V-coup-point (%2d %2d) %2d\n",m,n,cI2D(ctx,m,n,kcv));
                }

                //
                // Also Build equation for V(m,n+1)
                //
                if ( cI2D(ctx,m,n+1,kcv) == 3 )
                {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Building V for %d,%d,%d (ctx %d)\n",
                                    m,n+1,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN+1)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        m,n+1,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    cI3D(ctx,m,n+1,k,wrkb4) = (float) horAverage / (float) nHorRef ;
                    // printf("Build eq. for outer V-point (%2d %2d)\n",m,n+1);
                }
            }
          }
            break;

        case Seq_Zeta: case Seq_Zeta_ADI_St1: case Seq_Zeta_ADI_St2:
            printf("THIS CODE LOCATION SHOULD NOT BE REACHED\n");
            break;

        case Seq_Conc: case Seq_Conc_ADI_St1: case Seq_Conc_ADI_St2:

            MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
            {
                ON_DEBUG ( DBLEV2,
                    FMapLog((char*)"Building C for %d,%d,%d, c-%d (ctx %d)\n",
                                m,n,k,anL, ctx);
                )

                horAverage = 0.0;
                MAP_REFINED_LOOP(oCtx,oM,oN)
                {
                    ON_DEBUG ( DBLEV3,
                        FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                    m,n,fM,fN);
                    )
                    V_AVERAGE4D(oCtx,fM,fN,oK,anL, r1,vertAverage);
                    horAverage += vertAverage;
                }
                cI4D(ctx,m,n,k,anL,wrkc4) = (float) horAverage / (float) nHorRef;
                // printf("stoftransp-normaal %4d %4d %4d %4d %10.7f \n",m,n,oM,oN,cI4D(ctx,m,n,k,anL,wrkc4));

#if 1
                //
                //  Ad Hoc corrections for "inner corner" problem
                //
                if ( Edge[ctx] == Edge_Left )
                {
                   horAverage = 0.0;
                   MAP_REFINED_LOOP(oCtx,oM-1,oN)
                   {
                       ON_DEBUG ( DBLEV3,
                           FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                       m,n,fM,fN);
                       )
                       V_AVERAGE4D(oCtx,fM,fN,oK,anL, r1,vertAverage);
                       horAverage += vertAverage;
                   }
                   cI4D(ctx,m-1,n,k,anL,wrkc4) = (float) horAverage / (float) nHorRef;
                   // printf("stoftransp-links %4d %4d %10.6f \n",m-1,n,cI4D(ctx,m-1,n,k,anL,wrkc4));
                }

                if ( Edge[ctx] == Edge_Bottom )
                {
                   horAverage = 0.0;
                   MAP_REFINED_LOOP(oCtx,oM,oN-1)
                   {
                       ON_DEBUG ( DBLEV3,
                           FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                       m,n,fM,fN);
                       )
                       V_AVERAGE4D(oCtx,fM,fN,oK,anL, r1,vertAverage);
                       horAverage += vertAverage;
                   }
                   cI4D(ctx,m,n-1,k,anL,wrkc4) = (float) horAverage / (float) nHorRef;
                   // printf("stoftransp-onder %4d %4d %10.6f \n",m,n-1,cI4D(ctx,m,n-1,k,anL,wrkc4));
                }
#endif
            }

            break;

        case Seq_2DAD:

            MAP_CELLS_LOOP(ctx,Eq_Zeta)
            {
                //
                // copy quantity to be transported
                //
                horAverage = 0.0;
                MAP_REFINED_LOOP(oCtx,oM,oN)
                    horAverage += cI3D(oCtx,fM,fN,1,wrkb17);
                cI3D(ctx,m,n,1,wrkb17) = (float) (horAverage / nHorRef);
            }
            break;

        case Seq_RolUV:

            if ( C[ctx]->Roller )
            {
                MAP_CELLS_LOOP(ctx,Eq_Zeta)
                {
                    //
                    // compute spatial average of quantities
                    //
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                        horAverage += cI2D(oCtx,fM,fN,qxkr);
                    cI2D(ctx,m,n,qxkr) = (float) (horAverage / nHorRef);
                    //
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                        horAverage += cI2D(oCtx,fM,fN,qykr);
                    cI2D(ctx,m,n,qykr) = (float) (horAverage / nHorRef);
                    //
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                        horAverage += cI2D(oCtx,fM,fN,qxkw);
                    cI2D(ctx,m,n,qxkw) = (float) (horAverage / nHorRef);
                    //
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                        horAverage += cI2D(oCtx,fM,fN,qykw);
                    cI2D(ctx,m,n,qykw) = (float) (horAverage / nHorRef);
                }
            }
            break;

        default:

            throw new Exception (true, "unexpected case BuildDdEquations");
            break;
        }
    }
}


int D3dFlowMapper::Proces(
    Seq     seq,        // Variable
    int     anL         // concentration loop counter
    )
{
    int     converged=0;        // boolean: convergence reached?
    int     allConverged=0;     // boolean: allover converg. reached?
    int     toBeSolved[NR_CNTXTS];  // which context must be resolved?

    MAPDBG_FUN("D3dFlowMapper::Proces");

    //
    // Overal Convergence is reached (so nor 'resolve' is needed) when:
    // -    this mapper concludes convergence between the domains
    //      (if no convergence yet: Adjust equations).
    // -    The barrier-minimum function returns 1 (all mappers
    //      conclude convergence)
    //

    converged = CheckConvergence(seq, anL, toBeSolved);

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER \"%s\" calls minimumbarrier (converged %d)", IteratorSelf()->name, converged);
    allConverged = MinimumBarrier(converged);
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER \"%s\" minimumbarrier returned, conv: %d", IteratorSelf()->name, allConverged);

    if ( ! allConverged )
    {
        Adjust(seq, anL, toBeSolved);
    }

    return (! allConverged);

}


int D3dFlowMapper::CheckConvergence(
    Seq     seq,            // Variable type
    int     anL,            // concentration loop counter
    int     toBeSolved[NR_CNTXTS]   // which context must be resolved?
    )
{
    Eq          eq;     // location of variable
    MapScalar   delta,deltaMax[NR_CNTXTS];
    MapScalar   currentRHS, otherRHS;

    MAPDBG_FUN("D3dFlowMapper::CheckConvergence");

    eq = location[seq];

    //
    // Evaluate Right Hand Side from source, compare with Right Hand
    //   Side in Target
    //

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        SET_OTHER_CTX;

        deltaMax[ctx] = 0.0L;

        ON_DEBUG ( DBLEV1,
            FMapLog((char*)"\nEVALUATE RHS for %s (ctx %d, cntxt-Id %d)\n\n",
                        seqTxt[seq], ctx, C[ctx]->GetContextId());
            )

        switch (seq)
        {
        case Seq_U:
            MAP_COARSE_CELLS_AND_COARSE_LAYERS_LOOP(ctx,eq)
            {
                if ( (cI2D(ctx,m,n,kcu) == 3 && cI2D(ctx,m,n,kfu) == 1 ) &&
                     (cI2D(ctx,m,n,kcs) * cI2D(ctx,m+1,n,kcs) < 9 ) &&
                     ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right ) )

                {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"ChkConv U for %d,%d,%d (ctx %d)\n",
                                    m,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    otherRHS = horAverage / nHorRef;

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(ctx,m,n)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        oM,oN,fM,fN);
                        )
                        V_AVERAGE3D( ctx,fM,fN, k, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    currentRHS = horAverage / nHorRef;

                    delta = fabs(  otherRHS - currentRHS );
                    ON_DEBUG ( DBLEV6,
                        FMapLog((char*)"2 sides %10.8f,%10.8f\n",
                               otherRHS, currentRHS );
                        FMapLog((char*)"Delta %3d,%3d,%3d  %10.8f\n",
                                m,n,k,delta);
                    )
                    deltaMax[ctx]  = B_MAX( deltaMax[ctx], delta );
                }
            }
            break;

        case Seq_V:
            MAP_COARSE_CELLS_AND_COARSE_LAYERS_LOOP(ctx,eq)
            {
                if ( (cI2D(ctx,m,n,kcv) == 3 && cI2D(ctx,m,n,kfv) == 1 ) &&
                     (cI2D(ctx,m,n,kcs) * cI2D(ctx,m,n+1,kcs) < 9 ) &&
                     ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top ) )
                {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"ChkConv V for %d,%d,%d (ctx %d)\n",
                                    m,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        m,n,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    otherRHS = horAverage / nHorRef;

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(ctx,m,n)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        oM,oN,fM,fN);
                        )
                        V_AVERAGE3D( ctx,fM,fN, k, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    currentRHS = horAverage / nHorRef;

                    delta = fabs(  otherRHS - currentRHS );
                    ON_DEBUG ( DBLEV6,
                        FMapLog((char*)"2 sides %10.8f,%10.8f\n",
                               otherRHS, currentRHS );
                        FMapLog((char*)"Delta %3d,%3d,%3d  %10.8f\n",
                                m,n,k,delta);
                    )
                    deltaMax[ctx]  = B_MAX( deltaMax[ctx], delta );
                }
            }
            break;

        case Seq_Zeta: case Seq_Zeta_ADI_St1: case Seq_Zeta_ADI_St2:
            printf("This option for Seq_Zeta is not available\n");
            break;

        case Seq_Conc: case Seq_Conc_ADI_St1: case Seq_Conc_ADI_St2:

            MAP_COARSE_CELLS_AND_COARSE_LAYERS_LOOP(ctx,eq)
            {
                horAverage = 0.0;
                MAP_REFINED_LOOP(oCtx,oM,oN)
                {
                    V_AVERAGE4D(oCtx,fM,fN,oK, anL, r1, vertAverage );
                    horAverage += vertAverage;
                }
                otherRHS = horAverage / nHorRef;

                horAverage = 0.0;
                MAP_REFINED_LOOP(ctx,m,n)
                {
                    V_AVERAGE4D( ctx,fM,fN, k, anL, r1, vertAverage );
                    horAverage += vertAverage;
                }
                currentRHS = horAverage / nHorRef;

                delta = fabs(  otherRHS - currentRHS );
                ON_DEBUG ( DBLEV6,
                    FMapLog((char*)"2 sides %10.8f,%10.8f\n",
                           otherRHS, currentRHS );
                    FMapLog((char*)"Delta %3d,%3d,%3d,%3d  %10.8f\n",
                        m,n,k,anL, delta);
                )
                deltaMax[ctx]  = B_MAX( deltaMax[ctx], delta );
            }
            break;

        case Seq_2DAD:
            MAP_COARSE_CELLS_LOOP(ctx,Eq_Zeta)
            {
                horAverage = 0.0;
                MAP_REFINED_LOOP(oCtx,oM,oN)
                {
                    horAverage += cI3D(oCtx,fM,fN,1,wrkb17);
                }
                otherRHS = horAverage / nHorRef;

                horAverage = 0.0;
                MAP_REFINED_LOOP(ctx,m,n)
                {
                    horAverage += cI3D(ctx,fM,fN,1,wrkb17);
                }
                currentRHS = horAverage / nHorRef;

                delta = fabs(  otherRHS - currentRHS );
                ON_DEBUG ( DBLEV6,
                    FMapLog((char*)"2 sides %10.8f,%10.8f\n",
                           otherRHS, currentRHS );
                    FMapLog((char*)"Delta %3d,%3d  %10.8f\n",
                        m,n, delta);
                )
                deltaMax[ctx]  = B_MAX( deltaMax[ctx], delta );
            }
            break;
        }

        ON_DEBUG ( DBLEV2+DBLEV6,
            FMapLog((char*)"deltaMax-%s Cntxt-%d: %10.8f\n", seqTxt[seq],
                                 ctx, deltaMax[ctx]);
        )

        toBeSolved[ctx] = ( deltaMax[ctx] > EpsMap[eq] ) ? 1 : 0;
    }

    return ( ( ! toBeSolved[C_0] ) && ( ! toBeSolved[C_1] ) );

}


void D3dFlowMapper::Adjust(
    Seq     seq,            // Equation type
    int     anL,            // concentration loop counter
    int     toBeSolved[NR_CNTXTS]   // which context must be resolved?
    )
{
    Eq          eq;     // Location of unknown

    MAPDBG_FUN("D3dFlowMapper::Adjust");

    eq = location[seq];

    // TODO: Currently both contexts are being adjusted (by resetting
    // toBeSolved). Check if this can be reduced to: 'only adjust a
    // context when it has to be resolved'.
    toBeSolved[C_0] = 1;
    toBeSolved[C_1] = 1;

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        SET_OTHER_CTX;

        ON_DEBUG( DBLEV1,
            FMapLog((char*)"\nADJUST equations for %s (ctx %d, cntxt-Id %d)\n\n",
                            seqTxt[seq], ctx, C[ctx]->GetContextId());
        )

        if ( toBeSolved[ctx] )
        {
            switch (seq)
            {
            case Seq_U :

              if ( Edge[ctx] == Edge_Left || Edge[ctx] == Edge_Right )
              {
                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                  //
                  // Adjust equation for U(m,n)
                  //
                  if (cI2D(ctx,m,n,kcu) == 3)
                  {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Adjusting U for %d,%d,%d (ctx %d)\n",
                                    m,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        oM,oN,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    double targetValue = (double) ( horAverage / (MapScalar) nHorRef );
                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        targetValue = this->InterpolateNormalU1(ctx, oCtx, n, oM, oN, oK);
                    }
                    cI3D(ctx,m,n,k,wrkb4) = targetValue;
                  }
                  //
                  // Also adjust equation for U(m+1,n)
                  //
                  if (cI2D(ctx,m+1,n,kcu) == 3)
                  {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Adjusting U for %d,%d,%d (ctx %d)\n",
                                    m+1,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM+1,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        oM+1,oN,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, u1, vertAverage );
                        horAverage += vertAverage;
                    }
                    double targetValue = (double) ( horAverage / (MapScalar) nHorRef );

                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        targetValue = this->InterpolateNormalU1(ctx, oCtx, n, oM+1, oN, oK);
                    }

                    cI3D(ctx,m+1,n,k,wrkb4) = targetValue;
                  }
                }
              }
                break;

            case Seq_V :

              if ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top )
              {
                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                  //
                  // Adjust equation for V(m,n)
                  //
                  if (cI2D(ctx,m,n,kcv) == 3)
                  {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Adjusting V for %d,%d,%d (ctx %d)\n",
                                    m,n,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        oM,oN,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    double targetValue = (double) ( horAverage / (MapScalar) nHorRef );

                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        targetValue = this->InterpolateNormalV1(ctx, oCtx, m, oM, oN, oK);
                    }

                    cI3D(ctx,m,n,k,wrkb4) = targetValue;
                  }
                  //
                  // Also adjust equation for V(m,n+1)
                  //
                  if (cI2D(ctx,m,n+1,kcv) == 3)
                  {
                    ON_DEBUG ( DBLEV2,
                        FMapLog((char*)"Adjusting V for %d,%d,%d (ctx %d)\n",
                                    m,n+1,k, ctx);
                    )

                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN+1)
                    {
                        ON_DEBUG ( DBLEV3,
                            FMapLog((char*)"\t\tRefine (%d,%d)<-(%d,%d)\n",
                                        oM,oN+1,fM,fN);
                        )
                        V_AVERAGE3D(oCtx,fM,fN,oK, v1, vertAverage );
                        horAverage += vertAverage;
                    }
                    double targetValue = (double) ( horAverage / (MapScalar) nHorRef );

                    if ( Ref[ctx] > Ref[oCtx] )
                    {
                        targetValue = this->InterpolateNormalV1(ctx, oCtx, m, oM, oN+1, oK);
                    }

                    cI3D(ctx,m,n+1,k,wrkb4) = targetValue;
                  }
                }
              }
                break;

            case Seq_Zeta: case Seq_Zeta_ADI_St1: case Seq_Zeta_ADI_St2:
                printf("This option for Seq_Zeta is not available\n");
                break;

            case Seq_Conc: case Seq_Conc_ADI_St1: case Seq_Conc_ADI_St2:

                MAP_CELLS_AND_LAYERS_LOOP(ctx,eq)
                {
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                    {
                        V_AVERAGE4D(oCtx,fM,fN,oK,anL, r1, vertAverage);
                        horAverage += vertAverage;
                    }
                    cI4D(ctx,m,n,k,anL,wrkc4) = (float) horAverage / (float) nHorRef ;
                }
                break;

            case Seq_2DAD:

                MAP_CELLS_LOOP(ctx,Eq_Zeta)
                {
                    //
                    // copy quantity to be transported
                    //
                    horAverage = 0.0;
                    MAP_REFINED_LOOP(oCtx,oM,oN)
                        horAverage += cI3D(oCtx,fM,fN,1,wrkb17);
                    cI3D(ctx,m,n,1,wrkb17) = (float) (horAverage / nHorRef);
                }
                break;
            }
        }
    }
}


double D3dFlowMapper::InterpolateNormalU1(
    int fineCtx,
    int coarseCtx,
    int fineN,
    int coarseM,
    int coarseN,
    int coarseK
    )
{
    // From coarse to fine, interpolate: (1-alpha) * A + alpha * B
    //                         (A: value on n-down, B: value on n-up)

    // Determine alpha
    int nFromStart = fineN - nStart[fineCtx][Eq_U];
    double alpha = (double) (( (nFromStart+1) + Ref[fineCtx]/2 ) % Ref[fineCtx]) / (double) Ref[fineCtx];

    // Determine indices for values at A and B
    int coarseN_A, coarseN_B;
    if ( fineN <= nStart[fineCtx][Eq_U] + Ref[fineCtx]/2 || fineN >= nEnd[fineCtx][Eq_U] - Ref[fineCtx]/2 )
    {
        // First cells, A not available, take value at B
        // or last cells, B not available, take value at A
        coarseN_A = coarseN_B = coarseN;
    }
    else
    {
        // not at begin or end, inbetween
        if ( alpha > 0.5L )
        {
            // Nearer to B (B is the value of the related coarse point)
            coarseN_A = coarseN - 1;
            coarseN_B = coarseN;
        }
        else
        {
            // Nearer to A (A is the value of the related coarse point)
            coarseN_A = coarseN;
            coarseN_B = coarseN + 1;
        }

        // avoid adding the term "zero times an undefined value" to interpolatedValue
        if ( alpha == 0.0L )
                {
                        coarseN_B = coarseN_A;
                }
                else if ( alpha == 1.0L )
                {
                        coarseN_A = coarseN_B;
                }
    }

    double A, B;
    V_AVERAGE3D(coarseCtx,coarseM,coarseN_A,coarseK, u1, A);
    V_AVERAGE3D(coarseCtx,coarseM,coarseN_B,coarseK, u1, B);

    double interpolatedValue = ( 1.0L - alpha ) * A + alpha * B;

    // It seems that undefined values in the mapper very often have the value +- x.xxxxxe-26 in debug mode in VS2008.
    // The following check may give useful information.
    //
        // if ( (fabs(A) > 9.0e-27L && fabs(A) < 1.0e-25L) || (fabs(B) > 9.9e-27L && fabs(B) < 1.0e-25L) )
        // {
        //      printf("\nERROR: Undefined A/B used:\n");
        // }
    // printf("norm-u1(n=%d,N=%d): %8.6lf*%10.7lf A(%d,%d,%d) + %8.6lf*%10.7lf B(%d,%d,%d) = %10.7lf\n",
    //     fineN, coarseN,
    //     1.0-alpha, A, coarseM, coarseN_A, coarseK,
    //     alpha    , B, coarseM, coarseN_B, coarseK,
    //     interpolatedValue);

    return interpolatedValue;
}


double D3dFlowMapper::InterpolateTangentialU1(
    int fineCtx,
    int coarseCtx,
    int fineM,
    int coarseM,
    int coarseN,
    int coarseK
    )
{
    // From coarse to fine, interpolate: (1-alpha) * A + alpha * B
    //                         (A: value on n-down, B: value on n-up)

    // Determine alpha
    int mFromStart = fineM - mStart[fineCtx][Eq_V];
    double alpha = (double) ( ( mFromStart + 1) % Ref[fineCtx] ) / (double) Ref[fineCtx];

    // Determine indices for values at A and B
    int coarseM_A, coarseM_B;
    if ( fineM < mStart[fineCtx][Eq_U] + Ref[fineCtx] || fineM > mEnd[fineCtx][Eq_U] - Ref[fineCtx] )
    {
        // First cells, A not available, take value at B
        // or last cells, B not available, take value at A
        coarseM_A = coarseM_B = coarseM;
    }
    else
    {
        // not at begin or end, inbetween
        if ( alpha >= 0.5L )
        {
            // Nearer to B (B is the value of the related coarse point)
            coarseM_A = coarseM - 1;
            coarseM_B = coarseM;
        }
        else
        {
            // Nearer to A (A is the value of the related coarse point)
            coarseM_A = coarseM;
            coarseM_B = coarseM + 1;
        }

        // avoid adding the term "zero times an undefined value" to interpolatedValue
        if ( alpha == 0.0L )
                {
                        coarseM_B = coarseM_A;
                }
                else if ( alpha == 1.0L )
                {
                        coarseM_A = coarseM_B;
                }
    }

    double A, B;
    V_AVERAGE3D(coarseCtx,coarseM_A,coarseN,coarseK, u1, A);
    V_AVERAGE3D(coarseCtx,coarseM_B,coarseN,coarseK, u1, B);

    double interpolatedValue = ( 1.0L - alpha ) * A + alpha * B;

    // It seems that undefined values in the mapper very often have the value +- x.xxxxxe-26 in debug mode in VS2008.
    // The following check may give useful information.
    //
        // if ( (fabs(A) > 9.0e-27L && fabs(A) < 1.0e-25L) || (fabs(B) > 9.9e-27L && fabs(B) < 1.0e-25L) )
        // {
        //      printf("\nERROR: Undefined A/B used:\n");
        // }
    // printf("tang-u1(m=%d,M=%d): %8.6lf*%10.7lf A(%d,%d,%d) + %8.6lf*%10.7lf B(%d,%d,%d) = %10.7lf\n",
    //     fineM, coarseM,
    //     1.0-alpha, A, coarseM_A, coarseN, coarseK,
    //     alpha    , B, coarseM_B, coarseN, coarseK,
    //     interpolatedValue);

    return interpolatedValue;
}


double D3dFlowMapper::InterpolateNormalV1(
    int fineCtx,
    int coarseCtx,
    int fineM,
    int coarseM,
    int coarseN,
    int coarseK
    )
{
    // From coarse to fine, interpolate: (1-alpha) * A + alpha * B
    //                         (A: value on n-down, B: value on n-up)

    // Determine alpha
    int mFromStart = fineM - mStart[fineCtx][Eq_V];
    double alpha = (double) (( (mFromStart+1) + Ref[fineCtx]/2 ) % Ref[fineCtx]) / (double) Ref[fineCtx];

    // Determine indices for values at A and B
    int coarseM_A, coarseM_B;
    if ( fineM < mStart[fineCtx][Eq_V] + Ref[fineCtx] || fineM > mEnd[fineCtx][Eq_V] - Ref[fineCtx] )
    {
        // First cells, A not available, take value at B
        // or last cells, B not available, take value at A
        coarseM_A = coarseM_B = coarseM;
    }
    else
    {
        // not at begin or end, inbetween
        if ( alpha > 0.5L )
        {
            // Nearer to B (B is the value of the related coarse point)
            coarseM_A = coarseM - 1;
            coarseM_B = coarseM;
        }
        else
        {
            // Nearer to A (A is the value of the related coarse point)
            coarseM_A = coarseM;
            coarseM_B = coarseM + 1;
        }

        // avoid adding the term "zero times an undefined value" to interpolatedValue
        if ( alpha == 0.0L )
                {
                        coarseM_B = coarseM_A;
                }
                else if ( alpha == 1.0L )
                {
                        coarseM_A = coarseM_B;
                }
    }

    double A, B;
    V_AVERAGE3D(coarseCtx,coarseM_A,coarseN,coarseK, v1, A);
    V_AVERAGE3D(coarseCtx,coarseM_B,coarseN,coarseK, v1, B);

    double interpolatedValue = ( 1.0L - alpha ) * A + alpha * B;

    // It seems that undefined values in the mapper very often have the value +- x.xxxxxe-26 in debug mode in VS2008.
    // The following check may give useful information.
    //
        // if ( (fabs(A) > 9.0e-27L && fabs(A) < 1.0e-25L) || (fabs(B) > 9.9e-27L && fabs(B) < 1.0e-25L) )
        // {
        //      printf("\nERROR: Undefined A/B used:\n");
        // }
    // printf("norm-v1(m=%d,M=%d): %8.6lf*%10.7lf A(%d,%d,%d) + %8.6lf*%10.7lf B(%d,%d,%d) = %10.7lf\n",
    //     fineM, coarseM,
    //     1.0-alpha, A, coarseM_A, coarseN, coarseK,
    //     alpha    , B, coarseM_B, coarseN, coarseK,
    //     interpolatedValue);

    return interpolatedValue;
}


double D3dFlowMapper::InterpolateTangentialV1(
    int fineCtx,
    int coarseCtx,
    int fineN,
    int coarseM,
    int coarseN,
    int coarseK
    )
{
    // From coarse to fine, interpolate: (1-alpha) * A + alpha * B
    //                         (A: value on n-down, B: value on n-up)

    // Determine alpha
    int nFromStart = fineN - nStart[fineCtx][Eq_U];
    double alpha = (double) ( ( nFromStart + 1) % Ref[fineCtx] ) / (double) Ref[fineCtx];

    // Determine indices for values at A and B
    int coarseN_A, coarseN_B;
    if ( fineN < nStart[fineCtx][Eq_U] + Ref[fineCtx] || fineN > nEnd[fineCtx][Eq_U] - Ref[fineCtx] )
    {
        // First cells, A not available, take value at B
        // or last cells, B not available, take value at A
        coarseN_A = coarseN_B = coarseN;
    }
    else
    {
        // not at begin or end, inbetween
                if ( alpha >= 0.5L )
        {
            // Nearer to B (B is the value of the related coarse point)
            coarseN_A = coarseN - 1;
            coarseN_B = coarseN;
        }
        else
        {
            // Nearer to A (A is the value of the related coarse point)
            coarseN_A = coarseN;
            coarseN_B = coarseN + 1;
        }

        // avoid adding the term "zero times an undefined value" to interpolatedValue
        if ( alpha == 0.0L )
                {
                        coarseN_B = coarseN_A;
                }
                else if ( alpha == 1.0L )
                {
                        coarseN_A = coarseN_B;
                }
    }

    double A, B;
    V_AVERAGE3D(coarseCtx,coarseM,coarseN_A,coarseK, v1, A);
    V_AVERAGE3D(coarseCtx,coarseM,coarseN_B,coarseK, v1, B);

    double interpolatedValue = ( 1.0L - alpha ) * A + alpha * B;

    // It seems that undefined values in the mapper very often have the value +- x.xxxxxe-26 in debug mode in VS2008.
    // The following check may give useful information.
    //
        // if ( (fabs(A) > 9.0e-27L && fabs(A) < 1.0e-25L) || (fabs(B) > 9.9e-27L && fabs(B) < 1.0e-25L) )
        // {
        //      printf("\nERROR: Undefined A/B used:\n");
        // }
    // printf("tang-v1(n=%d,N=%d): %8.6lf*%10.7lf A(%d,%d,%d) + %8.6lf*%10.7lf B(%d,%d,%d) = %10.7lf\n",
    //     fineN, coarseN,
    //     1.0-alpha, A, coarseM, coarseN_A, coarseK,
    //     alpha    , B, coarseM, coarseN_B, coarseK,
    //     interpolatedValue);

    return interpolatedValue;
}


double D3dFlowMapper::InterpolateS1(
    int fineCtx,
    int coarseCtx,
    int fineM,
    int fineN,
    int coarseM,
    int coarseN,
    int coarseK
    )
{
    // From coarse to fine, interpolate: (1-alpha) * A + alpha * B
    //                         (A: value on n-down, B: value on n-up)

    // Determine alpha
    bool topBottomMapper = ( Edge[ctx] == Edge_Bottom || Edge[ctx] == Edge_Top ) ? true
                                                                                 : false;
    int start = topBottomMapper ? mStart[fineCtx][Eq_Zeta]
                                : nStart[fineCtx][Eq_Zeta];
    int end   = topBottomMapper ? mEnd[fineCtx][Eq_Zeta]
                                : nEnd[fineCtx][Eq_Zeta];
    int index = topBottomMapper ? fineM
                                : fineN;

    int indexFromStart = index - start;
    double alpha = (double) (( (indexFromStart+1) + Ref[fineCtx]/2 ) % Ref[fineCtx]) / (double) Ref[fineCtx];

    // Determine indices for values at A and B
    int coarseM_A, coarseM_B, coarseN_A, coarseN_B;
    if ( index <= start + Ref[fineCtx]/2 || index >= end - Ref[fineCtx]/2 )
    {
        // First cells, A not available, take value at B
        // or last cells, B not available, take value at A
        coarseM_A = coarseM_B = coarseM;
        coarseN_A = coarseN_B = coarseN;
    }
    else
    {
        // not at begin or end, inbetween
        if ( alpha >= 0.5L )
        {
            // Nearer to B (B is the value of the related coarse point)
            if ( topBottomMapper )
            {
                coarseM_A = coarseM - 1;
                coarseM_B = coarseM;
                coarseN_A = coarseN_B = coarseN;
            }
            else
            {
                coarseN_A = coarseN - 1;
                coarseN_B = coarseN;
                coarseM_A = coarseM_B = coarseM;
            }
        }
        else
        {
            // Nearer to A (A is the value of the related coarse point)
            if ( topBottomMapper )
            {
                coarseM_A = coarseM;
                coarseM_B = coarseM + 1;
                coarseN_A = coarseN_B = coarseN;
            }
            else
            {
                coarseN_A = coarseN;
                coarseN_B = coarseN + 1;
                coarseM_A = coarseM_B = coarseM;
            }
        }

        // avoid adding the term "zero times an undefined value" to interpolatedValue
        if ( alpha == 0.0L )
                {
                        coarseM_B = coarseM_A;
                        coarseN_B = coarseN_A;
                }
                else if ( alpha == 1.0L )
                {
                        coarseM_A = coarseM_B;
                        coarseN_A = coarseN_B;
                }
    }

    double A, B;
    A = cI2D(coarseCtx,coarseM_A,coarseN_A,s1);
    B = cI2D(coarseCtx,coarseM_B,coarseN_B,s1);

    double interpolatedValue = ( 1.0L - alpha ) * A + alpha * B;

    // It seems that undefined values in the mapper very often have the value +- x.xxxxxe-26 in debug mode in VS2008.
    // The following check may give useful information.
    //
        // if ( (fabs(A) > 9.0e-27L && fabs(A) < 1.0e-25L) || (fabs(B) > 9.9e-27L && fabs(B) < 1.0e-25L) )
        // {
        //      printf("\nERROR: Undefined A/B used:\n");
        // }
    // printf("s1(n=%d,N=%d): %8.6lf*%10.7lf A(%d,%d,%d) + %8.6lf*%10.7lf B(%d,%d,%d) = %10.7lf\n",
    //     fineN, coarseN,
    //     1.0-alpha, A, coarseM_A, coarseN_A, coarseK,
    //     alpha    , B, coarseM_B, coarseN_B, coarseK,
    //     interpolatedValue);

    return interpolatedValue;
}


void D3dFlowMapper::FinishWang()
{
    Eq          eq;     // Location of unknown

    MAPDBG_FUN("D3dFlowMapper::FinishWang");

    eq = location[Seq_Zeta];

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        SET_OTHER_CTX;

        ON_DEBUG( DBLEV1,
            FMapLog((char*)"\nFinishWang for ctx %d (cntxt-Id %d)\n\n",
                            ctx, C[ctx]->GetContextId());
        )

        if ( Ref[ctx] < Ref[oCtx] )
        {
            //
            // Coarse Domain
            //

            // printf("FW:\t\tHDT's. %f %f\n", C[ctx]->Hdt, C[oCtx]->Hdt);

            MAP_CELLS_LOOP(ctx,eq)
            {
                int M_onEdge = m;
                int N_onEdge = n;
                int otherM_onEdge = oM;
                int otherN_onEdge = oN;

                // printf("coord %2d %2d %2d %2d\n",
                // M_onEdge,N_onEdge,otherM_onEdge,otherN_onEdge );

                MapScalar zetaCorrC = 0.0L;
                MapScalar zetaCorrF = 0.0L;
                MapScalar VertSum = 0.0L;
                MapScalar otherVertSum = 0.0L;
                MapScalar otherHorSum = 0.0L;
                MapScalar difference = 0.0L;
                MapScalar factor = 0.0L;

                //
                // Shift Zeta Coord to Normal Vel. Coords.
                //

                if ( Edge[ctx] == Edge_Right )
                {
                    M_onEdge--;
                    otherM_onEdge--;
                }
                if ( Edge[ctx] == Edge_Top )
                {
                    N_onEdge--;
                    otherN_onEdge--;
                }

                //
                // Correction of discharges in case of "closed cell"
                // at coarse grid interface
                //

                if ( (Edge[ctx] == Edge_Right || Edge[ctx] == Edge_Left )
                     &&   ( cI2D(ctx,M_onEdge,N_onEdge,kfu) == 0 )  )
                {
                    MAP_LAYERS_LOOP(oCtx)
                    {
                        MAP_REFINED_LOOP(oCtx,otherM_onEdge,otherN_onEdge)
                        {
                            ///V_SUM3D(oCtx,fM,fN,oK, qxk, otherVertSum);
                            otherVertSum = cI3D(oCtx,fM,fN,oK,qxk);
                            if ( fabs (otherVertSum) > EPSILON )
                            {
                                cI3D(oCtx,fM,fN,oK,qxk) = 0.0;
                                if ( Edge[oCtx] == Edge_Left )
                                {
                                    factor = -1.0;
                                    zetaCorrF = C[oCtx]->Hdt * factor * otherVertSum /
                                                     cI2D(oCtx,fM+1,fN,gsqs);
                                    cI2D(oCtx,fM+1,fN,s1) += (float) zetaCorrF;
                                }
                                else
                                {
                                    factor =  1.0;
                                    zetaCorrF = C[oCtx]->Hdt * factor * otherVertSum /
                                                     cI2D(oCtx,fM,fN,gsqs);
                                    cI2D(oCtx,fM,fN,s1) += (float) zetaCorrF;
                                }
                                // printf("FW-dry-ref-x (%2d %2d %2d) %10.7f %10.7f\n",fM,fN,oK,zetaCorrF,otherVertSum);
                            }
                        }
                    }
                }
                if ( (Edge[ctx] == Edge_Top || Edge[ctx] == Edge_Bottom )
                     &&   ( cI2D(ctx,M_onEdge,N_onEdge,kfv) == 0 )  )
                {
                    MAP_LAYERS_LOOP(oCtx)
                    {
                        MAP_REFINED_LOOP(oCtx,otherM_onEdge,otherN_onEdge)
                        {
                            // V_SUM3D(oCtx,fM,fN,oK, qyk, otherVertSum);
                            otherVertSum = cI3D(oCtx,fM,fN,oK,qyk);
                            if ( fabs (otherVertSum) > EPSILON )
                            {
                                cI3D(oCtx,fM,fN,oK,qyk) = 0.0;
                                if ( Edge[oCtx] == Edge_Bottom )
                                {
                                    factor = -1.0;
                                    zetaCorrF = C[oCtx]->Hdt * factor * otherVertSum /
                                                     cI2D(oCtx,fM,fN+1,gsqs);
                                    cI2D(oCtx,fM,fN+1,s1) += (float) zetaCorrF;
                                }
                                else
                                {
                                    factor =  1.0;
                                    zetaCorrF = C[oCtx]->Hdt * factor * otherVertSum /
                                                     cI2D(oCtx,fM,fN,gsqs);
                                    cI2D(oCtx,fM,fN,s1) += (float) zetaCorrF;
                                }
                                // printf("FW-dry-ref-y (%2d %2d %2d) %10.7f %10.7f\n",fM,fN,oK,zetaCorrF,otherVertSum);
                            }
                        }
                    }
                }

                //
                // Correction of discharges in case of "closed cells"
                // at all corresponding fine grid cell interfaces,
                // while coarse grid cell interface is still open
                //
                // at first for Left/Right coupling
                //
                int otherKfuSum = 0;
                MapScalar qxkold = 0.0L;

                if ( (Edge[ctx] == Edge_Right || Edge[ctx] == Edge_Left )
                     &&   ( cI2D(ctx,M_onEdge,N_onEdge,kfu) == 1 )  )
                {
                    MAP_LAYERS_LOOP(ctx)
                    {
                        otherKfuSum = 0;
                        MAP_REFINED_LOOP(oCtx,otherM_onEdge,otherN_onEdge)
                        {
                            otherKfuSum += cI2D(oCtx,fM,fN,kfu);
                        }
                        if ( otherKfuSum == 0 )
                        {
                            // printf("Discharge_KFU_correctie (%2d %2d) (%2d %2d)\n",
                            //          M_onEdge,N_onEdge,otherM_onEdge,otherN_onEdge);
                            V_SUM3D(ctx,M_onEdge,N_onEdge,k,qxk,qxkold);
                            cI3D(ctx,M_onEdge,N_onEdge,k,qxk) = 0.0;
                            if ( Edge[ctx] == Edge_Left )
                            {
                                factor = -1.0;
                                zetaCorrC = C[ctx]->Hdt * factor * qxkold /
                                              cI2D(ctx,M_onEdge+1,N_onEdge,gsqs);
                                cI2D(ctx,M_onEdge+1,N_onEdge,s1) += (float) zetaCorrC;
                                cI2D(oCtx,otherM_onEdge+1,otherN_onEdge,s1) = cI2D(ctx,M_onEdge+1,N_onEdge,s1);
                            }
                            else
                            {
                                factor =  1.0;
                                zetaCorrC = C[ctx]->Hdt * factor * qxkold /
                                              cI2D(ctx,M_onEdge,N_onEdge,gsqs);
                                cI2D(ctx,M_onEdge,N_onEdge,s1) += (float) zetaCorrC;
                                cI2D(oCtx,otherM_onEdge,otherN_onEdge,s1) = cI2D(ctx,M_onEdge,N_onEdge,s1);
                            }
                        }
                    }
                }

                //
                // now for Bottom/Top coupling
                //
                int otherKfvSum = 0;
                MapScalar qykold = 0.0L;

                if ( (Edge[ctx] == Edge_Top || Edge[ctx] == Edge_Bottom )
                     &&   ( cI2D(ctx,M_onEdge,N_onEdge,kfv) == 1 )  )
                {
                    MAP_LAYERS_LOOP(ctx)
                    {
                        otherKfvSum = 0;
                        MAP_REFINED_LOOP(oCtx,otherM_onEdge,otherN_onEdge)
                        {
                            otherKfvSum += cI2D(oCtx,fM,fN,kfv);
                        }
                        if ( otherKfvSum == 0 )
                        {
                            // printf("Discharge_KFV_correctie (%2d %2d) (%2d %2d)\n",M_onEdge,N_onEdge,otherM_onEdge,otherN_onEdge);
                            V_SUM3D(ctx,M_onEdge,N_onEdge,k,qyk,qykold);
                            cI3D(ctx,M_onEdge,N_onEdge,k,qyk) = 0.0;
                            if ( Edge[ctx] == Edge_Bottom )
                            {
                                factor = -1.0;
                                zetaCorrC = C[ctx]->Hdt * factor * qykold /
                                              cI2D(ctx,M_onEdge,N_onEdge+1,gsqs);
                                cI2D(ctx,M_onEdge,N_onEdge+1,s1) += (float) zetaCorrC;
                                cI2D(oCtx,otherM_onEdge,otherN_onEdge+1,s1) = cI2D(ctx,M_onEdge,N_onEdge+1,s1);
                            }
                            else
                            {
                                factor =  1.0;
                                zetaCorrC = C[ctx]->Hdt * factor * qykold /
                                              cI2D(ctx,M_onEdge,N_onEdge,gsqs);
                                cI2D(ctx,M_onEdge,N_onEdge,s1) += (float) zetaCorrC;
                                cI2D(oCtx,otherM_onEdge,otherN_onEdge,s1) = cI2D(ctx,M_onEdge,N_onEdge,s1);
                            }
                        }
                    }
                }

                //
                // Correction of discharges in normal situation
                // at coarse grid interface (thus "open cell")
                //

                MAP_LAYERS_LOOP(ctx)
                {
                    otherHorSum = 0.0;
                    MAP_REFINED_LOOP(oCtx,otherM_onEdge,otherN_onEdge)
                    {
                        if ( (Edge[ctx] == Edge_Right) ||
                             (Edge[ctx] == Edge_Left )   )
                        {
                            V_SUM3D(oCtx,fM,fN,oK, qxk, otherVertSum);
                        }
                        else
                        {
                            V_SUM3D(oCtx,fM,fN,oK, qyk, otherVertSum);
                        }
                        otherHorSum += otherVertSum;
                    }

                    if ( (Edge[ctx] == Edge_Right) ||
                         (Edge[ctx] == Edge_Left )   )
                    {
                        V_SUM3D(ctx,M_onEdge,N_onEdge,k, qxk, VertSum);
                        cI3D(ctx,M_onEdge,N_onEdge,k,qxk) = (float) otherHorSum;
                    }
                    else
                    {
                        V_SUM3D(ctx,M_onEdge,N_onEdge,k, qyk, VertSum);
                        cI3D(ctx,M_onEdge,N_onEdge,k,qyk) = (float) otherHorSum;
                    }
                    difference += otherHorSum - VertSum;
                    // printf("Diff-zt-ref %2d %2d %2d %10.7f %10.7f %10.7f\n",
                        // M_onEdge,N_onEdge,k,difference,otherHorSum,VertSum);
                }

                //
                // Shift to first internal grid cell
                //

                if ( (Edge[ctx] == Edge_Left) )
                {
                    M_onEdge++;
                }

                if ( (Edge[ctx] == Edge_Bottom) )
                {
                    N_onEdge++;
                }

                zetaCorrC = difference *
                        C[ctx]->Hdt / cI2D(ctx,M_onEdge,N_onEdge,gsqs) ;

                if ( (Edge[ctx] == Edge_Right) ||
                     (Edge[ctx] == Edge_Top)    )
                {
                    cI2D(ctx,M_onEdge,N_onEdge,s1) -= (float) zetaCorrC;
                }
                else
                {
                    cI2D(ctx,M_onEdge,N_onEdge,s1) += (float) zetaCorrC;
                }
                // printf("FW-cor-Ref %2d %2d %10.5e\n",M_onEdge,N_onEdge,zetaCorrC);
            }
        }
        else if ( ( Ref[ctx] == Ref[oCtx] ) &&
              ( C[ctx]->kMax <= C[oCtx]->kMax ) )
        {
            //
            // Only correction in domain with smallest number of layers
            //
            MAP_CELLS_LOOP(ctx,eq)
            {
                int M_onEdge = m;
                int N_onEdge = n;
                int otherM_onEdge = oM;
                int otherN_onEdge = oN;
                // printf("coord %2d %2d %2d %2d\n",
                // M_onEdge,N_onEdge,otherM_onEdge,otherN_onEdge );

                //
                // Shift Zeta Coord to Normal Vel. Coords.
                //

                if ( Edge[ctx] == Edge_Right )
                {
                    M_onEdge--;
                    otherM_onEdge--;
                }
                if ( Edge[ctx] == Edge_Top )
                {
                    N_onEdge--;
                    otherN_onEdge--;
                }

                MapScalar zetaCorr = 0.0L;
                MapScalar VertSum = 0.0L;
                MapScalar otherVertSum = 0.0L;
                MapScalar difference = 0.0L;
                MAP_LAYERS_LOOP(ctx)
                {
                    if ( (Edge[ctx] == Edge_Right) ||
                     (Edge[ctx] == Edge_Left )  )
                    {
                        V_SUM3D(oCtx,otherM_onEdge,otherN_onEdge,oK, qxk, otherVertSum);
                    }
                    else
                    {
                        V_SUM3D(oCtx,otherM_onEdge,otherN_onEdge,oK, qyk, otherVertSum);
                    }

                    if ( (Edge[ctx] == Edge_Right) ||
                     (Edge[ctx] == Edge_Left )  )
                    {
                        V_SUM3D(ctx,M_onEdge,N_onEdge,k, qxk, VertSum);
                        cI3D(ctx,M_onEdge,N_onEdge,k,qxk) = (float) otherVertSum;
                    }
                    else
                    {
                        V_SUM3D(ctx,M_onEdge,N_onEdge,k, qyk, VertSum);
                        cI3D(ctx,M_onEdge,N_onEdge,k,qyk) = (float) otherVertSum;
                    }
                    difference += otherVertSum - VertSum;
                    // printf("Diff-zt-noref %2d %2d %2d %10.7f %10.7f %10.7f\n",
                        // M_onEdge,N_onEdge,k,difference,otherVertSum,VertSum);
                }

                //
                // Shift to Zeta Coords.
                //

                if ( (Edge[ctx] == Edge_Left) )
                {
                    M_onEdge++;
                }

                if ( (Edge[ctx] == Edge_Bottom) )
                {
                    N_onEdge++;
                }

                zetaCorr = difference *
                C[ctx]->Hdt / cI2D(ctx,M_onEdge,N_onEdge,gsqs) ;

                if ( (Edge[ctx] == Edge_Right) ||
                     (Edge[ctx] == Edge_Top)    )
                {
                    cI2D(ctx,M_onEdge,N_onEdge,s1) -= (float) zetaCorr;
                }

                else
                {
                    cI2D(ctx,M_onEdge,N_onEdge,s1) += (float) zetaCorr;
                }
                // printf("FW-cor-NoRef %2d %2d %2d %2d %10.7f %10.7f %10.7f\n",M_onEdge,N_onEdge,C[ctx]->kMax,C[oCtx]->kMax,zetaCorr,otherVertSum,VertSum);
            }
        }
    }
}


int D3dFlowMapper::CheckForDry()
{
    int     myWet=1;        // boolean: dry points in my domain?
    int     allWet=0;       // boolean: dry points anywhere?
    DDMesg    * dryMess=NULL;       // message from FLOW
    int     nMess=0;        // #messages (should be 2)

    MAPDBG_FUN("D3dFlowMapper::CheckForDry");

    //
    // Check if there are any dry points in all subdomains
    // -    The barrier-minimum function returns 1 or 0
    //

    while ( (dryMess = mapMess->GetInMess() ) != NULL )
    {
        if ( dryMess->value != 0 )
        {
            myWet = 0;
        }
        nMess++;
    }

    if ( nMess != 2 )
    {
        throw new Exception (true, "Didn't receive 2 dry messages");
    }

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER \"%s\" calls minimumbarrier (myWet %d)", IteratorSelf()->name, myWet);
    allWet = MinimumBarrier(myWet);
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "MAPPER \"%s\" minimumbarrier returned (allWet %d)", IteratorSelf()->name, allWet);

    return (! allWet);

}


void D3dFlowMapper::CopySediment(
    Seq         seq                     // Type of variable
    )
{
    Eq          eq = location[seq];// type of equation location

    MAPDBG_FUN("D3dFlowMapper::CopySediment");

    for ( ctx = 0 ; ctx < NR_CNTXTS ; ctx++ )
    {
        oCtx = 1 - ctx;

        switch(seq)
        {
            case Seq_U:

                                // m,n,oM,oN are different comparing Edge_Left to Edge_Right
                                // copy sbuu only for Edge_Left for both copying directions:
                                // if zero, overwrite
             if ( l2r && Edge[ctx] == Edge_Left )
             {
                if ( Ref[ctx] < Ref[oCtx] )
                {
                   // oCtx is refined compared to ctx
                   MAP_CELLS_LOOP(ctx,eq)
                   {
                      // printf("coor-coarse (%2d %2d) <--?--> (%2d %2d)\n",m,n,oM,oN);
                      for ( l = 1 ; l <= C[ctx]->lSedtt ; l++ )
                      {
                        MapScalar totsed = 0.0L;

                        MAP_REFINED_LOOP(oCtx,oM+1,oN)
                        {
                                                        // Do not use fM+1 but fM!
                           totsed += cI3D(oCtx,fM,fN,l,sbuu);
                        }
                                                if ( cI3D(ctx,m+1,n,l,sbuu) == 0.0L )
                                                {
                                                        cI3D(ctx,m+1,n,l,sbuu) = (float) totsed / (float) nHorRef;
                                                        // printf("   Sed-SBUU-coarse (%2d %2d %2d)<-- %10.7f\n",m+1,n,l,cI3D(ctx,m+1,n,l,sbuu));
                                                }
                                                else
                                                {
                                                        MAP_REFINED_LOOP(oCtx,oM+1,oN)
                                                        {
                                                                // Do not use fM+1 but fM!
                                                                if ( cI3D(oCtx,fM,fN,l,sbuu) == 0.0L )
                                                                {
                                                                        cI3D(oCtx,fM,fN,l,sbuu) = cI3D(ctx,m+1,n,l,sbuu);
                                                                        // printf("   Sed-SBUU-fine (%2d %2d %2d)<-- %10.7f\n",fM,fN,l,cI3D(oCtx,fM,fN,l,sbuu));
                                                                }
                                                        }
                                                }
                      }
                   }
                }
                else if ( Ref[ctx] > Ref[oCtx] )
                {
                                        // ctx is refined compared to oCtx
                                        // MAP_CELLS_LOOP(oCtx)  does not work!
                                        // loop over the refined side and add sbuu to totsed
                                        // the for-lsed-loop and the map-cells-m,n-loop must be swapped
                                        // When the last refined cell connected to oM,oN is handled, totsed can be used and reset
                  for ( l = 1 ; l <= C[ctx]->lSedtt ; l++ )
                  {
                    MapScalar totsed = 0.0L;
                                        int refCount = 0;

                                    MAP_CELLS_LOOP(ctx,eq)
                                    {
                      // printf("coor-fine (%2d %2d) <--?--> (%2d %2d)\n",m,n,oM,oN);
                                                refCount++;
                        totsed += cI3D(ctx,m+1,n,l,sbuu);
                                                if ( cI3D(ctx,m+1,n,l,sbuu) == 0.0L )
                                                {
                                                        cI3D(ctx,m+1,n,l,sbuu) = cI3D(oCtx,oM+1,oN,l,sbuu);
                                                        // printf("   Sed-SBUU-fine (%2d %2d %2d)<-- %10.7f\n",m+1,n,l,cI3D(ctx,m+1,n,l,sbuu));
                                                }
                                                if ( refCount == Ref[ctx] )
                                                {
                                                        if ( cI3D(oCtx,oM+1,oN,l,sbuu) == 0.0L )
                                                        {
                                                                cI3D(oCtx,oM+1,oN,l,sbuu) = (float) totsed / (float) Ref[ctx];
                                                                // printf("   Sed-SBUU-coarse (%2d %2d %2d)<-- %10.7f\n",oM+1,oN,l,cI3D(oCtx,oM+1,oN,l,sbuu));
                                                        }
                                                        totsed = 0.0L;
                                                        refCount = 0;
                                                }
                      }
                   }
                }
                else
                {
                   // no grid refinement
                   MAP_CELLS_LOOP(ctx,eq)
                   {
                      // printf("coor (%2d %2d) <--?--> (%2d %2d)\n",m,n,oM,oN);
                      for ( l = 1 ; l <= C[ctx]->lSedtt ; l++ )
                      {
                                                        if ( cI3D(ctx,m+1,n,l,sbuu) == 0.0L )
                                                        {
                                                                cI3D(ctx,m+1,n,l,sbuu)= cI3D(oCtx,oM+1,oN,l,sbuu);
                                                                // printf("   Sed-SBUU-norm (%2d %2d %2d)<-- (%2d %2d) %10.7f\n",m+1,n,l,oM+1,oN,cI3D(ctx,m+1,n,l,sbuu));
                                                        }
                                                        else
                                                        {
                                                                cI3D(oCtx,oM+1,oN,l,sbuu)= cI3D(ctx,m+1,n,l,sbuu);
                                                                // printf("   Sed-SBUU-norm (%2d %2d %2d)<-- (%2d %2d) %10.7f\n",oM+1,oN,l,m+1,n,cI3D(oCtx,oM+1,oN,l,sbuu));
                                                        }
                      }
                   }
                }
             }
            break;

            case Seq_V:

                                // m,n,oM,oN are different comparing Edge_Bottom to Edge_Top
                                // copy sbvv only for Edge_Bottom for both copying directions:
                                // if zero, overwrite
             if ( b2t && Edge[ctx] == Edge_Bottom )
             {
                if ( Ref[ctx] < Ref[oCtx] )
                {
                   // oCtx is refined compared to ctx
                   MAP_CELLS_LOOP(ctx,eq)
                   {
                      // printf("coor-coarse (%2d %2d) <--?--> (%2d %2d)\n",m,n,oM,oN);
                      for ( l = 1 ; l <= C[ctx]->lSedtt ; l++ )
                      {
                        MapScalar totsed = 0.0L;

                        MAP_REFINED_LOOP(oCtx,oM,oN+1)
                        {
                                                        // Do not use fN+1 but fN!
                           totsed += cI3D(oCtx,fM,fN,l,sbvv);
                        }
                                                if ( cI3D(ctx,m,n+1,l,sbvv) == 0.0L )
                                                {
                                                        cI3D(ctx,m,n+1,l,sbvv) = (float) totsed / (float) nHorRef;
                                                        // printf("   Sed-SBVV-coarse (%2d %2d %2d)<-- %10.7f\n",m,n+1,l,cI3D(ctx,m,n+1,l,sbvv));
                                                }
                                                else
                                                {
                                                        MAP_REFINED_LOOP(oCtx,oM,oN+1)
                                                        {
                                                                // Do not use fN+1 but fN!
                                                                if ( cI3D(oCtx,fM,fN,l,sbvv) == 0.0L )
                                                                {
                                                                        cI3D(oCtx,fM,fN,l,sbvv) = cI3D(ctx,m,n+1,l,sbvv);
                                                                        // printf("   Sed-SBVV-fine (%2d %2d %2d)<-- %10.7f\n",fM,fN,l,cI3D(oCtx,fM,fN,l,sbvv));
                                                                }
                                                        }
                                                }
                      }
                   }
                }
                else if ( Ref[ctx] > Ref[oCtx] )
                {
                                        // ctx is refined compared to oCtx
                                        // MAP_CELLS_LOOP(oCtx)  does not work!
                                        // loop over the refined side and add sbvv to totsed
                                        // the for-lsed-loop and the map-cells-m,n-loop must be swapped
                                        // When the last refined cell connected to oM,oN is handled, totsed can be used and reset
                  for ( l = 1 ; l <= C[ctx]->lSedtt ; l++ )
                  {
                    MapScalar totsed = 0.0L;
                                        int refCount = 0;

                                    MAP_CELLS_LOOP(ctx,eq)
                                    {
                        // printf("coor-fine (%2d %2d) <--?--> (%2d %2d)\n",m,n,oM,oN);
                                                refCount++;
                        totsed += cI3D(ctx,m,n+1,l,sbvv);
                                                if ( cI3D(ctx,m,n+1,l,sbvv) == 0.0L )
                                                {
                                                        cI3D(ctx,m,n+1,l,sbvv) = cI3D(oCtx,oM,oN+1,l,sbvv);
                                                        // printf("   Sed-SBVV-fine (%2d %2d %2d)<-- %10.7f\n",m,n+1,l,cI3D(ctx,m,n+1,l,sbvv));
                                                }
                                                if ( refCount == Ref[ctx] )
                                                {
                                                        if ( cI3D(oCtx,oM,oN+1,l,sbvv) == 0.0L )
                                                        {
                                                                cI3D(oCtx,oM,oN+1,l,sbvv) = (float) totsed / (float) Ref[ctx];
                                                                // printf("   Sed-SBVV-coarse (%2d %2d %2d)<-- %10.7f\n",oM,oN+1,l,cI3D(oCtx,oM,oN+1,l,sbvv));
                                                        }
                                                        totsed = 0.0L;
                                                        refCount = 0;
                                                }
                      }
                   }
                }
                else
                {
                   // no grid refinement
                   MAP_CELLS_LOOP(ctx,eq)
                   {
                      // printf("coor (%2d %2d) <--?--> (%2d %2d)\n",m,n,oM,oN);
                      for ( l = 1 ; l <= C[ctx]->lSedtt ; l++ )
                      {
                                                  if ( cI3D(ctx,m,n+1,l,sbvv) == 0.0L )
                                                  {
                                                        cI3D(ctx,m,n+1,l,sbvv)= cI3D(oCtx,oM,oN+1,l,sbvv);
                                                        // printf("   Sed-SBVV-norm (%2d %2d %2d)<-- (%2d %2d) %10.7f\n",m,n+1,l,oM,oN+1,cI3D(oCtx,m,n+1,l,sbvv));
                                                  }
                                                  else
                                                  {
                                                        cI3D(oCtx,oM,oN+1,l,sbvv)= cI3D(ctx,m,n+1,l,sbvv);
                                                        // printf("   Sed-SBVV-norm (%2d %2d %2d)<-- (%2d %2d) %10.7f\n",oM,oN+1,l,m,n+1,cI3D(oCtx,oM,oN+1,l,sbvv));
                                                  }
                      }
                   }
                }
             }
            break;

            case Seq_Zeta:

                                MAP_CELLS_LOOP(ctx,eq)
                                {
                                        cI2D(ctx,m,n,dps) = cI2D(oCtx,oM,oN,dps);
                                        // printf("Sed-Depchg (%2d %2d) <-- (%2d %2d) %10.7f\n",m,n,oM,oN,cI2D(ctx,m,n,depchg));
                                }
            break;

        default:
        FLOW2D3D->dd->log->Write (Log::WARN, "unexpected case CopySediment");
        break;
        }
    }
}

