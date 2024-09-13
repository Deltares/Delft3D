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
// $Id: context_distrib_functs.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/context_distrib_functs.cpp $
//------------------------------------------------------------------------------
//  Class: D3dFlowContext
//  Context Attacher, base class (parent for Flow side Map side, Gaws Side)
//
//  Functions to support a distributed context.
//  - determine the strip to be communicate between flow and mapper.
//  - setup an administration of sizes and strides for the 2/3/4D D3D flow vars.
//  - create a var info collection for all vars that have to be communicated
//    between flow and mapper.
//
//  Stef.Hummel@deltares.nl
//  Erik.deGoede@deltares.nl
//  Adri.Mourits@deltares.nl
//  1 jun 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


void D3dFlowContext::DetermineStripSize(void)
{
    int eq, mEndMax, nEndMax;

    //
    // Determination of the strip with computational cells (i.e. indices
    // independent of equation type) that will be communicated between
    // Flow Context and Mapper Context:

    //
    // Determine minima and maxima
    //

    mStartMin = mStart[eq = 0];
    mEndMax   = mEnd  [eq = 0];
    nStartMin = nStart[eq = 0];
    nEndMax   = nEnd  [eq = 0];

    for (eq = 1; eq < NR_EQ; eq++)
    {
        if (mStartMin > mStart[eq])
        {
            mStartMin = mStart[eq];
        }
        if (mEndMax < mEnd[eq])
        {
            mEndMax = mEnd[eq];
        }
        if (nStartMin > nStart[eq])
        {
            nStartMin = nStart[eq];
        }
        if (nEndMax < nEnd[eq])
        {
            nEndMax = nEnd[eq];
        }
    }

    //
    // Extend the length of the strip on both sides,
    // and extend it 1 cell to the interior of the domain.
    //
    switch ( this->edgeType )
    {
#if 1
        case Edge_Left:
            mEndMax++;
            break;
        case Edge_Right:
            mStartMin--;
            break;
        case Edge_Bottom:
            nEndMax++;
            break;
        case Edge_Top:
            nStartMin--;
            break;
#else
        case Edge_Left:
            mEndMax++;
            nStartMin--;
            nEndMax++;
            break;
        case Edge_Right:
            mStartMin--;
            nStartMin--;
            nEndMax++;
            break;
        case Edge_Bottom:
            mStartMin--;
            mEndMax++;
            nEndMax++;
            break;
        case Edge_Top:
            mStartMin--;
            mEndMax++;
            nStartMin--;
            break;
#endif
    }

    //
    // Determine size
    //

    mStripSize = (mEndMax - mStartMin) + 1;
    nStripSize = (nEndMax - nStartMin) + 1;

    //
    // Set value for the Access Macro's (I2D, etc.).
    //

    this->mArraySize = this->mStripSize;
    this->nArraySize = this->nStripSize;
    this->mArrayOffset = - this->mStartMin; // TDC 2 + dDb - 1 - this->mStartMin;
    this->nArrayOffset = - this->nStartMin; //         dDb - 1 - this->nStartMin;

}


int D3dFlowContext::FillBlockAdmin(void)
{
    //
    // Fill the offset, the sizes and the strides for data the
    // 1D, 2D, 2D*LSedtt, 3D, and 4D (=3D*lStsci) strips to be communicated.
    //

    // print debug info on strip si
    int off =  ( (this->mStartMin) + (mArrayOffset) ) * nArraySize +
               ( (this->nStartMin) + (nArrayOffset) );

    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "FlowConfig \"%s\" for Mapper \"%s\": mAO %4d, nAO %4d, mAS %4d, nAS %4d",
                    this->flowIterator->name, // ConfigBlob()->Address(),
                    this->mapperIterator->name,
                    mArrayOffset, nArrayOffset, mArraySize, nArraySize);
    FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR, "FlowConfig \"%s\" for Mapper \"%s\": mStartMin %4d, mEndMax %4d, mStripSize %4d, nStripSize %4d, OFF %4d",
                    this->flowIterator->name, // ConfigBlob()->Address(),
                    this->mapperIterator->name,
                    mStartMin, nStartMin, mStripSize , nStripSize , off);

    for (int pntType = Points_Tangential ; pntType <= Points_Normal ; pntType++)
    {
        for (int dim = Dim_2d; dim < NR_DIM_TYPES; dim++)
        {

            //
            // For all 2/3/4D var's:
            // - the offset is the lower corner of the strip to be communicated
            // - the strides are the array-sizes, i.e. the sizes of the strips
            //   as they are created at the mapper side, or the sizes of the
            //   full domain at the flow side
            // - the amounts in 1st (==N) and 2nd (==M) direction are
            //   the sizes of the strips to be communicated.
            // - the amount in 3th dimension is (kmax | kmax+1 | lSedtt).
            //

            d3dblockInfo[dim][pntType].Offset = O2D(this->mStartMin, this->nStartMin);

            d3dblockInfo[dim][pntType].Amount_D1 = this->nStripSize;
            d3dblockInfo[dim][pntType].Stride_D1 = 1;

            if ( ( pntType == Points_Tangential ) &&
                 ( this->edgeType == Edge_Left || this->edgeType == Edge_Right ) )
            {
                d3dblockInfo[dim][pntType].Amount_D1--;
            }

            d3dblockInfo[dim][pntType].Amount_D2 = this->mStripSize;
            d3dblockInfo[dim][pntType].Stride_D2 = this->nArraySize;

            if ( ( pntType == Points_Tangential ) &&
                 ( this->edgeType == Edge_Top || this->edgeType == Edge_Bottom ) )
            {
                d3dblockInfo[dim][pntType].Amount_D2--;
            }

            if (dim == Dim_2d)
            {
                //
                // 2D finished, set size
                //

                d3dblockInfo[dim][pntType].Size = d3dblockInfo[dim][pntType].Amount_D1 *
                                         d3dblockInfo[dim][pntType].Amount_D2;
            }
            else
            {
                //
                // 3D/4D var.s. In the 3-th dimension:
                // - the stride is the size of a memory slice for a 2-dim. array.
                // - the amount of values is the 3th dimension (kmax | kmax+1 | lSedtt).
                //

                d3dblockInfo[dim][pntType].Stride_D3 = this->mArraySize * this->nArraySize;

                if ( (dim == Dim_3d) || (dim == Dim_4d) )
                {
                    d3dblockInfo[dim][pntType].Amount_D3 = kMax;
                }
                else if ( (dim == Dim_3dt) )
                {
                    d3dblockInfo[dim][pntType].Amount_D3 = kMax + 2;
                }
                else if ( (dim == Dim_4dt) )
                {
                    d3dblockInfo[dim][pntType].Amount_D3 = kMax + 1;
                    // This is not used (because turbulence is not communicated) ?
                }
                else if ( dim == Dim_3dLsedtt )
                {
                    d3dblockInfo[dim][pntType].Amount_D3 = lSedtt;
                }

                if ( (dim == Dim_3d) || (dim == Dim_3dt) || (dim == Dim_3dLsedtt) )
                {
                    //
                    // 3D finished, set size
                    //
                    d3dblockInfo[dim][pntType].Size = d3dblockInfo[dim][pntType].Amount_D1 *
                                             d3dblockInfo[dim][pntType].Amount_D2 *
                                             d3dblockInfo[dim][pntType].Amount_D3;
                }
                else if ((dim == Dim_4d) || (dim == Dim_4dt))
                {
                    //
                    // 4D var.s. In the 4-th dimension:
                    // - the stride is (kmax | kmax+1 | lSedtt) times the size of
                    //   a memory slice for a 2-dim. array.
                    // - the amount of values is the 4th dimension (=lStsci).
                    //

                    d3dblockInfo[dim][pntType].Stride_D4 = d3dblockInfo[dim][pntType].Amount_D3 *
                                                  this->mArraySize * this->nArraySize;

                    d3dblockInfo[dim][pntType].Amount_D4 = lStsci;

                    //
                    // 4D finished, set size
                    //
                    d3dblockInfo[dim][pntType].Size = d3dblockInfo[dim][pntType].Amount_D1 *
                                             d3dblockInfo[dim][pntType].Amount_D2 *
                                             d3dblockInfo[dim][pntType].Amount_D3 *
                                             d3dblockInfo[dim][pntType].Amount_D4;
                }
            }
        }
    }

    this->DumpBlockAdmin();
    return 0;
}


void D3dFlowContext::DumpBlockAdmin(void)
{
    for (int pntType = Points_Tangential ; pntType <= Points_Normal ; pntType++)
    {
        for (int dim = Dim_2d; dim < NR_DIM_TYPES; dim++)
        {
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR,
                 "BA: Offset=%d, Size=%d, aD1=%d, sD1=%d",
                            d3dblockInfo[dim][pntType].Offset,
                            d3dblockInfo[dim][pntType].Size,
                            d3dblockInfo[dim][pntType].Amount_D1,
                            d3dblockInfo[dim][pntType].Stride_D1);
            FLOW2D3D->dd->log->Write (Log::DDMAPPER_MINOR,
                 "BA: aD2=%d, sD2=%d, aD3=%d, sD3=%d, aD4=%d, sD4=%d",
                            d3dblockInfo[dim][pntType].Amount_D2,
                            d3dblockInfo[dim][pntType].Stride_D2,
                            d3dblockInfo[dim][pntType].Amount_D3,
                            d3dblockInfo[dim][pntType].Stride_D3,
                            d3dblockInfo[dim][pntType].Amount_D4,
                            d3dblockInfo[dim][pntType].Stride_D4);
        }
    }
}


void D3dFlowContext::FillVarInfoCollection(void)
{

    MAPDBG_FUN2("D3dFlowContext::FillVarInfoCollection");

    if (varInfoColl != NULL)
    {

        //
        // Create Block Info and Vars for 1D/2D/3D var's.
        //

        //
        // 1-dimensional variables (0:kmax, e.g thick)
        //

        BlockInfo * blockInfo1D = varInfoColl->AddBlockInfo(
                        0,          // offset
                        kMax,       // size
                        kMax,       // amount in first dim.
                        1           // amount in first dim.
                        );

        varInfoColl->AddVarInfo(VarThick, &thick[0], REAL_FP_TYPE, blockInfo1D);

        //
        // Block info for 2-dimensional var.s. Info on sizes of communicated strips
        // for normal and tangential velocity points
        //

        BlockInfo * blockInfo2DNormal = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_2d][Points_Normal].Offset,
                        d3dblockInfo[Dim_2d][Points_Normal].Size,
                        d3dblockInfo[Dim_2d][Points_Normal].Amount_D1,
                        d3dblockInfo[Dim_2d][Points_Normal].Stride_D1,
                        d3dblockInfo[Dim_2d][Points_Normal].Amount_D2,
                        d3dblockInfo[Dim_2d][Points_Normal].Stride_D2);

        BlockInfo * blockInfo2DTangential = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_2d][Points_Tangential].Offset,
                        d3dblockInfo[Dim_2d][Points_Tangential].Size,
                        d3dblockInfo[Dim_2d][Points_Tangential].Amount_D1,
                        d3dblockInfo[Dim_2d][Points_Tangential].Stride_D1,
                        d3dblockInfo[Dim_2d][Points_Tangential].Amount_D2,
                        d3dblockInfo[Dim_2d][Points_Tangential].Stride_D2);

        //
        // 2-dimensional var.s in Zeta-points
        // (strip size for zeta points == Strip size for normal velocity points)
        //

        varInfoColl->AddVarInfo(VarKcs , &kcs [0], IntegerType, blockInfo2DNormal);
        varInfoColl->AddVarInfo(VarGsqs, &gsqs[0], REAL_FP_TYPE  , blockInfo2DNormal);

        varInfoColl->AddVarInfo(VarDps , &dps [0], REAL_PREC_TYPE  , blockInfo2DNormal);

        varInfoColl->AddVarInfo(VarS0  , &s0  [0], REAL_FP_TYPE  , blockInfo2DNormal);
        varInfoColl->AddVarInfo(VarS1  , &s1  [0], REAL_FP_TYPE  , blockInfo2DNormal);

        if (lSedtt>0)
        {
            varInfoColl->AddVarInfo(VarDepchg, &depchg[0], REAL_FP_TYPE, blockInfo2DNormal);
        }

        //
        // 2-dimensional var.s in U-points
        // Depending on Edge type, the U points are the tangential or the normal points
        //

        BlockInfo * blockInfo_2D_U = ( this->edgeType == Edge_Left ||
                                       this->edgeType == Edge_Right ) ? blockInfo2DNormal
                                                                      : blockInfo2DTangential;

        varInfoColl->AddVarInfo(VarKcu   , &kcu   [0], IntegerType, blockInfo_2D_U);
        varInfoColl->AddVarInfo(VarKfu   , &kfu   [0], IntegerType, blockInfo_2D_U);
        varInfoColl->AddVarInfo(VarDpu   , &dpu   [0], REAL_FP_TYPE  , blockInfo_2D_U);
        varInfoColl->AddVarInfo(VarGuu   , &guu   [0], REAL_FP_TYPE  , blockInfo_2D_U);
        varInfoColl->AddVarInfo(VarZ0urou, &z0urou[0], REAL_FP_TYPE  , blockInfo_2D_U);
        varInfoColl->AddVarInfo(VarUMean , &umean [0], REAL_FP_TYPE  , blockInfo_2D_U);

        if (Zmodel>0)
        {
            varInfoColl->AddVarInfo(VarKfuMin, &kfumin[0], IntegerType, blockInfo_2D_U);
            varInfoColl->AddVarInfo(VarKfuMx0, &kfumx0[0], IntegerType, blockInfo_2D_U);
        }

        //
        // 2-dimensional var.s in V-points
        // Depending on Edge type, the V points are the tangential or the normal points
        //
        BlockInfo * blockInfo_2D_V = ( this->edgeType == Edge_Bottom ||
                                       this->edgeType == Edge_Top     ) ? blockInfo2DNormal
                                                                        : blockInfo2DTangential;

        varInfoColl->AddVarInfo(VarKcv   , &kcv[0]   , IntegerType, blockInfo_2D_V);
        varInfoColl->AddVarInfo(VarKfv   , &kfv[0]   , IntegerType, blockInfo_2D_V);
        varInfoColl->AddVarInfo(VarDpv   , &dpv[0]   , REAL_FP_TYPE  , blockInfo_2D_V);
        varInfoColl->AddVarInfo(VarGvv   , &gvv[0]   , REAL_FP_TYPE  , blockInfo_2D_V);
        varInfoColl->AddVarInfo(VarZ0vrou, &z0vrou[0], REAL_FP_TYPE  , blockInfo_2D_V);
        varInfoColl->AddVarInfo(VarVMean , &vmean[0] , REAL_FP_TYPE  , blockInfo_2D_V);

        if (Zmodel>0)
        {
            varInfoColl->AddVarInfo(VarKfvMin, &kfvmin[0], IntegerType, blockInfo_2D_V);
            varInfoColl->AddVarInfo(VarKfvMx0, &kfvmx0[0], IntegerType, blockInfo_2D_V);
        }

        //
        // Block info for 2-dimensional var.s. Info on sizes of communicated strips
        // for normal and tangential velocity points (except for var.s CFUROU and CFVROU)
        //

        BlockInfo * blockInfo3DNormal = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_3d][Points_Normal].Offset,
                        d3dblockInfo[Dim_3d][Points_Normal].Size,
                        d3dblockInfo[Dim_3d][Points_Normal].Amount_D1,
                        d3dblockInfo[Dim_3d][Points_Normal].Stride_D1,
                        d3dblockInfo[Dim_3d][Points_Normal].Amount_D2,
                        d3dblockInfo[Dim_3d][Points_Normal].Stride_D2,
                        d3dblockInfo[Dim_3d][Points_Normal].Amount_D3,
                        d3dblockInfo[Dim_3d][Points_Normal].Stride_D3);

        BlockInfo * blockInfo3DTangential = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_3d][Points_Tangential].Offset,
                        d3dblockInfo[Dim_3d][Points_Tangential].Size,
                        d3dblockInfo[Dim_3d][Points_Tangential].Amount_D1,
                        d3dblockInfo[Dim_3d][Points_Tangential].Stride_D1,
                        d3dblockInfo[Dim_3d][Points_Tangential].Amount_D2,
                        d3dblockInfo[Dim_3d][Points_Tangential].Stride_D2,
                        d3dblockInfo[Dim_3d][Points_Tangential].Amount_D3,
                        d3dblockInfo[Dim_3d][Points_Tangential].Stride_D3);

        //
        // Block info for 3-dimensional var.s CFUROU and CFVROU
        //

        BlockInfo * blockInfo3DNormal_Cf = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_3d][Points_Normal].Offset,
                        d3dblockInfo[Dim_3d][Points_Normal].Size,
                        d3dblockInfo[Dim_3d][Points_Normal].Amount_D1,
                        d3dblockInfo[Dim_3d][Points_Normal].Stride_D1,
                        d3dblockInfo[Dim_3d][Points_Normal].Amount_D2,
                        d3dblockInfo[Dim_3d][Points_Normal].Stride_D2,
                        2,                                              //  2 layers
                        d3dblockInfo[Dim_3d][Points_Normal].Stride_D3);

        BlockInfo * blockInfo3DTangential_Cf = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_3d][Points_Tangential].Offset,
                        d3dblockInfo[Dim_3d][Points_Tangential].Size,
                        d3dblockInfo[Dim_3d][Points_Tangential].Amount_D1,
                        d3dblockInfo[Dim_3d][Points_Tangential].Stride_D1,
                        d3dblockInfo[Dim_3d][Points_Tangential].Amount_D2,
                        d3dblockInfo[Dim_3d][Points_Tangential].Stride_D2,
                        2,                                              //  2 layers
                        d3dblockInfo[Dim_3d][Points_Tangential].Stride_D3);

        //
        // 3-dimensional var.s in U-points
        // Depending on Edge type, the U points are the tangential or the normal points
        //
        BlockInfo * blockInfo_3D_U = ( this->edgeType == Edge_Left ||
                                       this->edgeType == Edge_Right ) ? blockInfo3DNormal
                                                                      : blockInfo3DTangential;

        BlockInfo * blockInfo_3D_U_Cf = ( this->edgeType == Edge_Left ||
                                          this->edgeType == Edge_Right ) ? blockInfo3DNormal_Cf
                                                                         : blockInfo3DTangential_Cf;

        varInfoColl->AddVarInfo(VarQxk   , &qxk[0]   , REAL_FP_TYPE, blockInfo_3D_U   );
        varInfoColl->AddVarInfo(VarU0    , &u0[0]    , REAL_FP_TYPE, blockInfo_3D_U   );
        varInfoColl->AddVarInfo(VarU1    , &u1[0]    , REAL_FP_TYPE, blockInfo_3D_U   );
        varInfoColl->AddVarInfo(VarCfurou, &cfurou[0], REAL_FP_TYPE, blockInfo_3D_U_Cf);

        if (Zmodel>0)
        {
            varInfoColl->AddVarInfo(VarKfuZ0, &kfuz0[0], IntegerType, blockInfo_3D_U);
        }

        //
        // 3-dimensional var.s in V-points
        // Depending on Edge type, the V points are the tangential or the normal points
        //
        BlockInfo * blockInfo_3D_V = ( this->edgeType == Edge_Bottom ||
                                       this->edgeType == Edge_Top  ) ? blockInfo3DNormal
                                                                     : blockInfo3DTangential;

        BlockInfo * blockInfo_3D_V_Cf = ( this->edgeType == Edge_Bottom ||
                                          this->edgeType == Edge_Top     ) ? blockInfo3DNormal_Cf
                                                                           : blockInfo3DTangential_Cf;

        varInfoColl->AddVarInfo(VarQyk   , &qyk[0]   , REAL_FP_TYPE, blockInfo_3D_V);
        varInfoColl->AddVarInfo(VarV0    , &v0[0]    , REAL_FP_TYPE, blockInfo_3D_V);
        varInfoColl->AddVarInfo(VarV1    , &v1[0]    , REAL_FP_TYPE, blockInfo_3D_V);
        varInfoColl->AddVarInfo(VarCfvrou, &cfvrou[0], REAL_FP_TYPE, blockInfo_3D_V_Cf);

        if (Zmodel>0)
        {
            varInfoColl->AddVarInfo(VarKfvZ0, &kfvz0[0], IntegerType, blockInfo_3D_V);
        }

        //
        // 3-dimensional var.s, work arrays.
        // (strip size for work arrays == Strip size for normal velocity points)
        //

        varInfoColl->AddVarInfo(VarWrkb4 , &wrkb4 [0], REAL_FP_TYPE, blockInfo3DNormal);
        varInfoColl->AddVarInfo(VarWrkb17, &wrkb17[0], REAL_FP_TYPE, blockInfo3DNormal);

        //
        // Block info for 3-dimensional inbe*T*ween var.s (1 - kmax+1)
        //  (dicuv / vicuv on zeta points, i.e. use striplenght for normal points)
        //

        BlockInfo * blockInfo3Dt = varInfoColl->AddBlockInfo(
                            d3dblockInfo[Dim_3dt][Points_Normal].Offset,
                            d3dblockInfo[Dim_3dt][Points_Normal].Size,
                            d3dblockInfo[Dim_3dt][Points_Normal].Amount_D1,
                            d3dblockInfo[Dim_3dt][Points_Normal].Stride_D1,
                            d3dblockInfo[Dim_3dt][Points_Normal].Amount_D2,
                            d3dblockInfo[Dim_3dt][Points_Normal].Stride_D2,
                            d3dblockInfo[Dim_3dt][Points_Normal].Amount_D3,
                            d3dblockInfo[Dim_3dt][Points_Normal].Stride_D3);

        varInfoColl->AddVarInfo(VarDicuv, &dicuv[0], REAL_FP_TYPE, blockInfo3Dt);
        varInfoColl->AddVarInfo(VarVicuv, &vicuv[0], REAL_FP_TYPE, blockInfo3Dt);

        //
        // Block info for 3-dimensional 2D * LSedtt Vars
        //

        if (lSedtt>0)
        {
            BlockInfo * blockInfoLSedttNormal = varInfoColl->AddBlockInfo(
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Offset,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Size,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Amount_D1,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Stride_D1,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Amount_D2,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Stride_D2,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Amount_D3,
                            d3dblockInfo[Dim_3dLsedtt][Points_Normal].Stride_D3);

            BlockInfo * blockInfoLSedttTangential = varInfoColl->AddBlockInfo(
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Offset,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Size,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Amount_D1,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Stride_D1,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Amount_D2,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Stride_D2,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Amount_D3,
                            d3dblockInfo[Dim_3dLsedtt][Points_Tangential].Stride_D3);

            if ( this->edgeType == Edge_Left || this->edgeType == Edge_Right )
            {
                varInfoColl->AddVarInfo(VarSbuu, &sbuu[0], REAL_FP_TYPE, blockInfoLSedttNormal);
                varInfoColl->AddVarInfo(VarSbvv, &sbvv[0], REAL_FP_TYPE, blockInfoLSedttTangential);
            }
            else
            {
                varInfoColl->AddVarInfo(VarSbuu, &sbuu[0], REAL_FP_TYPE, blockInfoLSedttTangential);
                varInfoColl->AddVarInfo(VarSbvv, &sbvv[0], REAL_FP_TYPE, blockInfoLSedttNormal);
            }

        }

        //
        // Block info for 4-dimensional var.s
        //

        BlockInfo * blockInfo4D = varInfoColl->AddBlockInfo(
                        d3dblockInfo[Dim_4d][Points_Normal].Offset,
                        d3dblockInfo[Dim_4d][Points_Normal].Size,
                        d3dblockInfo[Dim_4d][Points_Normal].Amount_D1,
                        d3dblockInfo[Dim_4d][Points_Normal].Stride_D1,
                        d3dblockInfo[Dim_4d][Points_Normal].Amount_D2,
                        d3dblockInfo[Dim_4d][Points_Normal].Stride_D2,
                        d3dblockInfo[Dim_4d][Points_Normal].Amount_D3,
                        d3dblockInfo[Dim_4d][Points_Normal].Stride_D3,
                        d3dblockInfo[Dim_4d][Points_Normal].Amount_D4,
                        d3dblockInfo[Dim_4d][Points_Normal].Stride_D4);

        //
        // 4-dimensional var.s in zeta points
        //

        varInfoColl->AddVarInfo(VarR0, &r0[0], REAL_FP_TYPE, blockInfo4D);
        varInfoColl->AddVarInfo(VarR1, &r1[0], REAL_FP_TYPE, blockInfo4D);

        //
        // 4-dimensional var.s, work arrays.
        //

        varInfoColl->AddVarInfo(VarWrkc4, &wrkc4[0], REAL_FP_TYPE, blockInfo4D);


        //
        // Create groups to communicated (one empty group):
        //

        varInfoColl->AddGroup(MapDistrib_NoGroup    );



        //
        // Vars to be communicated initially:
        //
        varInfoColl->AddGroup(MapDistrib_Initial    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarThick  );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKcs    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKcu    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKcv    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarGuu    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarGvv    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarDps    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarGsqs   );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarCfurou );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarCfvrou );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarZ0urou );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarZ0vrou );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarU0     );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarU1     );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarV0     );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarV1     );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarS0     );
        varInfoColl->AddVarToGroup(MapDistrib_Initial, VarVicuv  );
        if ( lStsci > 0 )
        {
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarR0     );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarR1     );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarDicuv  );
        }
        if ( Zmodel > 0 )
        {
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKfuMin );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKfuMx0 );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKfvMin );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKfvMx0 );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKfuZ0  );
            varInfoColl->AddVarToGroup(MapDistrib_Initial, VarKfvZ0  );
        }



        //
        // Vars to be communicated initially (part 2):
        //
        varInfoColl->AddGroup(MapDistrib_Initial2   );
        varInfoColl->AddVarToGroup(MapDistrib_Initial2, VarDpu    );
        varInfoColl->AddVarToGroup(MapDistrib_Initial2, VarDpv    );


        //
        // Vars to be communicated when building U or V
        //
        varInfoColl->AddGroup(MapDistrib_Build_UV   );
        varInfoColl->AddVarToGroup(MapDistrib_Build_UV, VarWrkb4  );
        varInfoColl->AddVarToGroup(MapDistrib_Build_UV, VarU1     );
        varInfoColl->AddVarToGroup(MapDistrib_Build_UV, VarV1     );



        //
        // Vars to be communicated when checking drying/flooding
        //
        varInfoColl->AddGroup(MapDistrib_Dry   );
        varInfoColl->AddVarToGroup(MapDistrib_Dry, VarU1     );
        varInfoColl->AddVarToGroup(MapDistrib_Dry, VarV1     );
        varInfoColl->AddVarToGroup(MapDistrib_Dry, VarUMean  );
        varInfoColl->AddVarToGroup(MapDistrib_Dry, VarVMean  );


        //
        // Vars to be communicated when solving U or V
        //
        varInfoColl->AddGroup(MapDistrib_Solve_UV   );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarU1     );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarV1     );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfu    );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfv    );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarUMean  );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarVMean  );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarWrkb4  );
        if ( Zmodel > 0 )
        {
            varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfuMin );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfuMx0 );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfvMin );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfvMx0 );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfuZ0  );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_UV, VarKfvZ0  );
        }



        //
        // Vars to be communicated when building transport
        //
        varInfoColl->AddGroup(MapDistrib_Build_Conc );
        varInfoColl->AddVarToGroup(MapDistrib_Build_Conc, VarWrkc4  );
        varInfoColl->AddVarToGroup(MapDistrib_Build_Conc, VarR1     );



        //
        // Vars to be communicated when solving transport / morphology
        //
        varInfoColl->AddGroup(MapDistrib_Solve_Conc );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_Conc, VarVicuv  );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_Conc, VarS1     );
        if ( lStsci > 0 )
        {
            varInfoColl->AddVarToGroup(MapDistrib_Solve_Conc, VarDicuv  );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_Conc, VarR1     );
            varInfoColl->AddVarToGroup(MapDistrib_Solve_Conc, VarWrkc4  );
        }


        //
        // Vars to be communicated solving Wang System
        //
        varInfoColl->AddGroup(MapDistrib_Finish_Wang);
        varInfoColl->AddVarToGroup(MapDistrib_Finish_Wang, VarS1     );
        varInfoColl->AddVarToGroup(MapDistrib_Finish_Wang, VarQxk    );
        varInfoColl->AddVarToGroup(MapDistrib_Finish_Wang, VarQyk    );
        varInfoColl->AddVarToGroup(MapDistrib_Finish_Wang, VarKfu    );
        varInfoColl->AddVarToGroup(MapDistrib_Finish_Wang, VarKfv    );


        //
        // Vars to be communicated solving sediment / morphology
        //
        if ( lSedtt > 0 )
        {
            varInfoColl->AddGroup(MapDistrib_Sediment   );
            varInfoColl->AddVarToGroup(MapDistrib_Sediment, VarSbuu  );
            varInfoColl->AddVarToGroup(MapDistrib_Sediment, VarSbvv  );
            varInfoColl->AddVarToGroup(MapDistrib_Sediment, VarDps   );
        }


        //
        // Vars to be communicated during construction of roller velocities
        //
        varInfoColl->AddGroup(MapDistrib_Roller_UV );
        if (Roller>0)
        {
            varInfoColl->AddVarInfo(VarQxkr, &qxkr[0], REAL_FP_TYPE, blockInfo2DNormal);
            varInfoColl->AddVarInfo(VarQykr, &qykr[0], REAL_FP_TYPE, blockInfo2DNormal);
            varInfoColl->AddVarInfo(VarQxkw, &qxkw[0], REAL_FP_TYPE, blockInfo2DNormal);
            varInfoColl->AddVarInfo(VarQykw, &qykw[0], REAL_FP_TYPE, blockInfo2DNormal);
            varInfoColl->AddVarToGroup(MapDistrib_Roller_UV, VarQxkr );
            varInfoColl->AddVarToGroup(MapDistrib_Roller_UV, VarQykr );
            varInfoColl->AddVarToGroup(MapDistrib_Roller_UV, VarQxkw );
            varInfoColl->AddVarToGroup(MapDistrib_Roller_UV, VarQykw );
        }

        //
        // Vars to be communicated during 2D Advection-Diffusion
        //
        varInfoColl->AddGroup(MapDistrib_Build_2DAD );
        varInfoColl->AddVarToGroup(MapDistrib_Build_2DAD, VarWrkb17 );


        //
        // Vars to be communicated during 2D Advection-Diffusion
        //
        varInfoColl->AddGroup(MapDistrib_Solve_2DAD );
        varInfoColl->AddVarToGroup(MapDistrib_Solve_2DAD, VarWrkb17 );



#if 0
        varInfoColl->AddGroup(MapDistrib_All); // TODO: exclude from get-max-num-bytes
        int vId;
        for ( vId = 0 ; vId < NUM_VAR_IDS ; vId++ )
        {
            switch( vId )
            {

                case VarSbuu:
                case VarSbvv:
                case VarDepchg:
                    if ( lSedtt > 0 )
                    {
                        varInfoColl->AddVarToGroup(MapDistrib_All, vId);
                    }
                    break;

                case VarQxkr:
                case VarQykr:
                case VarQxkw:
                case VarQykw:
                    if ( Roller > 0 )
                    {
                        varInfoColl->AddVarToGroup(MapDistrib_All, vId);
                    }
                    break;

                case VarKfuMin:
                case VarKfuMx0:
                case VarKfvMin:
                case VarKfvMx0:
                case VarKfuZ0:
                case VarKfvZ0:
                    if ( Zmodel > 0 )
                    {
                        varInfoColl->AddVarToGroup(MapDistrib_All, vId);
                    }
                    break;

                default:
                    varInfoColl->AddVarToGroup(MapDistrib_All, vId);
                    break;

            }
        }
#endif

    }

    else

    {
        printf("VAR_INFO COLL = NULL\n"); // TODO: prived return value
    }
}
