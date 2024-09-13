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
// $Id: mapper_statemachine.cpp 878 2011-10-07 12:58:46Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/mapper_statemachine.cpp $
//------------------------------------------------------------------------------
//  Class: D3dFlowMapper
//  Mapper for DELFT3D-FLOW Domain Decomposition
//
//  Stef.Hummel@deltares.nl
//  Erik.deGoede@deltares.nl
//  Bert.Jagers@deltares.nl
//  31 may 11
//-------------------------------------------------------------------------------


#include "flow2d3d.h"


#define LOG_STEPS   0
#define LOG_WANG_STEPS  0
#define LOG_INIT    0


//////////////////////////////////////////////////////////////////////
//
// D3dFlowMapper
//
// Mapper for DELFT3D-FLOW Domain Decomposition
//


int D3dFlowMapper::DoMapping(void)
{
    int retVal = HY_OK;     // Return value
    int transportUsed;      // boolean
    int sedimentUsed;       // boolean

    MAPDBG_FUN("D3dFlowMapper::DoMapping");

    if ( retVal != HY_ERR )
    {
        //
        // Labelling after adjust, so this means that in case of
        // alternation, labels are not entirely correct. Since labels
        // are only used in Delft3D-FLOW for checks this seems to work fine
        //
        InitKc(Eq_U);
        InitKc(Eq_V);
        CheckDDbounds(Eq_Zeta);
        InitKc(Eq_Zeta);

        InitFlowVariables();

        transportUsed  = (C[C_0]->lStsci > 0 && C[C_1]->lStsci > 0 );
        sedimentUsed   = (C[C_0]->lSedtt > 0 && C[C_1]->lSedtt > 0 );

        //
        // Call time loop statemachine
        //

        StateMachineAdiOrWang(transportUsed, sedimentUsed);
    }

    return retVal;
}


void D3dFlowMapper::StateMachineAdiOrWang(
    int transportUsed,      // boolean calculate transport?
    int sedimentUsed        // boolean Sediment or Not
    )
{
    int     thisMapperStep; // the step to be performed by the mapper
    int     nextFlowStep;   // next step to be performed by FLOW
    int     nMapIter;       // Max # 'resolve' iterations
    int     tStep;          // time step counter
    int     lSed;           // constituent
    int     Stage;          // Stage = 1 or 2
    int     notConverged;   // result of Proces

    MAPDBG_FUN("D3dFlowMapper::StateMachineAdiOrWang");

    // TRICOM: Init
    SendDataToFlow(D3dFlowMap_Init,STEP_UNDEF);

    tStep = 0;
    while (1)
    {
        thisMapperStep = ReceiveDataFromFlow();
        nextFlowStep = STEP_UNDEF;
        //printf("Mapper: %s (%d)\n",PrintNextStepName(thisMapperStep),thisMapperStep);

        switch (thisMapperStep)
        {
            case D3dFlowMap_InitTimeStep:
                // TRISOL: InitTimeStep
                if (tStep == 0)
                {
                    CheckDepthUV(Eq_U);
                    CheckDepthUV(Eq_V);
                }
                tStep++;
                break;
            case D3dFlowMap_Finish:
                // TRISIM: Finish
                break;
            case D3dFlowMap_Build_V:
                // UZD: Build_V
                Stage = 1;
                Build(Seq_V,0);
                nMapIter = 0;
                lSed = 0;
                break;
            case D3dFlowMap_Build_U:
                // UZD: Build_U
                Stage = 2;
                Build(Seq_U,0);
                nMapIter = 0;
                lSed = 0;
                break;
            case D3dFlowMap_Check_V:
                // UZD: Check_V (Solve_V)
                nMapIter++;
                if ( Proces(Seq_V,0) && ( nMapIter != MaxIter_Vel ) )
                    nextFlowStep = D3dFlow_Solve_V;
                else
                    CopyFlowVariables(Seq_V);
                break;
            case D3dFlowMap_Check_U:
                // UZD: Check_U (Solve_U)
                nMapIter++;
                if ( Proces(Seq_U,0) && ( nMapIter != MaxIter_Vel ) )
                    nextFlowStep = D3dFlow_Solve_U;
                else
                    CopyFlowVariables(Seq_U);
                break;
            case D3dFlowMap_Build_ADI_Zeta:
                // SUD: Build_ADI_Zeta
                break;
            case D3dFlowMap_Check_ADI_Zeta:
                // SUD: Check_ADI_Zeta (Solve_ADI_Zeta)
                break;
            case D3dFlowMap_Check_SUD_Dry:
                // SUD: Check_SUD_Dry
                if ( CheckForDry() )
                    nextFlowStep = D3dFlow_Build_ADI_Zeta;
                break;
            case D3dFlowMap_Finish_Wang:
                // SUD: Finish_Wang
                CopyFlowVariables(Seq_Zeta);
                FinishWang();
                break;
            case D3dFlowMap_Check_ADI_Dry:
                // ADI: Check_ADI_Dry
                if ( CheckForDry() )
                    nextFlowStep = D3dFlow_Build_ADI_Zeta;
                else if (Stage == 1)
                {
                    CopyOuterNormalVelocities(Seq_U);
                    CopyOuterTangentVelocities(Seq_V);
                }
                else
                {
                    CopyOuterNormalVelocities(Seq_V);
                    CopyOuterTangentVelocities(Seq_U);
                }
                break;
            case D3dFlowMap_Sediment:
                // EROSED: Sediment
                CopySediment(Seq_U);
                CopySediment(Seq_V);
                break;
            case D3dFlowMap_Build_ADI_Conc:
                // DIFU: Build_ADI_Conc
                lSed++;
                if (Stage == 1)
                    Build(Seq_Conc_ADI_St1,lSed);
                else
                    Build(Seq_Conc_ADI_St2,lSed);
                nMapIter = 0;
                break;
            case D3dFlowMap_Check_ADI_Conc:
                // DIFU: Check_ADI_Conc (Solve_ADI_Conc)
                nMapIter++;
                LOG_ITER(nMapIter);
                if (Stage == 1)
                    notConverged = Proces(Seq_Conc_ADI_St1,lSed);
                else
                    notConverged = Proces(Seq_Conc_ADI_St2,lSed);
                if ( notConverged && ( nMapIter != MaxIter_Conc ) )
                    nextFlowStep = D3dFlow_Solve_ADI_Conc;
                else if (lSed == C[C_0]->lStsci)
                    CopyFlowVariables(Seq_Conc);
                break;
            case D3dFlowMap_Roller_UV:
                // QKWCG: Roller_UV
                Build(Seq_RolUV,0);
                break;
            case D3dFlowMap_Build_2DAD:
                // DIFUWE: Build_2DAD
                Build(Seq_2DAD,0);
                nMapIter = 0;
                break;
            case D3dFlowMap_Check_2DAD:
                // DIFUWE: Check_2DAD (Solve_2DAD)
                nMapIter++;
                notConverged = Proces(Seq_2DAD,0);
                if ( notConverged && ( nMapIter != MaxIter_2DAD ) )
                    nextFlowStep = D3dFlow_Solve_2DAD;
                break;
            case D3dFlowMap_Bottom3D:
                // BOTT3D: Bottom3D
                CopySediment(Seq_Zeta);
                break;
            default:
                throw new Exception (true, "D3dFlowMapper::StateMachineAdiOrWang: unexpected map request: %s (%d)",
                            PrintNextStepName(nextFlowStep),nextFlowStep);
        }
        if (thisMapperStep == D3dFlowMap_Finish)
            break;
        else
            SendDataToFlow(thisMapperStep,nextFlowStep);
    }
}
