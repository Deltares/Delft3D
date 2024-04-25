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
// $Id: flow_steps_c.h 883 2011-10-07 16:32:16Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/flow_steps_c.h $
//------------------------------------------------------------------------------
//  Enumeration of states for DELFT3D-FLOW and its D3dFlow-Mapper
//
//  Stef.Hummel@Deltares.NL
//  30 may 11
//-------------------------------------------------------------------------------


#pragma once




/*
 *     Possible states for DELFT3D-FLOW and its D3dFlow-Mapper
 *     Remark: '..._InitTimeStep' includes 'build U equations'.
 *
 *     DELFT3D-FLOW states  D3dFlow-Mapper states
 * -------------------  --------------------- */

#define    D3dFlow_Init                         0
#define                 D3dFlowMap_Init         1

#define    D3dFlow_InitTimeStep                     2
#define                 D3dFlowMap_InitTimeStep     3

#define    D3dFlow_Build_U                      4
#define                 D3dFlowMap_Build_U      5
#define    D3dFlow_Solve_U                      6
#define                 D3dFlowMap_Check_U      7

#define    D3dFlow_Build_V                      8
#define                 D3dFlowMap_Build_V      9
#define    D3dFlow_Solve_V                      10
#define                 D3dFlowMap_Check_V      11

#define    D3dFlow_Finish                       98
#define                 D3dFlowMap_Finish       99

/*
 *  Some steps for the ADI state machine
 */

#define    D3dFlow_Build_ADI_Zeta                   60
#define                 D3dFlowMap_Build_ADI_Zeta   61

#define    D3dFlow_Solve_ADI_Zeta                   64
#define                 D3dFlowMap_Check_ADI_Zeta   65

#define    D3dFlow_Build_ADI_Conc                   68
#define                 D3dFlowMap_Build_ADI_Conc   69

#define    D3dFlow_Solve_ADI_Conc                   72
#define                 D3dFlowMap_Check_ADI_Conc   73

#define    D3dFlow_Check_SUD_Dry                    76
#define                 D3dFlowMap_Check_SUD_Dry    77

#define    D3dFlow_Check_ADI_Dry                    85
#define                 D3dFlowMap_Check_ADI_Dry    86

/*
 *  Steps for the ADI-Wang state machine
 */

#define    D3dFlow_Solve_Wang                       78
#define    D3dFlow_Finish_Wang                      80
#define                 D3dFlowMap_Finish_Wang      79


/*
 *  Steps for Sediment transport
 */

#define    D3dFlow_Sediment                         81
#define                 D3dFlowMap_Sediment         82

#define    D3dFlow_Bottom3D                         83
#define                 D3dFlowMap_Bottom3D         84

/*
 *  Steps for 2D Advection-Diffusion solver (Roller)
 */

#define    D3dFlow_Build_2DAD                       87
#define                 D3dFlowMap_Build_2DAD       88

#define    D3dFlow_Solve_2DAD                       89
#define                 D3dFlowMap_Check_2DAD       90

/*
 *  Steps for roller velocities (Roller)
 */

#define    D3dFlow_Roller_UV                        91
#define                 D3dFlowMap_Roller_UV        92

/*
 * number of timesteps (used for array declaration)
 */

#define                 D3DFLOW_NR_STEPS        100

#define    NONEIGHBORS                          777
