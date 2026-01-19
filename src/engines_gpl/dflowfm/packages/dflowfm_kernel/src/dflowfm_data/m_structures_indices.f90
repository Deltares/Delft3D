!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!> holds indices for all structure types
module m_structures_indices

   implicit none

! COMMON indices for all structure types:
   integer, parameter :: NUMVALS_COMMON = 11 !< Number of common variables for all structure types except for pump and gate (new)
   integer, parameter :: IVAL_WIDTH = 1 !< Index of total width
   integer, parameter :: IVAL_WIDTHWET = 2 !< Index of total width of wet links
   integer, parameter :: IVAL_WIDTHUP = 3 !< Index of wet flow link width on upstream side
   integer, parameter :: IVAL_WIDTHDN = 4 !< Index of wet flow link width on downstream side
   integer, parameter :: IVAL_WIDTHUPDN = 5 !< Index of width of wet flow links that have both upstream and downstream nodes wet
   integer, parameter :: IVAL_DIS = 6 !< Index of discharge
   integer, parameter :: IVAL_S1UP = 7 !< Index of water level at upstream
   integer, parameter :: IVAL_S1DN = 8 !< Index of water level at downstream
   integer, parameter :: IVAL_HEAD = 9 !< Index of head of the structure
   integer, parameter :: IVAL_AREA = 10 !< Index of flow area of the structure
   integer, parameter :: IVAL_VEL = 11 !< Index of flow velocity

 !! For general structure, weir, orifice, because they share some common output variables
   ! Followings are extra variables for general structure, weir and orifice:
   integer, parameter :: IVAL_S1ONCREST = NUMVALS_COMMON + 1 !< Index of water level on crest
   integer, parameter :: IVAL_CRESTL = NUMVALS_COMMON + 2 !< Index of crest level
   integer, parameter :: IVAL_CRESTW = NUMVALS_COMMON + 3 !< Index of crest width
   integer, parameter :: IVAL_STATE = NUMVALS_COMMON + 4 !< Index of state (0: closed, 1: free weir, 2: drowned/submerged weir)
   integer, parameter :: IVAL_FORCEDIF = NUMVALS_COMMON + 5 !< Index of force difference per unit width
   ! Followings are extra variables for general structure and orifice:
   integer, parameter :: IVAL_OPENW = NUMVALS_COMMON + 6 !< Index of gate opening width
   integer, parameter :: IVAL_EDGEL = NUMVALS_COMMON + 7 !< Index of gate lower edge level
   integer, parameter :: IVAL_OPENH = NUMVALS_COMMON + 8 !< Index of gate opening height
   ! Followings are extra variables only for general structure:
   integer, parameter :: IVAL_UPPL = NUMVALS_COMMON + 9 !< Index of gate upper edge level
   integer, parameter :: IVAL_DIS_OPEN = NUMVALS_COMMON + 10 !< Index of discharge through gate opening
   integer, parameter :: IVAL_DIS_OVER = NUMVALS_COMMON + 11 !< Index of discharge over gate
   integer, parameter :: IVAL_DIS_UNDER = NUMVALS_COMMON + 12 !< Index of discharge under gate
   integer, parameter :: IVAL_AREA_OPEN = NUMVALS_COMMON + 13 !< Index of flow area through gate opening
   integer, parameter :: IVAL_AREA_OVER = NUMVALS_COMMON + 14 !< Index of flow area over gate
   integer, parameter :: IVAL_AREA_UNDER = NUMVALS_COMMON + 15 !< Index of flow area under gate
   integer, parameter :: IVAL_VEL_OPEN = NUMVALS_COMMON + 16 !< Index of velocity through gate opening
   integer, parameter :: IVAL_VEL_OVER = NUMVALS_COMMON + 17 !< Index of velocity over gate
   integer, parameter :: IVAL_VEL_UNDER = NUMVALS_COMMON + 18 !< Index of velocity under gate
   integer, parameter :: IVAL_COUNT = NUMVALS_COMMON + 19 !< Index of counters of partitions for parallel

   integer, parameter :: NUMEXTVALS_GENSTRU = 19 ! Number of extra variables for general structure, including last one as a counter
   integer, parameter :: NUMEXTVALS_WEIRGEN = 6 ! Number of extra variables for weir, including last one as a counter
   integer, parameter :: NUMEXTVALS_ORIFGEN = 9 ! Number of extra variables for orifice, including last one as a counter
   integer, parameter :: NUMVALS_GENSTRU = NUMVALS_COMMON + NUMEXTVALS_GENSTRU !< Total number of variables for general structure (new exe file)
   integer, parameter :: NUMVALS_WEIRGEN = NUMVALS_COMMON + NUMEXTVALS_WEIRGEN !< Total number of variables for weir
   integer, parameter :: NUMVALS_ORIFGEN = NUMVALS_COMMON + NUMEXTVALS_ORIFGEN !< Total number of variables for orifice

   ! Bridge, extra variables:
   integer, parameter :: IVAL_BLUP = NUMVALS_COMMON + 1 !< Index of bed level up
   integer, parameter :: IVAL_BLDN = NUMVALS_COMMON + 2 !< Index of bed level down
   integer, parameter :: IVAL_BLACTUAL = NUMVALS_COMMON + 3 !< Index of actual bed level (crest)
   integer, parameter :: NUMEXTVALS_BRIDGE = 3 !< Number of extra variables for bridge
   integer, parameter :: NUMVALS_BRIDGE = NUMVALS_COMMON + NUMEXTVALS_BRIDGE !< Total number of variables for bridge

   ! Dambreak, extra variables:
   integer, parameter :: IVAL_DB_CRESTH = NUMVALS_COMMON + 1 !< Index of crest level for dambreak
   integer, parameter :: IVAL_DB_CRESTW = NUMVALS_COMMON + 2 !< Index of crest width for dambreak
   integer, parameter :: IVAL_DB_JUMP = NUMVALS_COMMON + 3 !< Index of water level jump for dambreak
   integer, parameter :: IVAL_DB_TIMEDIV = NUMVALS_COMMON + 4 !< Index of breach width time derivative for dambreak
   integer, parameter :: IVAL_DB_DISCUM = NUMVALS_COMMON + 5 !< Index of cumulative discharge for dambreak
   integer, parameter :: NUMEXTVALS_DAMBREAK = 5 !< Number of extra variables for dambreak
   integer, parameter :: NUMVALS_DAMBREAK = NUMVALS_COMMON + NUMEXTVALS_DAMBREAK !< Total number of variables for dambreak

   ! Culvert, extra variables:
   integer, parameter :: IVAL_CL_CRESTL = NUMVALS_COMMON + 1 !< Index of culvert crest level
   integer, parameter :: IVAL_CL_STATE = NUMVALS_COMMON + 2 !< Index of culvert state (0: closed, 1: free weir, 2: drowned/submerged weir)
   integer, parameter :: IVAL_CL_EDGEL = NUMVALS_COMMON + 3 !< Index of culvert gate lower edge level
   integer, parameter :: IVAL_CL_OPENH = NUMVALS_COMMON + 4 !< Index of culvert gate opening height
   integer, parameter :: NUMEXTVALS_CULVERT = 4 !< Number of extra variables for culvertt
   integer, parameter :: NUMVALS_CULVERT = NUMVALS_COMMON + NUMEXTVALS_CULVERT !< Total number of variables for culvert

   ! Univeral weir, extra variables:
   integer, parameter :: IVAL_UW_CRESTL = NUMVALS_COMMON + 1 !< Index of universal weir crest level
   integer, parameter :: NUMEXTVALS_UNIWEIR = 1 !< Number of extra variables for universal weir
   integer, parameter :: NUMVALS_UNIWEIR = NUMVALS_COMMON + NUMEXTVALS_UNIWEIR !< Total number of variables for universal weir

   ! gate (new),  extra variables:
   integer, parameter :: NUMVALS_COMMON_GATE = 8 !< Number of common variables shared by gate
   integer, parameter :: IVAL_GATE_FLOWH = NUMVALS_COMMON_GATE + 1 !< Upstream average water level
   integer, parameter :: IVAL_GATE_COUNT = NUMVALS_COMMON_GATE + 2 !< Counter
   integer, parameter :: IVAL_GATE_OPENW = NUMVALS_COMMON_GATE + 3 !< Gate opening width
   integer, parameter :: IVAL_GATE_EDGEL = NUMVALS_COMMON_GATE + 4 !< Gate lower edge level
   integer, parameter :: IVAL_GATE_SILLH = NUMVALS_COMMON_GATE + 5 !< Gate crest level (via general structure)
   integer, parameter :: IVAL_GATE_WIDTHWET = NUMVALS_COMMON_GATE + 6 !< Width of wet links at upstream (used for IVAL_GATE_FLOWH)
   integer, parameter :: NUMEXTVALS_GATE = 6 !< Number of extra variables for gate
   integer, parameter :: NUMVALS_GATEGEN = NUMVALS_COMMON_GATE + NUMEXTVALS_GATE !< Total number of variables for gate

   ! Compound structure
   integer, parameter :: NUMVALS_CMPSTRU = NUMVALS_COMMON !< Total number of variables for compound structure, no extra variables.

   ! Pump shares the first 9 indices in common indices, extra variables are as follows:
   integer, parameter :: NUMVALS_COMMON_PUMP = 9 !< Number of common variables shared by pump
   integer, parameter :: IVAL_PP_CAP = NUMVALS_COMMON_PUMP + 1 !< Pump capacity
   integer, parameter :: IVAL_PP_STAG = NUMVALS_COMMON_PUMP + 2 !< Actual pump stage
   integer, parameter :: IVAL_PP_HEAD = NUMVALS_COMMON_PUMP + 3 !< Pump head
   integer, parameter :: IVAL_PP_RED = NUMVALS_COMMON_PUMP + 4 !< Pump reduction factor
   integer, parameter :: IVAL_PP_S1DEL = NUMVALS_COMMON_PUMP + 5 !< Pump water level at delivery side
   integer, parameter :: IVAL_PP_S1SUC = NUMVALS_COMMON_PUMP + 6 !< Pump water level at suction side
   integer, parameter :: IVAL_PP_DISDIR = NUMVALS_COMMON_PUMP + 7 !< Pump discharge w.r.t. pumping orientation (same sign as capacity)
   integer, parameter :: NUMEXTVALS_PUMP = 7 !< Number of extra variables for pump
   integer, parameter :: NUMVALS_PUMP = NUMVALS_COMMON_PUMP + NUMEXTVALS_PUMP !< Total number of variables for pump

   ! Long culvert
   integer, parameter :: IVAL_LC_VALVE = NUMVALS_COMMON + 1 !< long culvert valve relative opening
   integer, parameter :: NUMEXTVALS_LONGCULV = 1 !< Number of extra variables for long culvert
   integer, parameter :: NUMVALS_LONGCULVERT = NUMVALS_COMMON + NUMEXTVALS_LONGCULV !< Number of variables for long culvert

   ! For old stype structures
   integer :: NUMVALS_GATE = 5 !< Number of variables for gate
   integer :: NUMVALS_CDAM = 4 !< Number of variables for controble dam
   integer :: NUMVALS_CGEN = 4 !< Number of variables for general structure (old ext file)

end module m_structures_indices
