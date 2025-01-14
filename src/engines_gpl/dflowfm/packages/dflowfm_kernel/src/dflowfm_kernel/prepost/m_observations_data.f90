!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

!> This module holds data used for observation stations that are used to monitor flow data at fixed and
!! moving points in the domain.
!! In arrays: (1:numobs = normal or fixed observation stations, numobs+1:numobs+nummovobs = moving observation stations)
module m_observations_data
   use messagehandling, only: Idlen
   use precision, only: dp

   implicit none

   integer :: numobs = 0 !< nr of observation stations
   integer :: nummovobs = 0 !< nr of *moving* observation stations
   real(kind=dp), allocatable :: xobs(:) !< x-coord of observation points (1:numobs = normal obs from *.xyn and *.ini files, numobs+1:numobs+nummovobs = moving obs)
   real(kind=dp), allocatable :: yobs(:) !< y-coord of observation points
   real(kind=dp), allocatable, target :: xyobs(:) !< xy-coord of *moving* observation points (work array for meteo)
   real(kind=dp), allocatable :: smxobs(:) !< maximum waterlevel of observation points
   real(kind=dp), allocatable :: cmxobs(:) !< maximum 2D flow velocity of observation points, 3D: maximum over all layers and time
   integer, allocatable :: kobs(:) !< node nrs of ACTIVE observation points
   integer, allocatable :: lobs(:) !< flowlink nrs of active observation points
   ! NOTE: kobs is not maintained here (so also not after deleteObservation, etc.) All done once by obs_on_flowgrid.
   character(len=IdLen), allocatable :: namobs(:) ! names of observation points
   integer, allocatable :: locTpObs(:) !< location type of observation points, determining to which flownodes to snap to (0=1d2d, 1=1d, 2=2d, 3=1d defined by branchID+chainage)
   integer, allocatable :: obs2OP(:) !< mapping from global m_observation::obs index to m_network::network%obs index (i.e., the ones defined via a *.ini file)

   integer :: mxls !< Unit nr hisdump to excel
   integer :: jafahrenheit = 0 !< Output in Celsius, otherwise Fahrenheit

   real(kind=dp) :: valobs_last_update_time !< Time at which the valobs array was last updated.
   real(kind=dp), dimension(:, :), allocatable, target :: valobs !< work array with 2d and 3d values stored at observation stations, dim(numobs+nummovobs, MAXNUMVALOBS2D+MAXNUMVALOBS3D*max(kmx,1)+MAXNUMVALOBS3Dw*(max(kmx,1)+1))

   integer :: MAXNUMVALOBS2D ! maximum number of outputted values at observation stations
   integer :: MAXNUMVALOBS3D ! maximum number of outputted values at observation stations, 3D layer centers
   integer :: MAXNUMVALOBS3Dw ! maximum number of outputted values at observation stations, 3D layer interfaces (e.g. zws)
   integer :: MAXNUMVALOBSLYR ! maximum number of outputted values at observation stations, bed sediment layers (e.g. msed)
   integer :: IVAL_S1 ! 2D first
   integer :: IVAL_HS
   integer :: IVAL_BL
   integer :: IVAL_SMX
   integer :: IVAL_CMX
   integer :: IVAL_WX
   integer :: IVAL_WY
   integer :: IVAL_PATM
   integer :: IVAL_RAIN
   integer :: IVAL_INFILTCAP
   integer :: IVAL_INFILTACT
   integer :: IVAL_WAVEH
   integer :: IVAL_WAVET
   integer :: IVAL_WAVED
   integer :: IVAL_WAVEL
   integer :: IVAL_WAVER
   integer :: IVAL_WAVEU
   integer :: IVAL_TAUX
   integer :: IVAL_TAUY
   integer :: IVAL_UCX ! 3D, layer centered after 2D
   integer :: IVAL_UCY
   integer :: IVAL_UCZ
   integer :: IVAL_UCXQ
   integer :: IVAL_UCYQ
   integer :: IVAL_UCXST
   integer :: IVAL_UCYST
   integer :: IVAL_SA1
   integer :: IVAL_TEM1
   integer :: IVAL_UMAG
   integer :: IVAL_QMAG
   integer :: IVAL_TRA1
   integer :: IVAL_TRAN
   integer :: IVAL_HWQ1
   integer :: IVAL_HWQN
   integer :: IVAL_WQB1
   integer :: IVAL_WQBN
   integer :: IVAL_WQB3D1
   integer :: IVAL_WQB3DN
   integer :: IVAL_SED ! HK code
   integer :: IVAL_SF1 ! stm code
   integer :: IVAL_SFN
   integer :: IVAL_ZCS
   integer :: IVAL_ZWS ! 3D, layer interfaces after layer centered
   integer :: IVAL_ZWU
   integer :: IVAL_BRUV
   integer :: IVAL_TKIN
   integer :: IVAL_TEPS
   integer :: IVAL_VIU
   integer :: IVAL_VICWWS
   integer :: IVAL_VICWWU
   integer :: IVAL_WS1
   integer :: IVAL_WSN
   integer :: IVAL_SEDDIF1
   integer :: IVAL_SEDDIFN
   integer :: IVAL_RICH
   integer :: IVAL_TAIR
   integer :: IVAL_WIND
   integer :: IVAL_RHUM
   integer :: IVAL_CLOU
   integer :: IVAL_AIRDENSITY
   integer :: IVAL_QSUN
   integer :: IVAL_QEVA
   integer :: IVAL_QCON
   integer :: IVAL_QLON
   integer :: IVAL_QFRE
   integer :: IVAL_QFRC
   integer :: IVAL_QTOT
   integer :: IVAL_RHOP
   integer :: IVAL_RHO
   integer :: IVAL_SBCX1
   integer :: IVAL_SBCXN
   integer :: IVAL_SBCY1
   integer :: IVAL_SBCYN
   integer :: IVAL_SBWX1
   integer :: IVAL_SBWXN
   integer :: IVAL_SBWY1
   integer :: IVAL_SBWYN
   integer :: IVAL_SSCX1
   integer :: IVAL_SSCXN
   integer :: IVAL_SSCY1
   integer :: IVAL_SSCYN
   integer :: IVAL_SSWX1
   integer :: IVAL_SSWXN
   integer :: IVAL_SSWY1
   integer :: IVAL_SSWYN
   integer :: IVAL_SOUR1
   integer :: IVAL_SOURN
   integer :: IVAL_SINK1
   integer :: IVAL_SINKN
   integer :: IVAL_BODSED1
   integer :: IVAL_BODSEDN
   integer :: IVAL_TAUB
   integer :: IVAL_DPSED
   integer :: IVAL_MSED1
   integer :: IVAL_MSEDN
   integer :: IVAL_THLYR
   integer :: IVAL_POROS
   integer :: IVAL_LYRFRAC1
   integer :: IVAL_LYRFRACN
   integer :: IVAL_FRAC1
   integer :: IVAL_FRACN
   integer :: IVAL_MUDFRAC
   integer :: IVAL_SANDFRAC
   integer :: IVAL_FIXFAC1
   integer :: IVAL_FIXFACN
   integer :: IVAL_HIDEXP1
   integer :: IVAL_HIDEXPN
   integer :: IVAL_MFLUFF1
   integer :: IVAL_MFLUFFN

   integer :: IPNT_S1 ! pointers in valobs work array
   integer :: IPNT_HS
   integer :: IPNT_BL
   integer :: IPNT_SMX
   integer :: IPNT_CMX
   integer :: IPNT_WX
   integer :: IPNT_WY
   integer :: IPNT_RAIN
   integer :: IPNT_INFILTCAP
   integer :: IPNT_INFILTACT
   integer :: IPNT_PATM
   integer :: IPNT_WAVEH
   integer :: IPNT_WAVET
   integer :: IPNT_WAVEL
   integer :: IPNT_WAVED
   integer :: IPNT_WAVER
   integer :: IPNT_WAVEU
   integer :: IPNT_TAUX
   integer :: IPNT_TAUY
   integer :: IPNT_UCX
   integer :: IPNT_UCY
   integer :: IPNT_UCZ
   integer :: IPNT_UCXQ
   integer :: IPNT_UCYQ
   integer :: IPNT_UCXST
   integer :: IPNT_UCYST
   integer :: IPNT_SA1
   integer :: IPNT_UMAG
   integer :: IPNT_QMAG
   integer :: IPNT_TEM1
   integer :: IPNT_TRA1
   integer :: IPNT_HWQ1
   integer :: IPNT_WQB1
   integer :: IPNT_WQB3D1
   integer :: IPNT_SF1
   integer :: IPNT_SFN
   integer :: IPNT_SED
   integer :: IPNT_ZCS
   integer :: IPNT_ZWS
   integer :: IPNT_ZWU
   integer :: IPNT_BRUV
   integer :: IPNT_TKIN
   integer :: IPNT_TEPS
   integer :: IPNT_VIU
   integer :: IPNT_VICWWS
   integer :: IPNT_VICWWU
   integer :: IPNT_WS1
   integer :: IPNT_WSN
   integer :: IPNT_SEDDIF1
   integer :: IPNT_RICH
   integer :: IPNT_TAIR
   integer :: IPNT_WIND
   integer :: IPNT_RHUM
   integer :: IPNT_CLOU
   integer :: IPNT_AIRDENSITY
   integer :: IPNT_QSUN
   integer :: IPNT_QEVA
   integer :: IPNT_QCON
   integer :: IPNT_QLON
   integer :: IPNT_QFRE
   integer :: IPNT_QFRC
   integer :: IPNT_QTOT
   integer :: IPNT_NUM
   integer :: IPNT_RHOP
   integer :: IPNT_RHO
   integer :: IPNT_SBCX1 ! should be done per fraction
   integer :: IPNT_SBCY1
   integer :: IPNT_SBWX1
   integer :: IPNT_SBWY1
   integer :: IPNT_SSCX1
   integer :: IPNT_SSCY1
   integer :: IPNT_SSWX1
   integer :: IPNT_SSWY1
   integer :: IPNT_SOUR1
   integer :: IPNT_SINK1
   integer :: IPNT_BODSED1
   integer :: IPNT_TAUB
   integer :: IPNT_DPSED
   integer :: IPNT_MSED1
   integer :: IPNT_THLYR
   integer :: IPNT_POROS
   integer :: IPNT_LYRFRAC1
   integer :: IPNT_FRAC1
   integer :: IPNT_FRACN
   integer :: IPNT_MUDFRAC
   integer :: IPNT_SANDFRAC
   integer :: IPNT_FIXFAC1
   integer :: IPNT_HIDEXP1
   integer :: IPNT_MFLUFF1

end module m_observations_data

