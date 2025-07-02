!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
!
!-------------------------------------------------------------------------------------------------------

!> Module for hydrological processes used in the dflowfm kernel.
module m_hydrology
   use m_hydrology_data
   use m_flowgeom
   use horton, only: HORTON_CAPSTAT_NOCHANGE

   implicit none
contains

   !> Initializes all memory for the the hydrology module members.
   !! Intended to be called as part of flow_modelinit().
   !! Actual initialization is done in init_hydrology().
   subroutine alloc_hydrology()
      use m_alloc

      integer :: ierr
      !
      ! Evaporation
      !
      call realloc(PotEvap, ndx, keepExisting=.false., fill=0d0)
      call realloc(ActEvap, ndx, keepExisting=.false., fill=0d0)

      !
      ! Interception
      !
      if (interceptionmodel == DFM_HYD_INTERCEPT_LAYER) then
         call realloc(InterceptThickness, ndx, keepExisting=.false., fill=0d0)
         call realloc(InterceptHs, ndx, keepExisting=.false., fill=0d0)
      end if

      !
      ! Infiltration
      !
      if (infiltrationmodel /= DFM_HYD_NOINFILT) then
         call realloc(infilt, ndx, keepExisting=.false., fill=0d0, stat=ierr)
      end if

      if (infiltrationmodel == DFM_HYD_INFILT_CONST) then
         call realloc(infiltcap, ndx, keepExisting=.false., fill=infiltcapuni, stat=ierr)
      end if

      if (infiltrationmodel == DFM_HYD_INFILT_HORTON) then
         call realloc(infiltcap0, ndx, keepExisting=.false., fill=huge(1d0), stat=ierr)
         call realloc(infiltcap, ndx, keepExisting=.false., fill=0d0, stat=ierr)
         call realloc(HortonMinInfCap, ndx, keepExisting=.false., fill=0d0, stat=ierr)
         call realloc(HortonMaxInfCap, ndx, keepExisting=.false., fill=0d0, stat=ierr)
         call realloc(HortonDecreaseRate, ndx, keepExisting=.false., fill=0d0, stat=ierr)
         call realloc(HortonRecoveryRate, ndx, keepExisting=.false., fill=0d0, stat=ierr)
         call realloc(HortonState, ndx, keepExisting=.false., fill=HORTON_CAPSTAT_NOCHANGE, stat=ierr)
      end if

   end subroutine alloc_hydrology

   !> Initializes the hydrology module and processes.
   !! Intended to be called as part of flow_modelinit().
   !! Memory allocation must have done before by alloc_hydrology().
   subroutine init_hydrology()
      use m_alloc

      !
      ! Infiltration
      !

      ! Start Horton at max infiltration (alsoto trigger decrease mode).
      if (infiltrationmodel == DFM_HYD_INFILT_HORTON) then
         infiltcap = HortonMaxInfCap * 1d-3 / 3600d0 ! mm/hr -> m/s
         infiltcap0 = infiltcap
      end if

   end subroutine init_hydrology

end module m_hydrology
