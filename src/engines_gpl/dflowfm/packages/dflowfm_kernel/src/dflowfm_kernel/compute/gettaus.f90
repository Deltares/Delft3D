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

!
!
module m_gettaus
   implicit none
contains
   subroutine gettaus(typout, kernel)
      use precision, only: dp
      use m_flowgeom, only: ndxi
      use m_flow, only: czs, taus
      use m_alloc, only: realloc
      use m_get_tau, only: get_tau
      use m_waveconst, only: wave_waq_shear_stress_hyd
      use m_flowparameters, only: flowWithoutWaves, jawaveswartdelwaq
      !
      !
      ! Parameters
      integer, intent(in) :: typout !< type of setting, 1: set czs and taus, 2: just set czs:
      integer, intent(in) :: kernel !< kernel requesting to compute taus, 1: D-Flow FM, 2: D-WAQ
      !
      ! Locals
      real(kind=dp) :: taucurc !< local variable for taucurrent
      real(kind=dp) :: czc !< local variable for chezy
      integer :: ierr !< Error code
      integer :: n !< Counter
      integer :: jawaveswartdelwaq_local !< Local value of jawaveswartdelwaq, depending on kernel and flowWithoutWaves
      integer, parameter :: USE_DFLOWFM = 1
      integer, parameter :: SET_CZS_TAUS = 1
      !
      ! Body
      if (flowWithoutWaves .and. kernel == USE_DFLOWFM) then
         jawaveswartdelwaq_local = WAVE_WAQ_SHEAR_STRESS_HYD
      else
         jawaveswartdelwaq_local = jawaveswartdelwaq
      end if
      if (.not. allocated(czs)) then
         call realloc(czs, ndxi, keepExisting=.false., fill=0.0_dp, stat=ierr)
      else if (size(czs) < ndxi) then
         call realloc(czs, ndxi, keepExisting=.false., fill=0.0_dp, stat=ierr)
      end if
      if (typout == SET_CZS_TAUS) then
         if (.not. allocated(taus)) then
            call realloc(taus, ndxi, keepExisting=.false., fill=0.0_dp, stat=ierr)
         else if (size(taus) < ndxi) then
            call realloc(taus, ndxi, keepExisting=.false., fill=0.0_dp, stat=ierr)
         end if
      end if

      do n = 1, ndxi
         call get_tau(n, taucurc, czc, jawaveswartdelwaq_local)
         czs(n) = czc
         if (typout == 1) then
            taus(n) = taucurc
         end if
      end do
   end subroutine gettaus
end module m_gettaus
