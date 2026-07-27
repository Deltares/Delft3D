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
!

module m_setzcs

   implicit none

   private

   public :: setzcs

contains

   !> Sets array zsc (z levels at layer mid-points, only for nudging)
   subroutine setzcs()
      use m_flow, only: zcs, ndkx, zws, layertype, LAYTP_Z, keepzlayeringatbed, zslay
      use m_flowgeom, only: ndx
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_zlayer_indices, only: getzlayerindices
      use m_alloc, only: realloc
      use precision, only: dp
      use m_add_baroclinic_pressure, only: BAROC_ORIGINAL, rhointerfaces

      integer :: kk, k, kb, kt, nlayb, nrlay

      if (.not. allocated(zcs)) then
         call realloc(zcs, ndkx)
      end if

      do kk = 1, Ndx
         call getkbotktop(kk, kb, kt)
         do k = kb, kt
            zcs(k) = 0.5_dp * (zws(k) + zws(k - 1))
         end do
         if (layertype == LAYTP_Z .and. keepzlayeringatbed /= 1 .and. rhointerfaces /= BAROC_ORIGINAL) then
            call getzlayerindices(kk, nlayb, nrlay)
            zcs(kb) = 0.5_dp * (zslay(nlayb - 1, 1) + zslay(nlayb, 1))
            if (kt > kb .and. keepzlayeringatbed == 2) then
               zcs(kb + 1) = 0.5_dp * (zslay(nlayb + 1, 1) + zslay(nlayb, 1))
            end if
         end if
      end do
   end subroutine setzcs

end module m_setzcs
