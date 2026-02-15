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

module m_setinitialverticalprofile

   implicit none

   private

   public :: setinitialverticalprofile

contains

   !> Sets initial vertical profile of e.g., salinity and temperature
   subroutine setinitialverticalprofile(target_array, ndkx, filename)
      use precision, only: dp
      use m_flowgeom, only: ndxi
      use m_flow, only: zws, layertype, LAYTP_Z, keepzlayeringatbed, zslay, kmxx
      use m_polygon, only: xpl, ypl, npl, savepol, restorepol
      use m_reapol, only: reapol
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_zlayer_indices, only: getzlayerindices
      use m_filez, only: oldfil
      use m_add_baroclinic_pressure, only: BAROC_ORIGINAL, rhointerfaces

      real(kind=dp), intent(in), dimension(ndkx) :: target_array !< Target array - e.g., sa1 (salinity) or tem1 (temperature)
      integer, intent(in) :: ndkx !< Dimension of 3d flow nodes (internal + boundary)
      character(len=*), intent(in) :: filename !< Filename of polygonfile

      real(kind=dp) :: z_center(kmxx)
      integer :: minp0, n, k, kb, kt, ktx, nlayb, nrlay

      call oldfil(minp0, filename)
      call savepol()
      call reapol(minp0, 0)

      do n = 1, ndxi
         call getkbotktop(n, kb, kt)
         do k = kb, kt
            z_center(k - kb + 1) = 0.5_dp * (zws(k) + zws(k - 1))
         end do
         ktx = kt - kb + 1
         if (layertype == LAYTP_Z .and. keepzlayeringatbed /= 1 .and. rhointerfaces /= BAROC_ORIGINAL) then
            call getzlayerindices(n, nlayb, nrlay)
            z_center(1) = 0.5_dp * (zslay(nlayb - 1, 1) + zslay(nlayb, 1))
            if (kt > kb .and. keepzlayeringatbed == 2) then
               z_center(2) = 0.5_dp * (zslay(nlayb + 1, 1) + zslay(nlayb, 1))
            end if
         end if
         call lineinterp(z_center, target_array(kb:), ktx, xpl, ypl, npl)
      end do

      call restorepol()

   end subroutine setinitialverticalprofile

end module m_setinitialverticalprofile
