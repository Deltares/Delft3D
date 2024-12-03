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

!
!

module m_setzcs

implicit none

private

public :: setzcs

contains

 subroutine setzcs()
    use m_flow, only: zcs, ndkx, zws, jabaroczlaybed, layertype, keepzlayeringatbed, zslay
    use m_flowgeom, only: ndx
    use m_get_kbot_ktop
    use m_get_zlayer_indices
    use m_alloc, only: realloc

    integer :: kk, k, kb, kt, nlayb, nrlay

    if (.not. allocated(zcs)) then
       call realloc(zcs, Ndkx)
    end if

    do kk = 1, Ndx
       call getkbotktop(kk, kb, kt)
       do k = kb, kt
          zcs(k) = 0.5d0 * (zws(k) + zws(k - 1))
       end do
       if (layertype == 2 .and. keepzlayeringatbed /= 1 .and. jabaroczlaybed == 1) then
          call getzlayerindices(kk, nlayb, nrlay)
          zcs(kb) = 0.5d0 * (zslay(nlayb - 1, 1) + zslay(nlayb, 1))
          if (kt > kb .and. keepzlayeringatbed == 2) then ! only 2
             zcs(kb + 1) = 0.5d0 * (zslay(nlayb + 1, 1) + zslay(nlayb, 1))
          end if
       end if

    end do

    return
 end subroutine setzcs

end module m_setzcs
