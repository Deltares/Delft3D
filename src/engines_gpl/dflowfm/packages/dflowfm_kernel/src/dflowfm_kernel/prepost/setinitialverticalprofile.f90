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

 subroutine setinitialverticalprofile(yy, ny, filename) ! polyfil
    use precision, only: dp
    use m_flowgeom
    use m_flow
    use m_polygon
    use m_reapol
    use m_get_kbot_ktop
    use m_get_zlayer_indices
    implicit none
    integer :: ny
    real(kind=dp) :: xx(kmxx)
    real(kind=dp) :: yy(ny)
    character(*), intent(in) :: filename ! file name for polygonfile

    integer :: minp0, n, k, kb, kt, ktx, nlayb, nrlay

    call oldfil(minp0, filename)
    call savepol()
    call reapol(minp0, 0)

    do n = 1, ndxi
       call getkbotktop(n, kb, kt)
       do k = kb, kt
          xx(k - kb + 1) = 0.5d0 * (zws(k) + zws(k - 1))
       end do
       ktx = kt - kb + 1
       if (layertype == 2 .and. keepzlayeringatbed /= 1 .and. jabaroczlaybed == 1) then
          call getzlayerindices(n, nlayb, nrlay)
          xx(1) = 0.5d0 * (zslay(nlayb - 1, 1) + zslay(nlayb, 1))
          if (kt > kb .and. keepzlayeringatbed == 2) then ! only 2
             xx(2) = 0.5d0 * (zslay(nlayb + 1, 1) + zslay(nlayb, 1))
          end if
       end if
       call lineinterp(xx, yy(kb:), ktx, xpl, ypl, npl)
    end do

    call restorepol()

 end subroutine setinitialverticalprofile

 ! 2 subroutines in 1 file, yes we can !

 subroutine keepzlayering()
    use m_flowgeom
    use m_flow
    use m_get_kbot_ktop
    use m_get_zlayer_indices
    implicit none

    integer :: n, kb, kt, nlayb, nrlay, Ltn

    do n = 1, ndxi
       call getkbotktop(n, kb, kt)
       call getzlayerindices(n, nlayb, nrlay)
       Ltn = laydefnr(n)
       zws(kb) = zslay(nlayb, Ltn)
       if (nlayb == 1) then
          zws(kb - 1) = 2 * zslay(nlayb, Ltn) - zslay(nlayb + 1, Ltn)
       else
          zws(kb - 1) = zslay(nlayb - 1, Ltn)
       end if
       if (keepzlayeringatbed == 2) then
          zws(kb) = zslay(nlayb, Ltn)
       end if
    end do
 end subroutine keepzlayering
