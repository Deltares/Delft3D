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

module m_tekbathy

implicit none

contains

 subroutine tekbathy(ja)
    use m_isocol2
    use m_halt
    use unstruc_display
    use m_flowgeom
    use m_flow
    use gridoperations
    use m_drawthis
    use m_pfiller
    use m_inview
    use m_znod
    implicit none
    integer :: nodewhat
    integer :: k, ja, nn, ncol
    double precision :: zn

    if (ndraw(39) == 0) return

    nodewhat = ndraw(28)
    ndraw(28) = 3

    do k = 1, ndxi
       if (mod(k, 200) == 0) then
          call halt(ja)
          if (ja == 1) then
             ndraw(28) = nodewhat
             return
          end if
       end if

       if (inview(xz(k), yz(k))) then
          zn = znod(k)
          call isocol2(zn, ncol)
          nn = size(nd(k)%x)
          call PFILLER(nd(k)%x, nd(k)%y, nn, NCOL, NCol)
       end if
    end do

    ndraw(28) = nodewhat
 end subroutine tekbathy

end module m_tekbathy
