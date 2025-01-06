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

module m_dropzout

implicit none

private

public :: dropzout

contains

 subroutine dropzout(idir)
    use precision, only: dp
    use m_isocol
    use m_polygon
    use m_flowgeom
    use m_flow
    use m_transportdata
    use m_missing, only: dmiss, jins
    use geometry_module, only: dbpinpol
    use m_get_kbot_ktop
    use m_set_kbot_ktop
    use m_pfiller

    integer, intent(in) :: idir !< direction (1 for up, -1 for down)

    ! locals
    integer :: n, nn, in, ncol, k, kb, kt
    real(kind=dp) :: dropstep

    if (ndx == 0) return

    dropstep = idir * sdropstep

    if (npl > 2) then
       in = -1
       do n = 1, ndx
          call DBPINPOL(xz(n), yz(n), IN, dmiss, jins, NPL, xpl, ypl, zpl)
          if (in == 1) then
             call getkbotktop(n, kb, kt)
             if (idir == 1) then
                kb = kb + kplot - 1
             end if
             do k = kb, kt
                sam1tot = sam1tot - constituents(isalt, k) * vol0(k)
                constituents(isalt, k) = max(0d0, constituents(isalt, k) + dropstep)
                sam1tot = sam1tot + constituents(isalt, k) * vol1(k)
                call isocol(constituents(isalt, n), ncol)
                nn = size(nd(n)%x)
                call pfiller(nd(n)%x, nd(n)%y, nn, ncol, 30)
             end do
          end if
       end do

    else

       n = nplot
       call getkbotktop(n, kb, kt)
       k = kb + kplot - 1
       sam1tot = sam1tot - constituents(isalt, k) * vol0(k)
       constituents(isalt, k) = max(0d0, constituents(isalt, k) + dropstep)
       sam1tot = sam1tot + constituents(isalt, k) * vol1(k)
       call isocol(constituents(isalt, n), ncol)
       nn = size(nd(n)%x)
       call pfiller(nd(n)%x, nd(n)%y, nn, ncol, 30)
    end if

    if (kmx > 0) then
       call setkbotktop(1) ! drop
    end if

 end subroutine dropzout

end module m_dropzout
