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

module m_dropwater

implicit none

private

public :: dropwater

contains

 !> Drop water *during* flow computation.
 !!
 !! Use idir=1 for adding water, -1 for lowering it.
 subroutine dropwater(xp, yp, idir)
    use precision, only: dp
    use m_isocol
    use m_polygon
    use m_flowgeom
    use m_flow
    use m_missing, only: dmiss, JINS
    use geometry_module, only: pinpok, dbpinpol
    use m_set_kbot_ktop
    use m_volsur
    use m_flow_f0isf1
    use m_pfiller

    real(kind=dp), intent(in) :: xp, yp !< Clicked point, which flow node to drop. If a polygon is active, drop all contained points, independent of xp, yp.
    integer, intent(in) :: idir !< direction (1 for up, -1 for down)

    ! locals
    integer :: n, nn, in, ncol
    real(kind=dp) :: dropstep, s10

    if (ndx == 0) return

    dropstep = idir * sdropstep

    if (npl > 2) then
       in = -1
       do n = 1, ndxi
          call DBPINPOL(xz(n), yz(n), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
          if (in == 1) then
             s10 = s1(n)
             s1(n) = max(bl(n), s1(n) + dropstep)
             vol0tot = vol0tot + (s1(n) - s10) * ba(n)

             call isocol(s1(n), ncol)
             nn = size(nd(n)%x)
             call pfiller(nd(n)%x, nd(n)%y, nn, ncol, 30)
          end if
       end do
    else

       do n = ndxi, 1, -1
          nn = size(nd(n)%x); if (nn == 0) cycle
          call PINPOK(Xp, Yp, Nn, nd(n)%x, nd(n)%y, IN, jins, dmiss)
          if (in == 1) then
             s10 = s1(n)
             s1(n) = max(bl(n), s1(n) + dropstep)

             call isocol(s1(n), ncol)
             nn = size(nd(n)%x)
             call pfiller(nd(n)%x, nd(n)%y, nn, ncol, 30)
             exit
          end if
       end do
    end if

    hs = s1 - bl
    call volsur() ! dropwater
    call flow_f0isf1() ! dropwater
    volerr = 0; volerrcum = 0

    if (kmx > 0) then
       call setkbotktop(1) ! dropwater
    end if

    validateon = .false.

 end subroutine dropwater

end module m_dropwater
