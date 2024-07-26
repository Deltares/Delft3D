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

 !> Drop land *during* flow computation.
 !!
 !! Use idir=1 for adding land, -1 for lowering it.
 !! The height change is performed on net node vertical zk
 !! With a polygon active: all masked net nodes,
 !! without polygon: all corner points of flow cell underneath mouse pointer.
 subroutine dropland(xp, yp, idir)
    use network_data
    use m_missing
    use m_polygon
    use m_flowgeom
    use m_flow
    use unstruc_display
    use m_sediment
    use geometry_module, only: pinpok, dbpinpol

    implicit none
    double precision, intent(in) :: xp, yp !< Clicked point, which flow node to drop. If a polygon is active, drop all contained points, independent of xp, yp.
    integer, intent(in) :: idir !< direction (1 for up, -1 for down)

    ! locals
    integer :: kk, k, n, nn, in, ncol, j
    double precision :: dropstep !< Amount to add (in meters, may be negative)

    if (ndx == 0) return

    dropstep = idir * zkdropstep

    if (npl > 2) then
       in = -1
       do k = 1, numk
          call DBPINPOL(xk(k), yk(k), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
          if (in == 1 .and. zk(k) /= dmiss) then
             zk(k) = zk(k) + dropstep
             if (jaceneqtr == 2 .and. jased > 0 .and. jased < 4) then
                do j = 1, mxgr
                   grainlay(j, k) = max(0d0, grainlay(j, k) + dropstep / mxgr)
                end do
             end if
             call isocol(zk(k), ncol)
             call movabs(xk(k), yk(k))
             call hlcir2(rcir, ncol, 30)
          end if
       end do
    else

       do n = ndxi, 1, -1
          nn = size(nd(n)%x)
          call PINPOK(Xp, Yp, Nn, nd(n)%x, nd(n)%y, IN, jins, dmiss)
          if (in == 1) then
             do kk = 1, nn
                k = nd(n)%nod(kk)
                zk(k) = zk(k) + dropstep
                if (jaceneqtr == 2 .and. jased > 0) then
                   do j = 1, mxgr
                      grainlay(j, k) = max(0d0, grainlay(j, k) + dropstep / mxgr)
                   end do
                end if
                call isocol(zk(k), ncol)
                call movabs(xk(k), yk(k))
                call hlcir2(rcir, ncol, 30)
             end do
             exit
          end if
       end do
    end if

    call setbobs()
    s1 = max(bl, s1); s0 = s1; s00 = s1

    hs = s1 - bl
    call volsur() ! dropland
    call flow_f0isf1() ! dropland
    volerr = 0; volerrcum = 0

    if (kmx > 0) then
       call setkbotktop(1) ! dropland
    end if

    ! NOTE: vol1tot cumulation now contains an error: new bl's have not been accounted for...
 end subroutine dropland
