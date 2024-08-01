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

!>    copy and move a whole spline
      subroutine copyspline(ispline, inode, xp, yp)
         use m_splines
         use m_sferic
         use geometry_module, only: dbdistance, dcosphi
         use m_missing, only: dmiss

         implicit none

         integer, intent(inout) :: ispline !< spline number
         integer, intent(in) :: inode !< spline control point
         double precision, intent(in) :: xp, yp !< new spline control point coordinates

         double precision, dimension(maxsplen) :: xspp, yspp, xlist, ylist

         double precision :: dnx, dny, dsx, dsy, curv, alphan, alphas
         double precision :: x0, y0, x1, y1, ds, t

         integer :: i, j, num

         double precision, parameter :: EPS = 1d-4

         integer, parameter :: Nresample = 1

         call nump(ispline, num)
         if (ispline > 0 .and. ispline <= maxspl .and. inode > 0 .and. inode <= num) then
            x0 = xsp(ispline, inode)
            y0 = ysp(ispline, inode)

            xlist(1:num) = xsp(ispline, 1:num)
            ylist(1:num) = ysp(ispline, 1:num)
            call spline(xlist, num, xspp)
            call spline(ylist, num, yspp)
            call comp_curv(num, xlist, ylist, xspp, yspp, dble(inode - 1), curv, dnx, dny, dsx, dsy)

            ds = dbdistance(x0, y0, xp, yp, jsferic, jasfer3D, dmiss)
            if (jsferic == 1) then
               ds = ds / (Ra * dg2rd)
            end if

            alphan = dcosphi(x0, y0, x0 + EPS * dnx, y0 + EPS * dny, x0, y0, xp, yp, jsferic, jasfer3D, dxymis) * ds
            alphas = 0d0

            call newspline()

!           copy and sample spline
            call spline(xlist, num, xspp)
            call spline(ylist, num, yspp)
            do i = 1, num
               do j = 1, Nresample
                  t = dble(i - 1) + dble(j - 1) / dble(Nresample)
                  call splint(xlist, xspp, num, t, x1)
                  call splint(ylist, yspp, num, t, y1)
                  call addsplinepoint(mcs, x1, y1)
               end do
            end do

!           move spline
            ispline = mcs ! activate new spline
            call nump(ispline, num)
            call spline(xlist, num, xspp)
            call spline(ylist, num, yspp)

!            ds = dbdistance(x0,y0,xp,yp)

!            alphan = dcosphi(x0,y0,x0+EPS*dnx,y0+EPS*dny,x0,y0,xp,yp)*ds
!!            alphas = dcosphi(x0,y0,x0+EPS*dsx,y0+EPS*dsy,x0,y0,xp,yp)*ds
!            alphas = 0d0

            do i = 1, num
               call comp_curv(num, xlist, ylist, xspp, yspp, dble(i - 1), curv, dnx, dny, dsx, dsy)
               x1 = xsp(ispline, i) + alphan * dnx + alphas * dsx
               y1 = ysp(ispline, i) + alphan * dny + alphas * dsy
               xsp(ispline, i) = x1
               ysp(ispline, i) = y1
            end do
         end if
         return
      end subroutine copyspline
