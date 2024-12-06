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

!>    copy and move a polygon orthogonally
      subroutine copypol(ipol, xp, yp)
         use precision, only: dp
         use m_sferic
         use m_polygon
         use m_sferic
         use m_missing
         use geometry_module, only: getdxdy

         implicit none

         integer, intent(in) :: ipol !< polygon point
         real(kind=dp), intent(in) :: xp, yp !< new polygon point coordinates

         real(kind=dp), dimension(:), allocatable :: dnx, dny !< node-based normal vectors

         real(kind=dp) :: dsx, dsy, dnxL, dnyL, dnxR, dnyR, ds, dist, fac
         real(kind=dp) :: dnxLi, dnyLi, dnxRi, dnyRi, dnxi, dnyi, dx, dy, det

         integer :: i, jstart, jend, jpoint, numadd

         logical :: Lotherpart = .true. !< also make other part (.true.) or not (.false.)

!        find the start and end index in the polygon array
         call get_polstartend(NPL, XPL, YPL, ipol, jstart, jend)

         numadd = jend - jstart + 1

         if (numadd < 2 .or. ipol < jstart .or. ipol > jend) return ! no polygon found

         if (Lotherpart) numadd = 2 * numadd

!        copy polygon
         jpoint = NPL
         if (xpl(jpoint) /= DMISS) then ! add dmiss
            NPL = NPL + numadd + 1
            call increasepol(NPL, 1)
            jpoint = jpoint + 1
            xpl(jpoint) = DMISS
            ypl(jpoint) = DMISS
            zpl(jpoint) = DMISS
            jpoint = jpoint + 1
         else
            NPL = NPL + numadd
            call increasepol(NPL, 1)
            jpoint = jpoint + 1
         end if

!        allocate
         allocate (dnx(numadd), dny(numadd))

         dnxLi = 0d0
         dnyLi = 0d0
         dnxRi = 0d0
         dnyRi = 0d0
         dnxi = 0d0
         dnyi = 0d0
!        compute normal vectors
         do i = jstart, jend
            if (i < jend) then
               !dsx = getdx(xpl(i),ypl(i),xpl(i+1),ypl(i+1))
               !dsy = getdy(xpl(i),ypl(i),xpl(i+1),ypl(i+1))
               call getdxdy(xpl(i), ypl(i), xpl(i + 1), ypl(i + 1), dsx, dsy, jsferic)
               ds = sqrt(dsx**2 + dsy**2)
               dnxR = -dsy / ds
               dnyR = dsx / ds
            else
               dnxR = dnxL
               dnyR = dnyL
            end if

            if (i == jstart) then
               dnxL = dnxR
               dnyL = dnyR
            end if

            fac = 1d0 / (1d0 + dnxL * dnxR + dnyL * dnyR)
            dnx(i - jstart + 1) = fac * (dnxL + dnxR)
            dny(i - jstart + 1) = fac * (dnyL + dnyR)

!           store normal vectors for selected polygon point
            if (i == ipol) then
               dnxLi = dnxL
               dnyLi = dnyL
               dnxRi = dnxR
               dnyRi = dnyR
               dnxi = dnx(i - jstart + 1)
               dnyi = dny(i - jstart + 1)
            end if

            dnxL = dnxR
            dnyL = dnyR
         end do

!        determine layer thickness
         ! dx = getdx(xpl(ipol),ypl(ipol),xp,yp)
         ! dy = getdy(xpl(ipol),ypl(ipol),xp,yp)
         call getdxdy(xpl(ipol), ypl(ipol), xp, yp, dx, dy, jsferic)
         det = dx * dnyi - dy * dnxi
         if (det > 1d-8) then
            dist = dx * dnxRi + dy * dnyRi
         else if (det < -1d-8) then
            dist = dx * dnxLi + dy * dnyLi
         else
            dist = dx * dnxi + dy * dnyi
         end if

!        add new polygon
         if (jsferic == 1) dist = dist / (Ra * dg2rd)
         do i = jstart, jend
            dy = dny(i - jstart + 1) * dist
            dx = dnx(i - jstart + 1) * dist
            if (jsferic == 1) dx = dx / cos((ypl(i) + 0.5d0 * dy) * dg2rd)
            xpl(jpoint + i - jstart) = xpl(i) + dx
            ypl(jpoint + i - jstart) = ypl(i) + dy
            zpl(jpoint + i - jstart) = zpl(i)
            if (Lotherpart) then
               xpl(jpoint + numadd - 1 - (i - jstart)) = xpl(i) - dx
               ypl(jpoint + numadd - 1 - (i - jstart)) = ypl(i) - dy
               zpl(jpoint + numadd - 1 - (i - jstart)) = zpl(i)
            end if
         end do

!        deallocate
         deallocate (dnx, dny)

         return
      end subroutine copypol
