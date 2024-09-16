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
module m_postgrid
   implicit none
contains
!> remove skewed cells and cells whose aspect ratio exceeds a prescibed value
!> note: latter not implemented yet
subroutine postgrid()

   use m_grid
   use m_missing, only: dmiss, dxymis
   use geometry_module, only: dbdistance, dcosphi
   use m_sferic, only: jsferic, jasfer3D
   use m_cirr
   use m_get_lr

   integer, dimension(mc) :: ifront
   double precision :: dcos, dcosR, xn, yn
   integer :: i, iL, iR, iRR, idum, iL0, iR0, j, ja, iter, numchanged
   double precision, parameter :: dcosmax = 0.93969
   double precision, parameter :: dtol = 1d-2
   double precision, parameter :: dtolcos = 1d-2

   call tekgrid(i)

   ja = 1
   call confrm('Remove skinny triangles?', ja)

   if (ja == 1) then
!     remove skewed cells
      do j = nc - 1, 2, -1
         ifront = 1
         do iter = 1, 10
            write (6, "('iter = ', i0, ': ')", advance="no") iter
            numchanged = 0
            !        loop over the edges
            !         do i=1,mc-1
            iR = 1
            i = iR
            do while (iR /= mc .or. i /= mc)
               if (iR > i) then
                  i = iR
               else
                  i = i + 1
                  if (i >= mc) exit
               end if

               if (xc(i, j) == DMISS) cycle

               call get_LR(mc, xc(:, j), yc(:, j), i, iL, iR)

               if (dbdistance(xc(i, j), yc(i, j), xc(iR, j), yc(iR, j), jsferic, jasfer3D, dmiss) < dtol) cycle

               !        detect triangular cell
               if (xc(i, j + 1) == DMISS) cycle

               call get_LR(mc, xc(:, j + 1), yc(:, j + 1), i, iL0, iR0)

               if (dbdistance(xc(iL, j), yc(iL, j), xc(i, j), yc(i, j), jsferic, jasfer3D, dmiss) < dtol) iL = i

               if (xc(iR, j + 1) /= DMISS) then
                  if (dbdistance(xc(i, j + 1), yc(i, j + 1), xc(iR, j + 1), yc(iR, j + 1), jsferic, jasfer3D, dmiss) < dtol .and. &
                      dcosphi(xc(i, j + 1), yc(i, j + 1), xc(i, j), yc(i, j), xc(i, j + 1), yc(i, j + 1), xc(iR, j), yc(iR, j), jsferic, jasfer3D, dxymis) > dcosmax) then
                     !              determine persistent node
                     dcos = dcosphi(xc(i, j - 1), yc(i, j - 1), xc(i, j), yc(i, j), xc(i, j), yc(i, j), xc(i, j + 1), yc(i, j + 1), jsferic, jasfer3D, dxymis)
                     dcosR = dcosphi(xc(iR, j - 1), yc(iR, j - 1), xc(iR, j), yc(iR, j), xc(iR, j), yc(iR, j), xc(iR, j + 1), yc(iR, j + 1), jsferic, jasfer3D, dxymis)

                     call get_LR(mc, xc(:, j), yc(:, j), iR, idum, iRR)
                     if ((iRR == iR .or. dcos - dcosR < -dtolcos) .and. iL /= i) then ! move left node
                        call cirr(xc(i, j), yc(i, j), 211)
                        call cirr(xc(iR, j), yc(iR, j), 31)
                        xc(i:iR - 1, j) = xc(iR, j)
                        yc(i:iR - 1, j) = yc(iR, j)
                        numchanged = numchanged + 1
                        write (6, "(I0, '-', I0, 'L ')", advance="no") i, iR - 1
                     else if ((iL == i .or. dcosR - dcos < -dtolcos) .and. iRR /= iR) then ! move right node
                        call cirr(xc(iR, j), yc(iR, j), 211)
                        call cirr(xc(i, j), yc(i, j), 204)
                        xc(iR:iRR - 1, j) = xc(i, j)
                        yc(iR:iRR - 1, j) = yc(i, j)
                        numchanged = numchanged + 1
                        write (6, "(I0, '-', I0, 'R ')", advance="no") iR, iRR - 1
                     else ! move both nodes
                        xn = 0.5d0 * (xc(i, j) + xc(iR, j))
                        yn = 0.5d0 * (yc(i, j) + yc(iR, j))
                        call cirr(xn, yn, 211)
                        xc(i:iR - 1, j) = xn
                        yc(i:iR - 1, j) = yn
                        xc(iR:iRR - 1, j) = xn
                        yc(iR:iRR - 1, j) = yn
                        numchanged = numchanged + 1
                        write (6, "(I0, '-', I0, 'C ')", advance="no") i, iRR - 1
                     end if
                  end if
               end if
            end do
            write (6, *)
            if (numchanged == 0) exit
         end do
         write (6, *) iter, numchanged
      end do
   end if

   return
end subroutine postgrid
end module m_postgrid
