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

subroutine foresterpoint2(constituents, numconst, ndkx, itemp, vol, a, d, km, kmxx, kb, maxit, ip)
   use m_flow, only: eps6, eps10
   implicit none

   double precision :: constituents(numconst, ndkx), vol(kmxx), a(km), d(km)
   integer :: numconst, ndkx, itemp, km, kmxx, kb, maxit, ip

   double precision :: dif
   integer :: k, m, ja

   do k = 1, km
      a(k) = constituents(itemp, kb + k - 1)
   end do

   do m = 1, maxit

      d(1:km) = a(1:km)
      ja = 0

      do k = 1, km - 1
         dif = d(k + 1) - d(k)
         if (dif * ip > eps6 .or. d(k) < 0d0 .or. d(k + 1) < 0d0) then
            if (vol(k) > eps10 .and. vol(k + 1) > eps10) then
               ja = 1
               dif = 0.1666666666667d0 * dif * (vol(k + 1) + vol(k))
               a(k) = a(k) + dif / vol(k)
               a(k + 1) = a(k + 1) - dif / vol(k + 1)
            else
               dif = 0d0
            end if
         end if
      end do

      if (ja == 0) then
         exit
      end if

   end do

   do k = 1, km
      constituents(itemp, kb + k - 1) = a(k)
   end do
   if (kmxx > km) then
      constituents(itemp, kb + km:kb + kmxx - 1) = a(km)
   end if

end subroutine foresterpoint2
