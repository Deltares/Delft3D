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

subroutine makethindamadmin()
   use precision, only: dp
   use m_flowgeom
   use network_data
   use m_alloc
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: getdxdy, duitpl, dlinedis
   use m_missing, only: dmiss

   implicit none

   integer :: n, kk, L, k1, k3, k4, ja, ierr
   real(kind=dp) :: cs, sn, a, b, sig, dis, xn, yn, rrr

   ! count thin dam points
   nthd = 0
   do n = 1, nump
      do kk = 1, netcell(n)%n
         L = netcell(n)%lin(kk)
         if (lne2ln(L) == 0) then
            nthd = nthd + 1
         end if
      end do
   end do

   ! set up thin dam structure
   if (allocated(thindam)) deallocate (thindam)
   allocate (thindam(6, nthd), stat=ierr)
   call aerr('thindam(6,nthd)', ierr, 6 * nthd)

   nthd = 0
   do n = 1, nump
      do kk = 1, netcell(n)%n
         L = netcell(n)%lin(kk)

         if (kn(3, L) == 0) then
            nthd = nthd + 1
            k1 = n
            k3 = kn(1, L) ! netnode 1
            k4 = kn(2, L) ! netnode 2

            thindam(1, nthd) = k1
            thindam(2, nthd) = k3
            thindam(3, nthd) = k4

            call duitpl(xzw(k1), yzw(k1), xk(k3), yk(k3), xzw(k1), yzw(k1), xk(k4), yk(k4), sig, jsferic)
            call dlinedis(xzw(k1), yzw(k1), xk(k3), yk(k3), xk(k4), yk(k4), JA, DIS, XN, YN, jsferic, jasfer3D, dmiss)
            a = 0d0; b = 0d0
            call getdxdy(xk(k3), yk(k3), xk(k4), yk(k4), a, b, jsferic)
            rrr = sqrt(a * a + b * b)
            cs = 0d0; sn = 0d0
            if (rrr /= 0d0) then
               cs = sig * a / rrr
               sn = sig * b / rrr
            end if
            thindam(4, nthd) = cs
            thindam(5, nthd) = sn
            thindam(6, nthd) = rrr
         end if
      end do
   end do
end subroutine makethindamadmin
