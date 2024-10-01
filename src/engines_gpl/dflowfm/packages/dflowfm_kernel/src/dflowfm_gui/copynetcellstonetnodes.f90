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

module m_copynetcellstonetnodes

implicit none

contains

subroutine copynetcellstonetnodes() ! for smooth plotting only
   use m_flowgeom, only: ndx2d, ba
   use m_netw, only: numk, rnod, netcell, rlin
   use m_alloc, only: aerr

   integer :: k, kk, kkk, n, nn4, ierr, ja
   real, allocatable, save :: rn(:)
   double precision :: znn

   ja = 0
   if (.not. allocated(rn)) then
      ja = 1
   else if (size(rn) < numk) then
      deallocate (rn); ja = 1
   end if
   if (ja == 1) then
      allocate (rn(numk), stat=ierr)
      call aerr('rn(numk)', ierr, numk)
   end if

   rnod = 0d0; rn = 0d0
   do n = 1, ndx2d
      nn4 = netcell(n)%n

      znn = rlin(n)

      do kk = 1, nn4
         kkk = netcell(n)%nod(kk)
         rnod(kkk) = rnod(kkk) + znn * ba(n)
         rn(kkk) = rn(kkk) + ba(n)
      end do
   end do

   do k = 1, numk
      if (rn(k) > 0) then
         rnod(k) = rnod(k) / rn(k)
      end if
   end do

end subroutine copynetcellstonetnodes !in afwachting van isosmoothflownodes

end module m_copynetcellstonetnodes
