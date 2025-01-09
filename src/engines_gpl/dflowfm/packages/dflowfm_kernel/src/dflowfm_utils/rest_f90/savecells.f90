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

module m_savecells

   implicit none

   private

   public :: savecells

contains

   subroutine savecells() !! save netcell, lne, lnn, idomain
      use network_data
      use m_partitioninfo, only: idomain, idomain0
      use m_flowgeom, only: xz, xz0, yz, yz0, ba, ba0
      use m_alloc

      integer :: ierr
      integer :: k, N

      nump0 = nump
      nump1d2d0 = nump1d2d

      if (allocated(netcell0)) then
         do k = 1, ubound(netcell0, 1)
            if (allocated(netcell0(k)%nod)) deallocate (netcell0(k)%nod)
            if (allocated(netcell0(k)%lin)) deallocate (netcell0(k)%lin)
         end do
         deallocate (netcell0)
      end if

      allocate (netcell0(nump1d2d), stat=ierr)
      do k = 1, nump1d2d
         N = netcell(k)%N
         netcell0(k)%N = N

         allocate (netcell0(k)%nod(N))
         netcell0(k)%nod = netcell(k)%nod(1:N)

         allocate (netcell0(k)%lin(N))
         netcell0(k)%lin = netcell(k)%lin(1:N)
      end do

!   netcell0(1: nump1d2d) = netcell(1: nump1d2d)

      call realloc(lne0, (/2, numl/), stat=ierr, keepExisting=.false.)
      call realloc(lnn0, numl, stat=ierr, keepExisting=.false.)
      lne0 = lne
      lnn0 = lnn

      call realloc(xz0, nump1d2d, stat=ierr, keepExisting=.false.)
      call realloc(yz0, nump1d2d, stat=ierr, keepExisting=.false.)
      xz0 = xz
      yz0 = yz

      call realloc(xzw0, nump1d2d, stat=ierr, keepExisting=.false.)
      call realloc(yzw0, nump1d2d, stat=ierr, keepExisting=.false.)
      xzw0 = xzw
      yzw0 = yzw

      call realloc(ba0, nump1d2d, stat=ierr, keepExisting=.false.)
      ba0 = ba

      if (allocated(idomain)) then
         call realloc(idomain0, nump1d2d, stat=ierr, keepExisting=.false.)
         idomain0 = idomain
      end if

   end subroutine savecells

end module m_savecells
