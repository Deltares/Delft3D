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

!> merge two cells with a common link and update administration
subroutine mergecells(ic1, ic2, jatek)
   use m_dellink, only: dellink
   use m_netw
   use m_alloc
   use gridoperations
   use m_tek_link

   implicit none

   integer, intent(in) :: ic1, ic2 !< cell numbers
   integer, intent(in) :: jatek !< plot (1) or not (0)

   integer, allocatable, dimension(:) :: nod3, lin3

   integer :: kk, kk1, kk2, kk3
   integer :: L, L1, L2, Lshare
   integer :: N1, N2, N3

   integer :: ierror

   logical :: Lcommon

   if (ic1 == ic2) return

   if (netstat /= NETSTAT_OK) call findcells(0)

   ierror = 1

   N1 = netcell(ic1)%N
   N2 = netcell(ic2)%N
   N3 = N1 + N2 - 2

!  allocate
   allocate (nod3(N3), lin3(N3))

   ! add links
   Lshare = 0
   kk3 = 0
   do kk1 = 1, N1
      L1 = netcell(ic1)%lin(kk1)
      Lcommon = .false.
!     see if this link is shared with cell 2
      do kk2 = 1, N2
         L2 = netcell(ic2)%lin(kk2)
         if (L1 == L2) then
!           add links of cell 2
            if (kk2 < N2) lin3(kk3 + 1:kk3 + N2 - kk2) = netcell(ic2)%lin(kk2 + 1:N2)
            if (kk2 > 1) lin3(kk3 + N2 - kk2 + 1:kk3 + N2 - 1) = netcell(ic2)%lin(1:kk2 - 1)
            kk3 = kk3 + N2 - 1
            Lcommon = .true.
            Lshare = L1
            exit
         end if
      end do
      if (.not. Lcommon) then
         kk3 = kk3 + 1
         lin3(kk3) = L1
      end if
   end do

   if (kk3 /= N3) then
      continue
   end if

   if (Lshare == 0) goto 1234

!  make the node list
!  determine orientation of first link
   L = lin3(1)
   L2 = lin3(2)
   if (kn(1, L) == kn(1, L2) .or. kn(1, L) == kn(2, L2)) then
      nod3(1:2) = kn(2:1:-1, L)
   else
      nod3(1:2) = kn(1:2, L)
   end if
   do kk = 2, N3 - 1
      L = lin3(kk)
      if (kn(1, L) == nod3(kk - 1) .or. kn(1, L) == nod3(kk)) then
         nod3(kk + 1) = kn(2, L)
      else
         nod3(kk + 1) = kn(1, L)
      end if
   end do

!  change lne
   do kk = 1, N3
      L = lin3(kk)
      if (lne(1, L) == ic2) then
         lne(1, L) = ic1
      else if (lne(1, L) /= ic1 .and. lnn(L) > 1) then
         lne(2, L) = ic1
      end if
   end do

!  change cell 1
   netcell(ic1)%N = N3
   call realloc(netcell(ic1)%nod, N3, keepExisting=.false.)
   netcell(ic1)%nod = nod3
   call realloc(netcell(ic1)%lin, N3, keepExisting=.false.)
   netcell(ic1)%lin = lin3

!  disable cell 2
   netcell(ic2)%N = 0

!  delete link
   if (jatek == 1) call teklink(Lshare, 0)
   call dellink(Lshare)

   ierror = 0

!  error handling
1234 continue

!  deallocate
   if (allocated(nod3)) deallocate (nod3, lin3)

   return
end subroutine mergecells
