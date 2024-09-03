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

!> perform the adminstration:
!!   determine the netcells and nodes in the stencil
subroutine orthonet_admin(k0, adm, ierror)
   use m_netw
   use m_missing
   use m_alloc
   use m_inverse_map
   use m_qnerror

   implicit none

   integer, intent(in) :: k0 !< center node
   type(tadm), intent(inout) :: adm !< structure with administration
   integer, intent(out) :: ierror !< 0: no error, 1: error

   integer :: k1, k2, L1, L2, i1, i2, L
   integer :: N, Nsize, Ksize
   integer :: ic, kcell, knode
   integer :: i, inewcell

   logical :: lisnew

   if (k0 == 42) then
      continue
   end if

   ierror = 1

   if (.not. allocated(adm%icell)) allocate (adm%icell(nmkx))
   if (.not. allocated(adm%kk2)) allocate (adm%kk2(nmkx2))
   if (.not. allocated(adm%kkc)) allocate (adm%kkc(M, nmkx))

   Nsize = ubound(adm%icell, 1) ! array size of icell
   Ksize = ubound(adm%kk2, 1) ! array size of adm%kk2

   adm%icell = 0
   adm%Ncell = 0
   if (nmk(k0) < 2) then
      call qnerror('orthonet_admin: nmk(k0) .lt. 2)', ' ', ' ')
      return
   end if

   inewcell = -1234
   do k1 = 1, nmk(k0)
      L1 = nod(k0)%lin(k1)
      k2 = k1 + 1
      if (k2 > nmk(k0)) k2 = 1

!      do while ( k2.ne.k1 ) ! try to find a common cell and the shared link (no folds: the next link)
      L2 = nod(k0)%lin(k2)

      if ((lnn(L1) < 1) .or. (lnn(L2) < 1)) then
         cycle
      end if

      !     find the cell that links L1 and L2 share
      i1 = max(min(lnn(L1), 2), 1)
      i2 = max(min(lnn(L2), 2), 1)

      if ((lne(1, L1) == lne(1, L2) .or. lne(1, L1) == lne(i2, L2)) .and. lne(1, L1) /= inewcell) then
         inewcell = lne(1, L1)
!            exit
      else if ((lne(i1, L1) == lne(1, L2) .or. lne(i1, L1) == lne(i2, L2)) .and. lne(i1, L1) /= inewcell) then
         inewcell = lne(i1, L1)
!            exit
      else
         inewcell = -1234 ! fictitious boundary cell
      end if

!         k2 = k2+1
!         if ( k2.gt.nmk(k0) ) k2=1
!      end do

      if (nmk(k0) == 2 .and. k1 == 2 .and. nb(k0) == 3) then
         if (inewcell == adm%icell(1)) inewcell = -1234 ! cornercell
      end if

      adm%Ncell = adm%Ncell + 1
!     reallocate icell array if necessary
      if (adm%Ncell > Nsize) then
         Nsize = adm%Ncell
         call realloc(adm%icell, Nsize)
      end if
      adm%icell(adm%Ncell) = inewcell
   end do

!  check if any cells are found and terminate otherwise
   if (adm%Ncell < 1) goto 1234

!  reallocate kkc if necessary
   if (adm%Ncell > ubound(adm%kkc, 2)) call realloc(adm%kkc, (/M, adm%Ncell/))

!  make the node administration kk2 and kkc
   adm%kk2 = 0
!  start with center node
   adm%kk2(1) = k0
   adm%nmk2 = 1
   adm%nmk = nmk(k0)

!  continue with the link-connected nodes
   do ic = 1, nmk(k0)
      L = nod(k0)%lin(ic)
      knode = kn(1, L) + kn(2, L) - k0
      adm%nmk2 = adm%nmk2 + 1
!        check array size
      if (adm%nmk2 > Ksize) then
         Ksize = Ksize + 1
         call realloc(adm%kk2, Ksize, fill=0)
      end if
      adm%kk2(adm%nmk2) = knode
   end do

!  continue with other nodes and fill kkc

   do ic = 1, adm%Ncell
      kcell = adm%icell(ic)
      if (kcell < 1) cycle ! for fictitious boundary cells

!     forward to center node
      k1 = 0
      do while (netcell(kcell)%nod(k1 + 1) /= k0)
         k1 = k1 + 1
      end do

!     loop over the other nodes
      N = netcell(kcell)%n
      do i = 1, N
         k1 = k1 + 1
         if (k1 > N) k1 = k1 - N

         knode = netcell(kcell)%nod(k1)

!        check if node is already administered and exclude center node
         lisnew = .true.
         do i2 = 1, adm%nmk2
            if (knode == adm%kk2(i2)) then
               lisnew = .false.
               adm%kkc(k1, ic) = i2 ! position of the node in adm%kk2
               exit
            end if
         end do

!        administer new node
         if (lisnew) then
            adm%nmk2 = adm%nmk2 + 1
!           check array size
            if (adm%nmk2 > Ksize) then
               Ksize = Ksize + 1
               call realloc(adm%kk2, Ksize, fill=0)
            end if
            adm%kk2(adm%nmk2) = knode
            adm%kkc(k1, ic) = adm%nmk2 ! position of the node in adm%kk2
         end if
      end do
   end do

1234 continue

   ierror = 0

end subroutine orthonet_admin
