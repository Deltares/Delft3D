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

!> copy netboundary to polygon, starting from a specified point
subroutine netboundtopoly(kstart)
   use precision, only: dp
   use m_polygon
   use m_netw
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dprodout

   implicit none

   integer, intent(in) :: kstart !< startnode

   integer, dimension(:), allocatable :: klist ! list of new startnodes
   integer :: nlist ! number of entries in list
   integer :: ilist ! position in list

   real(kind=dp) :: crs

   integer :: iorient !  orientation of branch, net on left (1) or right (0) or do not consider (-1)

   integer :: i, iDi, i_, ic, inew, k, knext, L, Lprev, num
   integer :: iorient_new, ja_addednode
   integer :: knext_store, iorient_new_store, L_store

   integer :: ierror

   ierror = 1

!  allocate
   allocate (klist(1))
   klist = 0
   nlist = 0

!  add startnode to list
   nlist = nlist + 1
   if (nlist > size(klist)) call realloc(klist, int(1.2d0 * dble(nlist)) + 1, fill=0, keepExisting=.true.)
   klist(nlist) = kstart

!  process the startnode list
   ilist = 0
   do while (ilist < nlist)
      ilist = ilist + 1

!     inialization
      k = klist(ilist)
      iorient = -1
      inew = 1

!     make a branch
      num = 0
      do
         ja_addednode = 0

         i = 1

         if (inew /= 1) then
            do while (nod(k)%lin(i) /= Lprev .and. i < nmk(k)); i = i + 1; end do
            if (nod(k)%lin(i) /= Lprev) then ! should not happen
               continue
               return
            end if
         end if

         if (iorient == 1) then
            iDi = 1
         else
            iDi = -1
         end if

!        loop over links connected to k
         do i_ = 1, nmk(k)
            i = i + iDi
            if (i > nmk(k)) i = i - nmk(k)
            if (i < 1) i = i + nmk(k)

            L = nod(k)%lin(i)
            if (Lc(L) /= 1) cycle

            knext = kn(1, L) + kn(2, L) - k

!           determine orientation
            ic = lne(1, L)
            crs = dprodout(xk(k), yk(k), xk(knext), yk(knext), xk(k), yk(k), xzw(ic), yzw(ic), jsferic, jasfer3D)
            iorient_new = -1
            if (crs > 0d0) then
               iorient_new = 1
            else if (crs < 0d0) then
               iorient_new = 0
            end if

!           check if we have to add a branch (orientation differs or already added node)
            if ((iorient_new /= iorient .and. iorient /= -1) .or. ja_addednode == 1) then
!              orientation differs: start a new branch later
               inew = 1

!              add new startnode to list
               nlist = nlist + 1
               if (nlist > size(klist)) call realloc(klist, int(1.2d0 * dble(nlist)) + 1, fill=0, keepExisting=.true.)
               klist(nlist) = k
               cycle ! do not add this node to branch
            end if

!           add point to polygon
            ja_addednode = 1
            num = num + 1
            if (inew == 1) then ! also add DMISS and first point
               if (NPL > 1) then
                  if (xpl(NPL) /= DMISS) then
                     call increasepol(NPL + 1, 1)
                     NPL = NPL + 1
                     xpl(NPL) = DMISS
                     ypl(NPL) = DMISS
                     zpl(NPL) = DMISS
                  end if
               end if
               call increasepol(NPL + 1, 1)
               NPL = NPL + 1
               xpl(NPL) = xk(k)
               ypl(NPL) = yk(k)
               zpl(NPL) = dble(k)
            end if
            call increasepol(NPL + 1, 1)
            NPL = NPL + 1
            xpl(NPL) = xk(knext)
            ypl(NPL) = yk(knext)
            zpl(NPL) = dble(knext)

!           deactivate link
            Lc(L) = 0

            inew = 0

!           remember the added new node
            knext_store = knext
            iorient_new_store = iorient_new
            L_store = L

!           set branche orientation if unset
            if (iorient == -1) then
               iorient = iorient_new
            end if
         end do ! do i=1,nmk(k)
         if (ja_addednode == 1) then
            k = knext_store
            iorient_new = iorient_new_store
            Lprev = L_store
         else
            exit
         end if
      end do

      if (num > 0 .and. iorient /= 0) then ! branch has ended: fix orientation if necessary
         call flippo(NPL)
      end if
   end do ! do while ( ilist.lt.nlist )

!  merge branches (unfortunately, the orientation may now change)
   call merge_polylines()

   ierror = 0
1234 continue

   if (allocated(klist)) deallocate (klist)

   return
end subroutine netboundtopoly
