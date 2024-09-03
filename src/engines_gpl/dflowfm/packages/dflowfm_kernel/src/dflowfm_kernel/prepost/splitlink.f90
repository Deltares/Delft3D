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

!> split a link, make new cells and update administration
subroutine splitlink(xp, yp, L_, dcosmin, jatek, ierror)
   use m_netw
   use network_data, only: xzw, yzw
   use m_flowgeom, only: xz, yz, ba
   use unstruc_colors, only: ncoldn
   use geometry_module, only: dbdistance, dcosphi
   use m_sferic, only: jsferic, jasfer3D, dtol_pole
   use m_missing, only: dxymis
   use gridoperations
   use m_alloc
   use m_qnerror

   implicit none

   double precision, intent(in) :: xp, yp !< clicked point coordinates (used if L.eq.0)
   integer, intent(in) :: L_ !< link number (used if L_.ne.0)
   double precision, intent(in) :: dcosmin !< parallelogram cosine tolerance
   integer, intent(in) :: jatek !< plot new links (1) or not (0)
   integer, intent(out) :: ierror ! error (1) or not (0)

   double precision :: zp ! link z-value

   double precision :: zzz, dcos1, dcos2, dcos3

   integer :: L ! link number
   integer :: ic1, icL, icR ! cell numbers
   integer :: LL, LR ! left and right connected links
   integer :: LnL, LnR ! new links
   integer :: kk1, kkL, kkR
   integer :: k1, k2, k3, kp
   integer :: i, N, kL, kR, kLL, kRR, LnLL, LnRR
   integer :: idum, icLL, icRR, kLLL, kRRR, numnew
   integer :: N2Dcells

   ierror = 1

   if (netstat /= NETSTAT_OK) then
      call findcells(100)
   end if

   if (L_ == 0) then
      L = 0
      call islink(L, xp, yp, zp)
   else
      L = L_
   end if

   if (L == 0) goto 1234

   if (jatek == 1) call teklink(L, 0)
   k1 = kn(1, L)
   k2 = kn(2, L)
   k3 = kn(3, L)

   icL = 0
   icR = 0
   icLL = 0
   icRR = 0

   !  count number of ajacent 2D cells
   if (kn(3, L) /= 2) then ! non-2D netlink
      N2Dcells = 0
   else
      N2Dcells = lnn(L)
   end if

   !  non-2D netlink, or isolated 2D netlink, or netlink outside selecting polygon
   if (N2Dcells == 0) then
      !  add node
      !call setnewpoint(0.5d0*(xk(k1)+xk(k2)), 0.5d0*(yk(k1)+yk(k2)), zp, kp)
      call dsetnewpoint(0.5d0 * (xk(k1) + xk(k2)), 0.5d0 * (yk(k1) + yk(k2)), kp)

      call connectdbn(k1, kp, LnL)
      if (jatek == 1) call teklink(LnL, ncoldn)
      kn(3, LnL) = k3
      call connectdbn(kp, k2, LnR)
      if (jatek == 1) call teklink(LnR, ncoldn)
      kn(3, LnR) = k3

      !     set lnn and lne for new links
      !     reallocate if necessary
      if (numL > ubound(lnn, 1)) then
         numnew = ceiling(1.2d0 * dble(numL))
         call realloc(lnn, numnew, keepExisting=.true.)
         call realloc(lne, (/2, numnew/), keepExisting=.true.)
      end if
      lnn(LnL) = 0
      lnn(LnR) = 0
      lne(1, LnL) = 0
      lne(2, LnL) = 0
      lne(1, LnR) = 0
      lne(2, LnR) = 0
      if (jatek == 1) call dcirr(xk(kp), yk(kp), zk(kp), ncoldn)
   end if

!  insert and connect new node
   do i = 1, N2Dcells
      ic1 = lne(i, L)
      N = netcell(ic1)%N

!     find the link in the cell
      kk1 = 1
      do while (netcell(ic1)%lin(kk1) /= L .and. kk1 < N); kk1 = kk1 + 1; end do
      if (netcell(ic1)%lin(kk1) /= L) then
         call qnerror('splitlink: link not found', ' ', ' ')
         goto 1234
      end if

!     find the left and right connected links and cells
      kkL = kk1 - 1; if (kkL < 1) kkL = kkL + N
      kkR = kk1 + 1; if (kkR > N) kkR = kkR - N
      LL = netcell(ic1)%lin(kkL)
      LR = netcell(ic1)%lin(kkR)
      icL = 0
      if (lnn(LL) > 1) icL = lne(1, LL) + lne(2, LL) - ic1
      icR = 0
      if (lnn(LR) > 1) icR = lne(1, LR) + lne(2, LR) - ic1

!     find the left and right original nodes (either k1 or k2)
      if (kn(1, LL) == k1 .or. kn(2, LL) == k1) then
         kL = k1
         kR = k2
      else
         kL = k2
         kR = k1
      end if

!     add node and make new links (once)
      if (i == 1) then
         !  add node
         call dsetnewpoint(0.5d0 * (xk(kL) + xk(kR)), 0.5d0 * (yk(kL) + yk(kR)), kp)
         call connectdbn(kL, kp, LnL)
         if (jatek == 1) call teklink(LnL, ncoldn)
         kn(3, LnL) = k3
         call connectdbn(kp, kR, LnR)
         if (jatek == 1) call teklink(LnR, ncoldn)
         kn(3, LnR) = k3
         if (jatek == 1) call dcirr(xk(kp), yk(kp), zk(kp), ncoldn)
      else ! swap orientation: switch new links LnL and LnR
         idum = LnL
         LnL = LnR
         LnR = idum
      end if

!     make new links
      kLL = kn(1, LL) + kn(2, LL) - kL
      kRR = kn(1, LR) + kn(2, LR) - kR

      call connectdbn(kLL, kp, LnLL)
      kn(3, LnLL) = kn(3, L)
      if (jatek == 1) call teklink(LnLL, ncoldn)

      if (kLL /= kRR) then
         call connectdbn(kRR, kp, LnRR)
         kn(3, LnRR) = kn(3, L)
         if (jatek == 1) call teklink(LnRR, ncoldn)
      else
         LnRR = LnLL
      end if

!     remove link from original cell, delete two nodes, add one new node and replace two links
      call del_intarrayelem(netcell(ic1)%N, netcell(ic1)%lin, L)
      call del_intarrayelem(netcell(ic1)%N, netcell(ic1)%nod, kL)
      call replace_intarrayelem(netcell(ic1)%N - 1, netcell(ic1)%nod, kR, 1, (/kp/))
      call replace_intarrayelem(netcell(ic1)%N - 1, netcell(ic1)%lin, LL, 1, (/LnLL/))
      call replace_intarrayelem(netcell(ic1)%N - 1, netcell(ic1)%lin, LR, 1, (/LnRR/))
      netcell(ic1)%N = netcell(ic1)%N - 1

!     make new cells
      call makecell(3, (/kLL, kL, kp/), (/LL, LnL, LnLL/), icLL, ierror)
      call makecell(3, (/kR, kRR, kp/), (/LnR, LR, LnRR/), icRR, ierror)
      if (ierror /= 0) goto 1234

!     set lnn and lne for new links
!     reallocate if necessary
      if (numL > ubound(lnn, 1)) then
         numnew = ceiling(1.2d0 * dble(numL))
         call realloc(lnn, numnew, keepExisting=.true.)
         call realloc(lne, (/2, numnew/), keepExisting=.true.)
      end if
      if (i == 1) then
         lnn(LnL) = lnn(L)
         lnn(LnR) = lnn(L)
         lne(1, LnL) = icLL
         lne(1, LnR) = icRR
      else
         lne(2, LnL) = icLL
         lne(2, LnR) = icRR
      end if

      if (netcell(ic1)%N > 2) then
         lnn(LnLL) = 2
         lne(1, LnLL) = icLL
         lne(2, LnLL) = ic1

         lnn(LnRR) = 2
         lne(1, LnRR) = icRR
         lne(2, LnRR) = ic1
      else
         lnn(LnLL) = 2
         lne(1, LnLL) = icLL
         lne(2, LnLL) = icRR

         lnn(LnRR) = 2
         lne(1, LnRR) = icRR
         lne(2, LnRR) = icLL
      end if

!     update lne for old links
      if (lne(1, LL) == ic1) then
         lne(1, LL) = icLL
      else if (lnn(LL) > 1) then
         lne(2, LL) = icLL
      end if

      if (lne(1, LR) == ic1) then
         lne(1, LR) = icRR
      else if (lnn(LR) > 1) then
         lne(2, LR) = icRR
      end if

!     compute cell centers, etcetera (may be needed for plotting)
      if (icL > 0) then
         call getcellweightedcenter(icL, xz(icL), yz(icL), zzz)
         call getcellsurface(icL, ba(icL), xzw(icL), yzw(icL))
      end if
      if (icR > 0) then
         call getcellweightedcenter(icR, xz(icR), yz(icR), zzz)
         call getcellsurface(icR, ba(icR), xzw(icR), yzw(icR))
      end if
      if (icLL > 0) then
         call getcellweightedcenter(icLL, xz(icLL), yz(icLL), zzz)
         call getcellsurface(icLL, ba(icLL), xzw(icLL), yzw(icLL))
      end if
      if (icRR > 0) then
         call getcellweightedcenter(icRR, xz(icRR), yz(icRR), zzz)
         call getcellsurface(icRR, ba(icRR), xzw(icRR), yzw(icRR))
      end if

!     merge triangular cells
!      if ( netcell(ic1)%N.lt.3 ) then
!         if ( icL.gt.0 ) then
!            if ( netcell(icL)%N.eq.3 .and. netcell(icLL)%N.eq.3 ) then
!               call mergecells(icL, icLL,jatek)
!            end if
!         end if
!         if ( icR.gt.0 ) then
!            if ( netcell(icR)%N.eq.3 .and. netcell(icRR)%N.eq.3 ) then
!               call mergecells(icR, icRR,jatek)
!            end if
!         end if
!      end if

!      if ( netcell(ic1)%N.lt.3 .and. icL.gt.0 .and. icR.gt.0 ) then
!         if ( netcell(icL)%N.eq.3 .and. netcell(icLL)%N.eq.3 .and.   &
!              netcell(icR)%N.eq.3 .and. netcell(icRR)%N.eq.3 ) then
!!           only merge cells if the two other links were formed by a split
!            kLLL = sum(netcell(icL)%nod(1:3)) - kL - kLL
!            kRRR = sum(netcell(icR)%nod(1:3)) - kR - kRR
!!           note: kLL equals kRR in this case and should be halfway between kLLL and kRRR
!            if ( dbdistance(0.5d0*(xk(kLLL)+xk(kRRR)), 0.5d0*(yk(kLLL)+yk(kRRR)), &
!                            xk(kLL), yk(kLL)) .lt. 1d-4 ) then
!               call mergecells(icL, icLL,jatek)
!               call mergecells(icR, icRR,jatek)
!            end if
!         end if
!      end if

!!     merge triangular cells
!      if ( netcell(ic1)%N.lt.3 ) then
!         if ( icL.gt.0 ) then
!            if ( netcell(icL)%N.eq.3 .and. netcell(icLL)%N.eq.3 ) then
!               kLLL = sum(netcell(icL)%nod(1:3)) - kL - kLL
!               if ( abs(dcosphi(xk(kLLL), yk(kLLL), xk(kL), yk(kL),  &
!                                xk(kL), yk(kL), xk(kp), yk(kp))).lt.0.5d0 ) then
!                  call mergecells(icL, icLL,jatek)
!               end if
!            end if
!         end if
!         if ( icR.gt.0 ) then
!            if ( netcell(icR)%N.eq.3 .and. netcell(icRR)%N.eq.3 ) then
!               kRRR = sum(netcell(icR)%nod(1:3)) - kR - kRR
!               if ( abs(dcosphi(xk(kRRR), yk(kRRR), xk(kR), yk(kR),  &
!                                xk(kR), yk(kR), xk(kp), yk(kp))).lt.1.5d0 ) then
!                  call mergecells(icR, icRR,jatek)
!               end if
!            end if
!         end if
!      end if

!!     merge triangular cells
!      if ( netcell(ic1)%N.lt.3 ) then
!         if ( icL.gt.0 .and. icR.gt.0 ) then
!            if ( netcell(icL)%N.eq.3 .and. netcell(icLL)%N.eq.3 .and.   &
!                 netcell(icR)%N.eq.3 .and. netcell(icRR)%N.eq.3 ) then
!               kLLL = sum(netcell(icL)%nod(1:3)) - kL - kLL
!               kRRR = sum(netcell(icR)%nod(1:3)) - kR - kRR
!
!               if ( abs(dcosphi(xk(kLLL), yk(kLLL), xk(kL), yk(kL),  &
!                                xk(kRRR), yk(kRRR), xk(kR), yk(kR))).gt.0.95d0 ) then
!                  call mergecells(icL, icLL,jatek)
!                  call mergecells(icR, icRR,jatek)
!               end if
!            end if
!         end if
!      end if

!     merge triangular cells in parallelograms
!       dcos1 and dcos2: cosine of angel between parallel edges ('paralleliness')
!       dcos3: a cosine of angle between two adjacent edges ('skewness')
      if (netcell(ic1)%N < 3) then
         if (icL > 0) then
            if (netcell(icL)%N == 3 .and. netcell(icLL)%N == 3) then
               kLLL = sum(netcell(icL)%nod(1:3)) - kL - kLL
               dcos1 = dcosphi(xk(kLLL), yk(kLLL), xk(kL), yk(kL), &
                               xk(kLL), yk(kLL), xk(kp), yk(kp), jsferic, jasfer3D, dxymis)
               dcos2 = dcosphi(xk(kLLL), yk(kLLL), xk(kLL), yk(kLL), &
                               xk(kL), yk(kL), xk(kp), yk(kp), jsferic, jasfer3D, dxymis)
               dcos3 = dcosphi(xk(kLLL), yk(kLLL), xk(kLL), yk(kLL), &
                               xk(kLL), yk(kLL), xk(kp), yk(kp), jsferic, jasfer3D, dxymis)
               if (abs(dcos1) > DCOSMIN .and. abs(dcos2) > DCOSMIN .and. dcos3 > -0.9d0) then
                  call mergecells(icL, icLL, jatek)
               end if
            end if
         end if
         if (icR > 0) then
            if (netcell(icR)%N == 3 .and. netcell(icRR)%N == 3) then
               kRRR = sum(netcell(icR)%nod(1:3)) - kR - kRR
               dcos1 = dcosphi(xk(kRRR), yk(kRRR), xk(kR), yk(kR), &
                               xk(kRR), yk(kRR), xk(kp), yk(kp), jsferic, jasfer3D, dxymis)
               dcos2 = dcosphi(xk(kRRR), yk(kRRR), xk(kRR), yk(kRR), &
                               xk(kR), yk(kR), xk(kp), yk(kp), jsferic, jasfer3D, dxymis)
               dcos3 = dcosphi(xk(kRRR), yk(kRRR), xk(kRR), yk(kRR), &
                               xk(kRR), yk(kRR), xk(kp), yk(kp), jsferic, jasfer3D, dxymis)
               if (abs(dcos1) > DCOSMIN .and. abs(dcos2) > DCOSMIN .and. dcos3 > -0.9d0) then
                  call mergecells(icR, icRR, jatek)
               end if
            end if
         end if
      end if

   end do

!  delete link
   call dellink(L)

   ierror = 0

!  error handling
1234 continue

   return

contains

!> delete an element from an allocatable integer array
   subroutine del_intarrayelem(N, ia, iy)
      use m_alloc

      implicit none

      integer, intent(in) :: N !< array size
      integer, allocatable, dimension(:), intent(inout) :: ia !< (allocatable) array
      integer, intent(in) :: iy !< element to be deleted from array

      integer, dimension(N) :: idum
      integer :: k, knew

      knew = 0
      do k = 1, N
         if (ia(k) /= iy) then
            knew = knew + 1
            idum(knew) = ia(k)
         end if
      end do

      call realloc(ia, knew)
      ia(1:knew) = idum(1:knew)

      return
   end subroutine del_intarrayelem

!> replace an element from an allocatable integer array by another integer array
   subroutine replace_intarrayelem(N, ia, iy, Nrep, iarep)
      use m_alloc

      implicit none

      integer, intent(in) :: N !< array size
      integer, allocatable, dimension(:), intent(inout) :: ia !< (allocatable) array
      integer, intent(in) :: iy !< element to be replaced in array
      integer, intent(in) :: Nrep !< array size
      integer, dimension(Nrep), intent(in) :: iarep !< array to be inserted

      integer, dimension(N + Nrep - 1) :: idum
      integer :: k, knew

      logical :: Ldone

      Ldone = .false.
      knew = 0
      do k = 1, N
         if (ia(k) /= iy) then
            knew = knew + 1
            idum(knew) = ia(k)
         else if (.not. Ldone) then
            idum(knew + 1:knew + Nrep) = iarep
            knew = knew + Nrep
            Ldone = .true. ! safety
         end if
      end do

      call realloc(ia, knew)
      ia(1:knew) = idum(1:knew)

      return
   end subroutine replace_intarrayelem
end subroutine splitlink
