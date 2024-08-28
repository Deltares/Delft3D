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

!>  grow gridlayers from a net boundary
subroutine netboundtocurvi(kp)
   use m_polygon
   use m_grid
   use m_gridsettings
   use m_missing
   use m_spline2curvi
   use m_netw
   use geometry_module, only: dbdistance, dprodout
   use m_sferic, only: jsferic, jasfer3D
   use gridoperations

   implicit none

   integer, intent(in) :: kp !< clicked node

   double precision, dimension(:), allocatable :: edgevel

   integer, dimension(:), allocatable :: ifront

   double precision :: dt, dwidthloc

   double precision :: crs, dis, xn, yn, rL

   integer :: i, ic, j, jc, k1, k2, k3, L, Lloc, kother
   integer :: istop, ierror, jacancelled
   integer :: ja
   integer :: iorient, iorient_new !  orientation of boundary (0: left, 1:right, -1:undetermined)

!   integer                                     :: NFAC_bak

   ierror = 1

!  store settings
!   nfac_bak = nfac

!  set default
!   nfac = 1

   if (netstat /= netstat_OK) call findcells(0)
   call netboundtopoly_makemasks()

   if (kc(kp) /= 1) goto 1234 ! invalid point

   call savepol()
   call delpol()
   call netboundtopoly(kp)

!   goto 1234

!  get the settings from a parameter menu, if user presses 'Esc', do nothing.
   jacancelled = 0
   call change_spline2curvi_param(jacancelled)
   if (jacancelled == 1) then
      return
   end if

   mc = NPL
   nc = nfac + 1

   if (mc < 2) goto 1234 ! no curvigrid

   call savegrd()
   call increasegrid(mc, nc)
   xc = DMISS
   yc = DMISS

!  check orientation of polygon
   iorient = -1
   do i = 1, NPL - 1
      k1 = int(zpl(i))
      k2 = int(zpl(i + 1))

      if (k1 < 1 .or. k2 < 1) cycle ! no netnodes found

!     determine the link
      L = 0
      do j = 1, nmk(k1)
         Lloc = nod(k1)%lin(j)
         if (kn(3, Lloc) /= 2) cycle ! not a 2D link
         kother = kn(1, Lloc) + kn(2, Lloc) - k1
         if (kother == k2) then
            L = Lloc
            exit
         end if
      end do
      if (L == 0) cycle ! no link found
      if (lnn(L) /= 1) cycle ! not a boundary link

!     determine the adjacent net cell
      ic = lne(1, L)

!     determine orientation
      crs = dprodout(xpl(i), ypl(i), xpl(i + 1), ypl(i + 1), xpl(i), ypl(i), xzw(ic), yzw(ic), jsferic, jasfer3D)
      iorient_new = -1
      if (crs > 0d0) then
         iorient_new = 1
      else if (crs < 0d0) then
         iorient_new = 0
      end if

      if (iorient == -1) then
         iorient = iorient_new
      else
!        compare
         if (iorient /= iorient_new) then
            call qnerror('pol2curvi: orientation error', ' ', ' ')
            goto 1234
         end if
      end if
   end do
!  swith orientation if necessary
   if (iorient /= 0) call flippo(0)

!  copy polygon to first gridline
   jc = 1
   xc(1:NPL, jc) = xpl(1:NPL)
   yc(1:NPL, jc) = ypl(1:NPL)

!  check for circular connectivity
   if (dbdistance(xc(1, jc), yc(1, jc), xc(NPL, jc), yc(NPL, jc), jsferic, jasfer3D, dmiss) <= dtolLR) then
      jacirc = 1
   else
      jacirc = 0
   end if

!  allocate
   if (allocated(edgevel)) deallocate (edgevel)
   allocate (edgevel(mc - 1))
   if (allocated(ifront)) deallocate (ifront)
   allocate (ifront(mc))

!  set the front mask
   ifront = 1
   where (xc(:, jc) == DMISS) ifront = 0

!  set edge velocity
   edgevel = DMISS

   if (dunigridsize <= 0d0) then
      do i = 1, mc - 1
         !     get the pointers to the netnodes from zpl
         k1 = int(zpl(i))
         k2 = int(zpl(i + 1))

         if (k1 < 1 .or. k2 < 1) cycle ! no netnodes found

         !     determine the link
         L = 0
         do j = 1, nmk(k1)
            Lloc = nod(k1)%lin(j)
            if (kn(3, Lloc) /= 2) cycle ! not a 2D link
            kother = kn(1, Lloc) + kn(2, Lloc) - k1
            if (kother == k2) then
               L = Lloc
               exit
            end if
         end do
         if (L == 0) cycle ! no link found
         if (lnn(L) /= 1) cycle ! not a boundary link

         !     determine the adjacent net cell
         ic = lne(1, L)

         dwidthloc = 0d0
         !     determine cell height: take maximum distance to boundary link
         do j = 1, netcell(ic)%N
            k3 = netcell(ic)%nod(j)
            call dlinedis2(xk(k3), yk(k3), xk(k1), yk(k1), xk(k2), yk(k2), ja, dis, xn, yn, rL)
            dwidthloc = max(dwidthloc, dis)
         end do
         edgevel(i) = dgrow * dwidthloc
      end do
   else ! user specified
      edgevel = dunigridsize
   end if

!  update the front
   do i = 1, mc - 1
      if (edgevel(i) == DMISS) then
         ifront(i) = 0
         ifront(i + 1) = 0
      end if
   end do

!  grow the grid
   dt = 1d0
   do j = jc + 1, nc
!      idum = 1
!      call plot(idum)
      call growlayer(mc, nc, mmax, nmax, 1, j, edgevel, dt, xc, yc, ifront, istop)

!     update edge velocity
      do i = 1, mc - 1
         edgevel(i) = dgrow * edgevel(i)
      end do

      if (dt < 1d-8 .or. istop == 1) exit
   end do

   ierror = 0
1234 continue

   call restorepol()

!  deallocate
   if (allocated(edgevel)) deallocate (edgevel)
   if (allocated(ifront)) deallocate (ifront)

!   call netboundstopoly_deallocatemasks()

!  restore settings
!   nfac = nfac_bak

   return
end subroutine netboundtocurvi
