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
module m_crspath_on_flowgeom
   implicit none
contains
!> Constructs the set of crossed flow links for a single path on the
!! current *flow* geometry.
!!
!! Input is a path with path coordinates in xp,yp.
!! Output path contains additional link numbers in ln and edge
!! coordinates in xk,yk.
!!
!! \see crspath_on_netgeom, crosssections_on_flowgeom, fixedweirs_on_flowgeom
subroutine crspath_on_flowgeom(path, includeghosts, jalinklist, numlinks, linklist, jaloc3, zork)
   use m_crspath
   use m_flowgeom
   use network_data
   use m_sferic
   use m_partitioninfo
   use stdlib_sorting, only: sort_index
   use geometry_module, only: dbdistance, normalout
   use m_missing, only: dmiss, dxymis
   use m_alloc

   type(tcrspath), intent(inout) :: path !< Cross section path that must be imposed on flow geometry.
   integer, intent(in) :: includeghosts !< include ghost links in path (1) or not (0)
   integer, intent(in) :: jalinklist !< use link list (1) or not (0)
   integer, intent(in) :: numlinks !< number of links in list
   integer, dimension(numlinks), intent(in) :: linklist !< list of flowlinks crossed by path
   integer, intent(in) :: jaloc3 !< If it has locationtype==3, then jaloc3>0, for Crs defined by branchID and chainage
   integer, intent(in) :: zork !< xk, yk for crossing or xz, yz

   integer :: i, iend, iLf, L, Lf, n1, n2, kint

   integer :: jaghost, idmn_ghost

   double precision :: x1, y1, x2, y2, xn, yn

   double precision, allocatable :: dpl(:)

   path%lnx = 0 ! Reset link administration for this path.

   kint = max(lnx / 1000, 1)

   if (jalinklist == 0) then
      iend = Lnx
   else
      iend = numlinks
   end if

!   Loop across all flow links
   do iLf = 1, iend
      if (jalinklist == 0) then
         Lf = iLf
      else
         Lf = linklist(iLf)
      end if

      n1 = ln(1, Lf); n2 = ln(2, Lf)
      L = ln2lne(Lf)
      if (n1 <= 0 .or. n2 <= 0 .or. L <= 0 .or. &
          n1 > ndx .or. n2 > ndx .or. L > numl) then
         cycle
      end if

      if (jampi == 1) then
         if (includeghosts /= 1) then
!             exclude ghost links
            call link_ghostdata(my_rank, idomain(ln(1, Lf)), idomain(ln(2, Lf)), jaghost, idmn_ghost, ighostlev(ln(1, Lf)), ighostlev(ln(2, Lf)))
            if (jaghost == 1) cycle
         end if
      end if

      if (abs(kcu(Lf)) == 2) then !   For 2D links: take net nodes of crossed net link.
         x1 = xk(lncn(1, Lf))
         y1 = yk(lncn(1, Lf))
         x2 = xk(lncn(2, Lf))
         y2 = yk(lncn(2, Lf))
      else !   For 1D links: produce fictious 'cross/netlink'
         call normalout(xz(n1), yz(n1), xz(n2), yz(n2), xn, yn, jsferic, jasfer3D, dmiss, dxymis)
         xn = -xn; yn = -yn ! flow link should be perpendicular to 'crs', and not vice versa.
         xn = wu(Lf) * xn; yn = wu(Lf) * yn
         if (jsferic == 1) then
            xn = rd2dg * xn / ra
            yn = rd2dg * yn / ra
         end if

         x1 = .5d0 * (xz(n1) + xz(n2)) - .5d0 * xn
         y1 = .5d0 * (yz(n1) + yz(n2)) - .5d0 * yn
         x2 = .5d0 * (xz(n1) + xz(n2)) + .5d0 * xn
         y2 = .5d0 * (yz(n1) + yz(n2)) + .5d0 * yn
      end if
      if (jaloc3 > 0) then ! for Crs defined by branchID and chainage
         call increaseCrossSectionPath(path, 0, 1)
         path%xk(1, 1) = x1
         path%yk(1, 1) = y1
         path%xk(2, 1) = x2
         path%yk(2, 1) = y2
         path%lnx = 1
         path%ln(1) = Lf
      else
         call crspath_on_singlelink(path, Lf, x1, y1, x2, y2, xz(n1), yz(n1), xz(n2), yz(n2), zork)
      end if
   end do

   if (path%lnx > 0) then
      if (jaloc3 == 0) then

         !  determine permutation array of flowlinks by increasing arc length order
         do i = 1, path%lnx
            path%sp(i) = dble(path%indexp(i)) + (1d0 - path%wfp(i))
         end do

         call sort_index(path%sp(1:path%lnx), path%iperm(1:path%lnx))

         !  compute arc length
         allocate (dpl(path%np))
         dpl(1) = 0d0
         do i = 2, path%np
            dpl(i) = dpl(i - 1) + dbdistance(path%xp(i - 1), path%yp(i - 1), path%xp(i), path%yp(i), jsferic, jasfer3D, dmiss)
         end do

         do i = 1, path%lnx
            path%sp(i) = dpl(path%indexp(i)) * path%wfp(i) + &
                         dpl(path%indexp(i) + 1) * (1d0 - path%wfp(i))
         end do

         deallocate (dpl)
      else
         ! only 1 link
         call realloc(path%iperm, 1, fill=1, keepExisting=.false.)
      end if
   end if

end subroutine crspath_on_flowgeom
end module m_crspath_on_flowgeom
