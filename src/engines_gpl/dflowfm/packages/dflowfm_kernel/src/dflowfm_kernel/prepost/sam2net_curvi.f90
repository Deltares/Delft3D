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

!> interpolation of sample data to network nodes, in curvilinear grid coordinates
subroutine sam2net_curvi(numk, xk, yk, zk)
   use precision, only: dp
   use m_disable_outside_curvigrid, only: disable_outside_curvigrid
   use m_confrm
   use m_grid
   use m_samples
   use m_alloc
   use m_missing, only: dmiss, jins
   use m_polygon
   use m_ec_basic_interpolation, only: triintfast, bilin_interp_loc
   use fm_external_forcings_data, only: transformcoef
   use m_sferic, only: jsferic, jasfer3D
   use m_qnerror
   use m_delpol
   use m_restore_grd

   implicit none

   integer, intent(in) :: numk !< number of netnodes
   real(kind=dp), dimension(numk), intent(in) :: xk, yk !< netnode coordinates
   real(kind=dp), dimension(numk), intent(inout) :: zk !< netnode z-values

   real(kind=dp), dimension(:, :), allocatable :: xietak ! network grid-coordinates, dim(2,numk)
   real(kind=dp), dimension(:, :), allocatable :: xietas ! sample grid-coordinates, dim(2,NS)
   real(kind=dp), dimension(:, :), allocatable :: xietac ! grid grid-coordinates, dim(2,mc*nc)

   real(kind=dp), dimension(:), allocatable :: xik, etak ! network grid-coordinates, dim(numk)
   real(kind=dp), dimension(:), allocatable :: xis, etas ! network grid-coordinates, dim(NS)
   integer, dimension(:), allocatable :: imaskk ! network inside curvigrid (1) or not (0)
   integer, dimension(:), allocatable :: imasks ! sample  inside curvigrid (1) or not (0)

   real(kind=dp) :: xiloc, etaloc, zloc(1, 1, 1), etamin, etamax

   integer :: ierror
   integer :: i, ipoint, j, ja, jadl, jakdtree, k

   logical :: Ldeletedpol
   logical :: L1D

   ierror = 1

   Ldeletedpol = .false.
   L1D = .false.

   if (mc * nc == 0) then
      call qnerror('no curvilinear grid available', ' ', ' ')
      goto 1234
   end if

   ja = 0
   call confrm('1D interpolation (no cross-sections)?', ja)
   if (ja == 1) L1D = .true.

!  regularize the curvigrid
   call regularise_spline2curvigrid()

!  allocate
   allocate (xietak(2, numk))
   allocate (xietas(2, NS))
   allocate (xietac(2, mmax * nmax)) ! should correspond with xc,yc,zc array sizes
   allocate (xik(numk), etak(numk))
   allocate (xis(NS), etas(NS))
   allocate (imaskk(numk), imasks(NS))

   xietak = DMISS
   xietas = DMISS
   xietac = DMISS

!  find nodes/samples inside curvigrid
   call disable_outside_curvigrid(numk, NS, xk, yk, xs, ys, imaskk, imasks)

!  assign (xi,eta) to the grid nodes
   etamin = huge(1d0)
   etamax = -etamin
   do i = 1, mc
      xiloc = dble(i - 1)
      do j = 1, nc
         ipoint = i + mmax * (j - 1)
         etaloc = dble(j - 1)

         xietac(1, ipoint) = xiloc
         xietac(2, ipoint) = etaloc

         etamax = max(etaloc, etamax)
         etamin = min(etaloc, etamin)
      end do
   end do

! find sample grid-coordinates
   jadl = 0
   jakdtree = 1

   call TRIINTfast(xc, yc, xietac, mmax * nmax, 2, xs, ys, xietas, NS, jadl, jakdtree, &
                   jsferic, NPL, jins, dmiss, jasfer3D, XPL, YPL, ZPL, transformcoef) ! will alter grid

   do i = 1, NS
!     apply inside-curvigrid mask
      if (imasks(i) /= 1) then
         xietas(1, i) = DMISS
         xietas(2, i) = DMISS
      end if

      if (i == 16355) then
         continue
      end if
      xiloc = xietas(1, i)
      etaloc = xietas(2, i)

      if (xiloc == DMISS .or. etaloc == DMISS) cycle ! no (xi,eta) found

!     note that current xiloc and etaloc serve as first iterate in call to bilin_interp_loc
      call bilin_interp_loc(mmax, nmax, mc, nc, 1, xc, yc, zc, xs(i), ys(i), xiloc, etaloc, zloc, ierror, dmiss, jsferic)
      if (ierror == 0) then
         xietas(1, i) = xiloc
         xietas(2, i) = etaloc
      end if
   end do

!  find network grid-coordinates
   jadl = 0
   jakdtree = 1
   if (NPL > 0) call savegrd()

   call TRIINTfast(xc, yc, xietac, mmax * nmax, 2, xk, yk, xietak, numk, jadl, jakdtree, &
                   jsferic, NPL, jins, dmiss, jasfer3D, XPL, YPL, ZPL, transformcoef)

   if (NPL > 0) call restoregrd()

   do k = 1, numk
!     apply inside-curvigrid mask
      if (imaskk(k) /= 1) then
         xietak(1, k) = DMISS
         xietak(2, k) = DMISS
      end if

      if (k == 603) then
         continue
      end if

      xiloc = xietak(1, k)
      etaloc = xietak(2, k)

      if (xiloc == DMISS .or. etaloc == DMISS) cycle ! no (xi,eta) found

!     note that current xiloc and etaloc serve as first iterate in call to bilin_interp_loc
      call bilin_interp_loc(mmax, nmax, mc, nc, 1, xc, yc, zc, xk(k), yk(k), xiloc, etaloc, zloc, ierror, dmiss, jsferic)
      if (ierror == 0) then
         xietak(1, k) = xiloc
         xietak(2, k) = etaloc
      end if
   end do

!!  DEBUG
!      do i=1,NS
!         zs(i) = xietas(1,i)
!!         zs(i) = xietas(2,i)
!      end do
!
!      do k=1,numk
!         zk(k) = xietak(1,k)
!!         zk(k) = xietak(2,k)
!      end do
!
!      goto 1234
!!  END DEBUG

   do i = 1, NS
      xis(i) = xietas(1, i)
      etas(i) = xietas(2, i)
   end do

   do k = 1, numk
      xik(k) = xietak(1, k)
      etak(k) = xietak(2, k)
   end do

   if (L1D) then
!     1D interpolation: move samples to etamin and copy to etamax
      call realloc(xis, 2 * NS, keepExisting=.true.)
      call realloc(etas, 2 * NS, keepExisting=.true.)
      call realloc(zs, 2 * NS, keepExisting=.true.)
      do i = 1, NS
         xiloc = xis(i)
         etas(i) = etamin
         xis(NS + i) = xiloc
         etas(NS + i) = etamax
         zs(NS + i) = zs(i)
      end do
      NS = 2 * NS
   end if

!  delete polygon temporarily for (xi,eta) interpolation
   call savepol()
   call delpol()
   Ldeletedpol = .true.

   jadl = 0
   jakdtree = 1 ! todo :
   call triintfast(xis, etas, zs, NS, 1, xik, etak, zk, numk, jadl, jakdtree, &
                   jsferic, NPL, jins, dmiss, jasfer3D, XPL, YPL, ZPL, transformcoef)

   ierror = 0

!  error handling
1234 continue

!  restore
   if (Ldeletedpol) then
      call restorepol()
   end if

   if (L1D) then
      NS = NS / 3
      call realloc(zs, NS, keepExisting=.true.)
   end if

!  deallocate
   if (allocated(xietak)) deallocate (xietak)
   if (allocated(xietas)) deallocate (xietas)
   if (allocated(xietac)) deallocate (xietac)
   if (allocated(xik)) deallocate (xik)
   if (allocated(etak)) deallocate (etak)
   if (allocated(xis)) deallocate (xis)
   if (allocated(etas)) deallocate (etas)
   if (allocated(imaskk)) deallocate (imaskk)
   if (allocated(imasks)) deallocate (imasks)

   return
end subroutine sam2net_curvi
