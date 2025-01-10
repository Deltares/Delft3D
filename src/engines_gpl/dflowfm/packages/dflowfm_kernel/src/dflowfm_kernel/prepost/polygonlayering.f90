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

module m_polygonlayering

   implicit none

   private

   public :: polygonlayering

contains

   subroutine polygonlayering(mpol)
      use precision, only: dp
      use m_closedefinedflownode, only: closedefinedflownode
      use m_flow
      use m_flowgeom
      use m_polygon
      use m_samples
      use m_missing
      use m_ec_triangle
      use m_sferic, only: jsferic, jasfer3D
      use m_ec_basic_interpolation, only: TRIINTfast
      use geometry_module
      use m_reapol
      use m_filez, only: error

      integer :: mpol
      integer :: k, j, jstart, jend, ierr, jdla, ipoint, jakdtree, ndim, n, in, nspl, n1
      integer, allocatable :: nds(:), ndn(:)
      real(kind=dp), allocatable :: zz(:)

      call reapol(mpol, 0)

      call increasesam(npl + ndx)

      if (allocated(indlaynod)) deallocate (indlaynod, wflaynod)
      allocate (indlaynod(3, ndxi), stat=ierr); indlaynod = 0
      call aerr('indlaynod(3,ndxi)', ierr, ndxi)
      allocate (wflaynod(3, ndxi), stat=ierr); wflaynod = 0d0
      call aerr(' wflaynod(3,ndxi)', ierr, ndxi)

      allocate (ndn(ndxi + npl), stat=ierr)
      call aerr('ndn(ndxi+npl)', ierr, ndxi + npl); ndn = 0

      allocate (zz(ndxi), stat=ierr)
      call aerr('zz(ndxi)', ierr, ndxi); zz = dmiss

      mxlaydefs = 0; ipoint = 1 ! first count and allocate
      jstart = 0; jend = 0; k = 0
      do while (ipoint <= npl) ! nr of layers in first polygonpoint, layertype in second point
         call get_startend(npl - ipoint + 1, xpl(ipoint:npl), ypl(ipoint:npl), jstart, jend, dmiss)
         jstart = ipoint + jstart - 1
         jend = ipoint + jend - 1
         ipoint = jend + 1
         mxlaydefs = mxlaydefs + 1
      end do
      deallocate (laymx, laytyp)
      allocate (laymx(mxlaydefs), laytyp(mxlaydefs), stat=ierr)
      call aerr('laymx(mxlaydefs), laytyp(mxlaydefs)', ierr, mxlaydefs)

      mxlaydefs = 0; ipoint = 1 ! then fill
      jstart = 0; jend = 0; k = 0

      do while (ipoint <= npl) ! nr of layers in first polygonpoint, layertype in second point

         call get_startend(npl - ipoint + 1, xpl(ipoint:npl), ypl(ipoint:npl), jstart, jend, dmiss)
         jstart = ipoint + jstart - 1
         jend = ipoint + jend - 1
         ipoint = jend + 1

         mxlaydefs = mxlaydefs + 1

         if (zpl(jstart) > kmx) then
            call error('increase kmx to allow for nr of layers specified in vertical_layering.pliz', ' ', ' ')
         end if

         laymx(mxlaydefs) = zpl(jstart) ! first point  = nr of layers
         laytyp(mxlaydefs) = zpl(jstart + 1) ! second point = type
         zpl(jstart:jend) = mxlaydefs ! now only point to laydef nr

         do j = jstart, jend ! add to sample set
            k = k + 1
            xs(k) = xpl(j)
            ys(k) = ypl(j)
            zs(k) = mxlaydefs
         end do

      end do
      nspl = k

      laydefnr = 0
      in = -1
      do n = 1, ndx ! add flownodes in polygon/laydef nr in to samples
         call inwhichpolygon(xz(n), yz(n), in)
         if (in > 0) then
            k = k + 1
            xs(k) = xz(n); ys(k) = yz(n); zs(k) = in; laydefnr(n) = in; ndn(k) = n
         end if
      end do
      ns = k

      jdla = 1; jakdtree = 1; ndim = 1

      jagetwf = 1
      allocate (indxx(3, ndxi), wfxx(3, ndxi)) ! if module variable jagetw == 1, make weightfactor_index arrays

      call TRIINTfast(XS, YS, ZS, NS, NDIM, Xz, Yz, Zz, ndxi, JDLA, jakdtree, jsferic, 0, jins, dmiss, jasfer3D, &
                      Xpl, Ypl, ZPL, transformcoef) !

      allocate (nds(nspl))
      do j = 1, nspl
         call CLOSEdefinedflownode(Xs(j), Ys(j), N1)
         if (n1 == 0) then
            nds(j) = 0
         else
            nds(j) = n1
         end if
      end do

      do n = 1, ndx ! refer back to flownode instead of polygonpoint
         if (laydefnr(n) == 0) then
            do k = 1, 3
               if (indxx(k, n) <= nspl) then
                  indlaynod(k, n) = nds(indxx(k, n))
                  wflaynod(k, n) = wfxx(k, n)
               else
                  indlaynod(k, n) = ndn(indxx(k, n))
                  wflaynod(k, n) = wfxx(k, n)
               end if
            end do
         end if
      end do

      ns = 0; npl = 0
      deallocate (indxx, wfxx, zz, nds, ndn, iistart, iiend)

   end subroutine polygonlayering

end module m_polygonlayering
