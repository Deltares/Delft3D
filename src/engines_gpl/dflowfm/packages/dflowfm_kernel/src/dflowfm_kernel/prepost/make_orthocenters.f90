!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

! compose an orthogonal dual mesh (cell centers), while keeping the primary mesh (net nodes) fixed
module m_make_orthocenters

   implicit none

   private

   public :: make_orthocenters

contains

   subroutine make_orthocenters(dmaxnonortho, maxiter)
      use precision, only: dp
      use m_comp_circumcenter, only: comp_circumcenter
      use m_halt3
      use m_netw
      use m_flowgeom, only: xz, yz
      use unstruc_display, only: ncolhl
      use geometry_module, only: dcosphi
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: dxymis
      use gridoperations
      use m_readyy
      use m_qnerror
      use m_cirr

      real(kind=dp), intent(in) :: dmaxnonortho !< maximum allowed non-orthogonality
      integer, intent(in) :: maxiter !< maximum number of iterations

      integer, parameter :: N6 = 6 ! maximum polygon dimension

      real(kind=dp), dimension(N6) :: xplist, yplist, xflist, yflist

      real(kind=dp), dimension(:), allocatable :: xc, yc !< cell centers

      real(kind=dp) :: af, dmaxabscosphi, drmsabscosphi, dabscosphi

      integer :: iter

      integer :: i, ip1, ii, ic, ic1, j, ja3, k, kp1, L, N

      integer :: ierror

      real(kind=dp), parameter :: dsigma = 0.95_dp

      ierror = 1
      ic = 0

      if (nump < 1) goto 1234

      if (netstat /= NETSTAT_OK) call findcells(0)

!  allocate
      allocate (xc(nump), yc(nump))

      call readyy(' ', -1.0_dp)
      call readyy('Computing orthocenters (press right mouse button to cancel)', 0.0_dp)

!  compute the initial cell centers
      do iter = 1, MAXITER
         dmaxabscosphi = 0.0_dp
         drmsabscosphi = 0.0_dp
         do ic = 1, nump
            N = netcell(ic)%N
            if (N > N6) then
               call qnerror('make_orthocenters: N>N6', ' ', ' ')
               goto 1234
            end if

            do i = 1, N
               ip1 = i + 1; if (ip1 > N) ip1 = ip1 - N
               k = netcell(ic)%nod(i)
               kp1 = netcell(ic)%nod(ip1)
               xplist(i) = xk(k)
               yplist(i) = yk(k)
               !        find the link connected to this node
               do j = 0, N - 1
                  ii = i + j; if (ii > N) ii = ii - N
                  L = netcell(ic)%lin(ii)
                  if ((kn(1, L) == k .and. kn(2, L) == kp1) .or. (kn(1, L) == kp1 .and. kn(2, L) == k)) then
                     exit ! found
                  end if
               end do

               if (lnn(L) == 2) then
                  !           internal link
                  ic1 = lne(1, L) + lne(2, L) - ic

                  xflist(i) = xz(ic1)
                  yflist(i) = yz(ic1)
                  dabscosphi = abs(dcosphi(xz(ic), yz(ic), xz(ic1), yz(ic1), xk(k), yk(k), xk(kp1), yk(kp1), jsferic, jasfer3D, dxymis))
                  dmaxabscosphi = max(dmaxabscosphi, dabscosphi)
                  drmsabscosphi = drmsabscosphi + dabscosphi**2
               else
                  !           boundary link
                  xflist(i) = 0.5_dp * (xk(k) + xk(kp1))
                  yflist(i) = 0.5_dp * (yk(k) + yk(kp1))
               end if
            end do

            call comp_circumcenter(N, xplist, yplist, xflist, yflist, xc(ic), yc(ic))

!         call cirr(xc(ic),yc(ic),31)
         end do ! do ic=1,nump

         drmsabscosphi = sqrt(drmsabscosphi / real(max(nump, 1), kind=dp))

!     relaxation
         xz(1:nump) = xz(1:nump) + dsigma * (xc(1:nump) - xz(1:nump))
         yz(1:nump) = yz(1:nump) + dsigma * (yc(1:nump) - yz(1:nump))

!     check residual
         if (drmsabscosphi <= dmaxnonortho) exit

!     output information
         af = real(iter, kind=dp) / real(MAXITER, kind=dp)
         call readyy('Computing orthocenters (press right mouse button to cancel)', af)
         write (6, '("+iter: ", I5, " max ortho: ", E11.4, " rms ortho: ", E11.4)') iter, dmaxabscosphi, drmsabscosphi

!     check for right mouse button
         call halt3(ja3)
         if (ja3 == 3) then
            ierror = 0
            goto 1234
         end if
      end do ! do iter=1,MAXITER

      ierror = 0
1234  continue

      call readyy(' ', -1.0_dp)

      if (ierror /= 0) then
!      call qnerror('make_orthocenters: error', ' ', ' ')
         if (ic > 0 .and. ic < nump) call cirr(xc(ic), yc(ic), ncolhl)
      end if

!  deallocate
      if (allocated(xc)) deallocate (xc, yc)

      return
   end subroutine make_orthocenters

end module m_make_orthocenters
