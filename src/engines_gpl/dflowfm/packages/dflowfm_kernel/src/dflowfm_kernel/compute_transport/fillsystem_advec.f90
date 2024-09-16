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

!> construct matrix for first-order upwind discretization of advection
   subroutine fillsystem_advec(ierror)
      use m_flowgeom
      use m_flow, only: vol1, Au
      use m_advec
      use m_alloc
      use m_qnerror
      use m_writematrix
      implicit none

      integer, intent(inout) :: ierror

!      double precision, dimension(:,:), allocatable :: dfluxfac

      double precision :: dfac
      double precision :: ux, uy

      integer :: i, irow, irowother, idir, ishift
      integer :: ipoint, ipointdiag
      integer :: k1, k2, kk, kkother, LL

      ierror = 1

!      call realloc(dfluxfac, (/ 2, Lnx /), keepExisting=.false., fill=0d0)

      ux = 1d0
      uy = 0.1d0

      do LL = 1, Lnx
         k1 = ln(1, LL)
         k2 = ln(2, LL)
!         dfac = qa(LL)
         dfac = (csu(LL) * ux + snu(LL) * uy) * Au(LL)
         dfluxfac(1, LL) = max(dfac, 0d0)
         dfluxfac(2, LL) = min(dfac, 0d0)
      end do

!     initialize matrix entries
      aC = 0d0

      ipoint = 0
      irow = 0
      do kk = 1, Ndxi
!        x-component
         irow = irow + 1

!        shift between x-component and y-component
         ishift = 1 + nd(kk)%lnx

!        diagonal entry x-component
         ipoint = ipoint + 1
         ipointdiag = ipoint

!        check numbering
         if (jC(ipointdiag) /= irow) then
            call qnerror('fillsystem_advec: numbering error', ' ', ' ')
            goto 1234
         end if

         if (jC(ipointdiag + ishift) /= irow + 1) then
            call qnerror('fillsystem_advec: numbering error', ' ', ' ')
            goto 1234
         end if

!        off-diagonals
         do i = 1, nd(kk)%lnx
            ipoint = ipoint + 1
            LL = nd(kk)%ln(i)

            idir = 0 ! to kk
            if (LL < 0) then
               idir = 1 ! from kk
               LL = -LL
            end if

!           check numbering
            if (jC(ipoint) /= (ln(1, LL) + ln(2, LL) - kk - 1) * 2 + 1) then
               call qnerror('fillsystem_advec: numbering error', ' ', ' ')
               goto 1234
            end if

            if (jC(ipoint + ishift) /= (ln(1, LL) + ln(2, LL) - kk - 1) * 2 + 2) then
               call qnerror('fillsystem_advec: numbering error', ' ', ' ')
            end if

!           x-component
            aC(ipoint) = aC(ipoint) + (idir * dfluxfac(2, LL) - (1 - idir) * dfluxfac(1, LL)) / vol1(kk)
            ac(ipointdiag) = aC(ipointdiag) + (idir * dfluxfac(1, LL) - (1 - idir) * dfluxfac(2, LL)) / vol1(kk)

!           y-component
            aC(ipoint + ishift) = aC(ipoint + ishift) + (idir * dfluxfac(2, LL) - (1 - idir) * dfluxfac(1, LL)) / vol1(kk)
            ac(ipointdiag + ishift) = aC(ipointdiag + ishift) + (idir * dfluxfac(1, LL) - (1 - idir) * dfluxfac(2, LL)) / vol1(kk)
         end do

!        shift to y-component
         ipoint = ipoint + ishift
         irow = irow + 1
      end do

!     boundaries: apply homogeneous Neumann condition (copy from inside)
      do kk = Ndxi + 1, Ndx
         irow = irow + 1
         LL = abs(nd(kk)%ln(1)) ! only one flowlink attached to boundary flownode
         kkother = ln(1, LL) + ln(2, LL) - kk ! ln(2,LL)

!        x-component
         irowother = (kkother - 1) * 2 + 1
         do i = iC(irowother), iC(irowother + 1) - 1
            ipoint = ipoint + 1
            aC(ipoint) = aC(i)
         end do

!        y-component
         irow = irow + 1
         irowother = (kkother - 1) * 2 + 2
         do i = iC(irowother), iC(irowother + 1) - 1
            ipoint = ipoint + 1
            aC(ipoint) = aC(i)
         end do
      end do

      if (jaoutput == 1) then
         call writematrix('C.m', 2 * Ndx, iC, jC, aC, 'A_C', 0)
      end if

      ierror = 0
1234  continue

!      if ( allocated(dfluxfac) ) deallocate(dfluxfac)

      return
   end subroutine fillsystem_advec
