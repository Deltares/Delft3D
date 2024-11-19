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
!> initialize matrix for first-order upwind discretization of advection
!>   Cartesian cell-centered vector components are numbered as
!>      ( ucx_1, ucy_1, ucx_2, ucy_2, ..., ...)
module m_inisolver_advec
use m_solversettings, only: solversettings


implicit none

private

public :: inisolver_advec

contains

   subroutine inisolver_advec(ierror)
      use m_flowgeom
      use m_alloc
      use m_solver
      use m_advec_data
      use m_writematrix
      use m_saadf, only: amub
      implicit none

      integer, intent(inout) :: ierror

      integer :: irow, irowother, icolumn, ipoint
      integer :: kk, kkb, kkother, k1, k2, LL
      integer :: i, ishift
      integer :: numnonzeros

      ierror = 1

!     construct interpolation matrix in CRS format
      numnonzeros = 4 * Lnx
      call realloc(iI, Lnx + 1, keepExisting=.false., fill=0)
      call realloc(jI, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aI, numnonzeros, keepExisting=.false., fill=0d0)

      ipoint = 0
      irow = 0
      do LL = 1, Lnx
         irow = irow + 1
         iI(irow) = ipoint + 1

         k1 = ln(1, LL)
         k2 = ln(2, LL)

!        x-component first flownode
         ipoint = ipoint + 1
         jI(ipoint) = (k1 - 1) * 2 + 1
         aI(ipoint) = acL(LL) * csu(LL)

!        y-component first flownode
         ipoint = ipoint + 1
         jI(ipoint) = (k1 - 1) * 2 + 2
         aI(ipoint) = acL(LL) * snu(LL)

!        x-component second flownode
         ipoint = ipoint + 1
         jI(ipoint) = (k2 - 1) * 2 + 1
         aI(ipoint) = (1d0 - acL(LL)) * csu(LL)

!        y-component second flownode
         ipoint = ipoint + 1
         jI(ipoint) = (k2 - 1) * 2 + 2
         aI(ipoint) = (1d0 - acL(LL)) * snu(LL)
      end do
      iI(irow + 1) = ipoint + 1

!     construct reconstruction matrix in CRS format

!     count number of non-zeros
      numnonzeros = 0
      do kk = 1, Ndxi
         numnonzeros = numnonzeros + 2 * nd(kk)%lnx
      end do
      do kkb = Ndxi + 1, Ndx ! boundaries: apply Neumann conditions (copy from inside)
         LL = abs(nd(kkb)%ln(1)) ! only one flowlink connected to boundary flownode
         kk = ln(1, LL) + ln(2, LL) - kkb
         numnonzeros = numnonzeros + 2 * nd(kk)%lnx
      end do

!     allocate
      call realloc(iR, 2 * Ndx + 1, keepExisting=.false., fill=0)
      call realloc(jR, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aR, numnonzeros, keepExisting=.false., fill=0d0)

!     fill entries
      ipoint = 0
      irow = 0
      do kkb = 1, Ndx
         if (kkb <= Ndxi) then
            kk = kkb
         else ! boundaries: apply homogeneous Neumann condition (copy from inside)
            LL = abs(nd(kkb)%ln(1)) ! only one flowlink connected to boundary flownode
            kk = ln(1, LL) + ln(2, LL) - kkb
         end if

!        x-component
         irow = irow + 1
         iR(irow) = ipoint + 1

!        shift between x-component and y-component
         ishift = nd(kk)%lnx

!        y-component (next row)
         iR(irow + 1) = iR(irow) + ishift

         do i = 1, nd(kk)%lnx
            LL = abs(nd(kk)%ln(i))

!           x-component
            ipoint = ipoint + 1
            jR(ipoint) = LL

!           y-component (next row)
            jR(ipoint + ishift) = LL

            if (ln(1, LL) == kk) then
               aR(ipoint) = wcx1(LL)
               aR(ipoint + ishift) = wcy1(LL)
            else
               aR(ipoint) = wcx2(LL)
               aR(ipoint + ishift) = wcy2(LL)
            end if
         end do

!        shift to y-component
         ipoint = ipoint + ishift
         irow = irow + 1
      end do

      iR(irow + 1) = ipoint + 1

!     construct collocated discretization matrix

!     count number of non-zeros: use reconstruction matrix
      numnonzeros = 2 * Ndx + (iR(2 * Ndx + 1) - 1)

!     allocate
      call realloc(iC, 2 * Ndx + 1, keepExisting=.false., fill=0)
      call realloc(jC, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aC, numnonzeros, keepExisting=.false., fill=0d0)
      call realloc(dfluxfac, (/2, Lnx/), keepExisting=.false., fill=0d0)

!     fill entries
      ipoint = 0
      irow = 0
      do kk = 1, Ndxi
         irow = irow + 1
         iC(irow) = ipoint + 1

!        shift between x-component and y-component
         ishift = 1 + nd(kk)%lnx

!        y-component (next row)
         iC(irow + 1) = iC(irow) + ishift

!        diagonal entry x-component
         ipoint = ipoint + 1
         icolumn = irow
         jC(ipoint) = icolumn
         ac(ipoint) = 1d0 ! some non-zero dummy value

!        diagonal entry y-component
         jc(ipoint + ishift) = irow + 1
         ac(ipoint + ishift) = 2d0 ! some non-zero dummy value

!        off-diagonals
         do i = 1, nd(kk)%lnx
            ipoint = ipoint + 1
            LL = abs(nd(kk)%ln(i))
            kkother = ln(1, LL) + ln(2, LL) - kk
            icolumn = (kkother - 1) * 2 + 1

!           x-component
            jc(ipoint) = icolumn
            ac(ipoint) = 3d0 ! some non-zero dummy value

!           y-component
            jc(ipoint + ishift) = icolumn + 1
            ac(ipoint + ishift) = 4d0 ! some non-zero dummy value
         end do

!        shift to y-component
         ipoint = ipoint + ishift
         irow = irow + 1
      end do

!     boundaries: apply homogeneous Neumann condition (copy from inside)
      do kk = Ndxi + 1, Ndx
         irow = irow + 1
         ic(irow) = ipoint + 1
         LL = abs(nd(kk)%ln(1)) ! only one flowlink attached to boundary flownode
         kkother = ln(1, LL) + ln(2, LL) - kk ! ln(2,LL)

!        x-component
         irowother = (kkother - 1) * 2 + 1
         do i = iC(irowother), iC(irowother + 1) - 1
            ipoint = ipoint + 1
            jC(ipoint) = jC(i)
         end do

!        y-component
         irow = irow + 1
         ic(irow) = ipoint + 1
         irowother = (kkother - 1) * 2 + 2
         do i = iC(irowother), iC(irowother + 1) - 1
            ipoint = ipoint + 1
            jC(ipoint) = jC(i)
         end do
      end do

      ic(irow + 1) = ipoint + 1

!     compute sparsity pattern of work matrix W = C R

!     allocate work array
      call realloc(iwork, 2 * Ndx, keepExisting=.false., fill=0)

!     estimate number of non zeros of W=CR
      ! call amub_countonly(2*Ndx,Lnx,aC,jC,iC,aR,jR,iR,iwork,numnonzeros)

!     allocate
      call realloc(iW, 2 * Ndx, keepExisting=.false., fill=0)
      call realloc(jW, numnonzeros, keepExisting=.false., fill=0)
      call realloc(aW, numnonzeros, keepExisting=.false., fill=0d0)

!     get sparsity pattern
      call amub(2 * Ndx, Lnx, 0, aC, jC, iC, aR, jR, iR, aW, jW, iW, numnonzeros, iwork, ierror)
      if (ierror /= 0) goto 1234

!     compute sparsity pattern of whole matrix A = I W (= I C R)

!     allocate work array
      call realloc(iwork, 2 * Ndx, keepExisting=.false., fill=0)

!     estimate number of non zeros of W=CR
      ! call amub_countonly(Lnx,2*Ndx,aI,jI,iI,aW,jW,iW,iwork,numnonzeros)

!     settings
      call SolverSettings(solver_advec, Lnx, numnonzeros)

!     allocate
      ! call allocSolver(solver_advec, ierror)

!     get sparsity pattern
      call amub(Lnx, 2 * Ndx, 0, aI, jI, iI, aW, jW, iW, solver_advec%a, solver_advec%ja, solver_advec%ia, solver_advec%numnonzeros, iwork, ierror)
      if (ierror /= 0) goto 1234

      if (jaoutput == 1) then
         call writematrix('Imat.m', Lnx, iI, jI, aI, 'I', 0)
         call writematrix('Rmat.m', 2 * Ndx, iR, jR, aR, 'R', 0)
         call writematrix('Cmat.m', 2 * Ndx, iC, jC, aC, 'C', 0)
         solver_advec%a = 1d0
         call writematrix('Amat.m', Lnx, solver_advec%ia, solver_advec%ja, solver_advec%a, 'A', 0)
      end if

      ierror = 0
1234  continue

      return
   end subroutine inisolver_advec

end module m_inisolver_advec
