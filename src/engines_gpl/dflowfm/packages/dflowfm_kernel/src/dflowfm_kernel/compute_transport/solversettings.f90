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

!> default solver settings
   subroutine SolverSettings(solver, numrows, numnonzeros)
      use m_solver
      implicit none

      type(tsolver), intent(inout) :: solver !< solver
      integer, intent(in) :: numrows !< number of rows
      integer, intent(in) :: numnonzeros !< number of nonzero elements

      solver%numrows = numrows
      solver%numnonzeros = numnonzeros
      solver%numnonzerosprecond = 120 * numrows
      solver%nwork = 2 * solver%numnonzerosprecond

         !!   ipar(1) = 0               ! initialized in "itaux"
      solver%ipar(2) = 1 ! no (0), left (1), right (2), both (3) precond
      solver%ipar(3) = 1 ! stopping criteria
      solver%ipar(4) = solver%nwork ! number of elems in array 'wk'
      solver%ipar(5) = 10 ! size of Krylov subspace in GMRES and variants
      solver%ipar(6) = 1000 ! max number of mat-vec multiplies

      solver%fpar(1) = 0.0d-16 ! relative tolerance ('exact' solve, except
      solver%fpar(2) = 1.0d-14 ! absolute tolerance

      solver%lfil = 3
      solver%alpha = 1d0
      solver%tol = 0.50d-2

      solver%jabcgstab = 1
   end subroutine SolverSettings
