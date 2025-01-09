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

module m_solve_petsc

   implicit none

   private

   public :: startpetsc, stoppetsc, ini_petsc, preparePETSCsolver, conjugategradientPETSC

   interface
      module subroutine startpetsc()
         implicit none
      end subroutine startpetsc

      module subroutine stoppetsc()
         implicit none
      end subroutine stoppetsc

      module subroutine ini_petsc(Ndx, ierror)
         implicit none
         integer, intent(in) :: Ndx !< number of cells
         integer, intent(out) :: ierror !< error (1) or not (0)
      end subroutine ini_petsc

      module subroutine preparePETSCsolver(japipe)
         implicit none
         integer, intent(in) :: japipe !< use pipelined CG (1) or not (0)
      end subroutine preparePETSCsolver

      module subroutine conjugategradientPETSC(s1, ndx, its, jacompprecond, iprecond)
         use precision, only: dp
         implicit none
         integer, intent(in) :: ndx
         real(kind=dp), dimension(Ndx), intent(inout) :: s1
         integer, intent(out) :: its
         integer, intent(in) :: jacompprecond !< compute preconditioner (1) or not (0)
         integer, intent(in) :: iprecond !< preconditioner type
      end subroutine conjugategradientPETSC
   end interface

end module m_solve_petsc
