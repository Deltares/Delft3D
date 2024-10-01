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

!> compute sample gradient at (j=constant) or (i=constant) edges
module m_comp_samplegradi

implicit none

contains

subroutine comp_samplegradi(IDIR, i, j, grad, Sn, DareaL, DareaR)
   use m_comp_grad
   use m_samples, only: MXSAM, MYSAM
   use m_samples_refine

   implicit none

   integer, intent(in) :: IDIR !< 0: (j=constant), 1: (i=constant) edge
   integer, intent(in) :: i, j !< edge indices
   double precision, dimension(2), intent(out) :: grad !< edge-based gradient vector
   double precision, dimension(2), intent(out) :: Sn !< edge surface vector (for divergence)
   double precision, intent(out) :: DareaL, DareaR !< contribution to control volume area (for divergence)

   integer :: ip0, ip1, ip0L, ip0R, ip1L, ip1R

   grad = 0d0
   Sn = 0d0
   DareaL = 0d0
   DareaR = 0d0

   if (IDIR == 0) then
!     i-edge gradient at (i+1/2,j) location
!        control volume:
!
!                  L:(i+1/2,j+1/2)
!                 / \
!                /   \
!         0:(i,j)-----1:(i+1,j)
!                \   /
!                 \ /
!                  R:(i+1/2,j-1/2)
      ip0 = i + MXSAM * (j - 1) ! pointer to (i,j)
      ip1 = i + 1 + MXSAM * (j - 1) ! pointer to (i+1,j)
      ip0L = i + MXSAM * (min(j + 1, MYSAM) - 1) ! pointer to (i,j+1)
      ip0R = i + MXSAM * (max(j - 1, 1) - 1) ! pointer to (i,j-1)
      ip1L = i + 1 + MXSAM * (min(j + 1, MYSAM) - 1) ! pointer to (i+1,j+1)
      ip1R = i + 1 + MXSAM * (max(j - 1, 1) - 1) ! pointer to (i+1,j-1)

      call comp_grad(zss, ip0, ip1, ip0L, ip0R, ip1L, ip1R, grad(1), grad(2), Sn(1), Sn(2), DareaL, DareaR)
   else if (IDIR == 1) then
!     j-edge gradient at (i,j+1/2) location
!        control volume:
!
!                  1:(i,j+1)
!                 / \
!                /   \
! L:(i-1/2,j+1/2)-----R:(i+1/2,j+1/2)
!                \   /
!                 \ /
!                  0:(i,j)
      ip0 = i + MXSAM * (j - 1) ! pointer to (i,j)
      ip1 = i + MXSAM * (j) ! pointer to (i,j+1)
      ip0L = max(i - 1, 1) + MXSAM * (j - 1) ! pointer to (i-1,j)
      ip0R = min(i + 1, MXSAM) + MXSAM * (j - 1) ! pointer to (i+1,j)
      ip1L = max(i - 1, 1) + MXSAM * (j) ! pointer to (i-1,j+1)
      ip1R = min(i + 1, MXSAM) + MXSAM * (j) ! pointer to (i+1,j+1)

      call comp_grad(zss, ip0, ip1, ip0L, ip0R, ip1L, ip1R, grad(1), grad(2), Sn(1), Sn(2), DareaL, DareaR)
   end if

   return
end subroutine comp_samplegradi

end module m_comp_samplegradi
