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

!>  compute the orientation of a cell by SVD
subroutine orthonet_compute_orientation(aspect, uu1, vv1, uu2, vv2, i)
   use m_netw
   use m_sferic
   use m_alloc
   use m_qnerror

   implicit none

   integer, intent(in) :: i !< netcell number
   double precision, intent(out) :: aspect !< aspect ratio
   double precision, intent(out) :: uu1, uu2 !< components of first orientation vector
   double precision, intent(out) :: vv1, vv2 !< components of second orientation vector

   double precision, dimension(2, 2) :: B, Jacobian ! Jacobian matrix

   integer, parameter :: M = 6 ! maximum nodes in cell
   double precision, dimension(M, 2) :: A, R ! coefficient matrix
   double precision, dimension(M) :: xi, eta, xminx0, yminy0, theta
   double precision :: x0, y0, D

   integer, dimension(M) :: knodes ! indices of the nodes

   double precision, dimension(2, 2) :: C

   double precision, dimension(2, 2) :: UU, VV ! left and right singular vectors
   double precision, dimension(2) :: S ! singular values

   integer :: j, k, N

   double precision, parameter :: EPS = 1d-4

!--------------------------------------------------------------
!  compute the Jacobian matrix J of net cell i
!--------------------------------------------------------------
   N = netcell(i)%n
   if (N > M) then
      call qnerror('orthonet_compute_orientation: N > M', ' ', ' ')
      return
   end if

   knodes = 0
   knodes(1:N) = (/(netcell(i)%nod(j), j=1, N)/)

!--------------------------------------------------------------
!  Assume (x,y)' = (x0,y0)' + Jacobian*(xi,eta)'
!    and do a least-square fit through the nodes
!  (x0,y0)' is the mean of the node coordinates
!--------------------------------------------------------------

   if (jsferic == 1 .and. jasfer3D == 1) then
      x0 = xzw(i)
      y0 = yzw(i)

      call spher2loc(x0, y0, N, xk(knodes(1:N)), yk(knodes(1:N)), xminx0(1:N), yminy0(1:N))
   else
      xminx0(1:N) = (/(xk(knodes(j)), j=1, N)/)
      yminy0(1:N) = (/(yk(knodes(j)), j=1, N)/)

      x0 = sum(xminx0(1:N)) / N
      y0 = sum(yminy0(1:N)) / N

      xminx0 = xminx0 - x0
      yminy0 = yminy0 - y0
   end if

   theta(1:N) = (/(k - 1, k=1, N)/)
   theta(1:N) = theta(1:N) / N * 2d0 * pi

   xi(1:N) = cos(theta(1:N))
   eta(1:N) = sin(theta(1:N))

   A = 0d0
   A(1:N, 1) = xi(1:N)
   A(1:N, 2) = eta(1:N)

   R = 0d0
   R(1:N, 1) = xminx0
   R(1:N, 2) = yminy0

!     B = A'A
   B = matmul(transpose(A(1:N, :)), A(1:N, :))

!     C = inv(A'A) = inv(B)
   D = B(1, 1) * B(2, 2) - B(1, 2) * B(2, 1) ! determinant

   if (D == 0d0) then
      call qnerror('orthonet_compute_orientation: D==0', ' ', ' ')
      return
   end if

   B = B / D
   C(1, 1) = B(2, 2)
   C(2, 2) = B(1, 1)
   C(1, 2) = -B(1, 2)
   C(2, 1) = -B(2, 1)

!     Jacobian = (inv(A'A)*A'*R)' = (C*A'*R)'
   Jacobian = transpose( &
              matmul( &
              matmul( &
              C, &
              transpose(A(1:N, :)) &
              ), &
              R &
              ) &
              )
!--------------------------------------------------------------
!  compute the Singular Value Decomposition of the Jacobian matrix
!--------------------------------------------------------------
   UU = Jacobian
   call svdcmp(UU, 2, 2, 2, 2, S, VV)

   aspect = min(S(1) / (S(2) + EPS), S(2) / (S(1) + EPS))
   uu1 = UU(1, 1) * S(1)
   vv1 = UU(2, 1) * S(1)
   uu2 = UU(1, 2) * S(2)
   vv2 = UU(2, 2) * S(2)

end subroutine orthonet_compute_orientation
