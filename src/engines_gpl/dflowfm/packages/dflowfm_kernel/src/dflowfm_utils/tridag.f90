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

! =================================================================================================
! =================================================================================================
 subroutine tridag(a, b, c, d, e, u, n)
    use precision, only: dp
    implicit none
    integer :: n, j
    real(kind=dp) :: a(n), b(n), c(n), d(n), e(n), u(n), bet, accur = 1d-15

    bet = b(1)
    u(1) = d(1) / bet
    do j = 2, n
       e(j) = c(j - 1) / bet
       bet = b(j) - a(j) * e(j)
       if (abs(bet) < accur) then
          bet = sign(accur, bet)
       end if
       u(j) = (d(j) - a(j) * u(j - 1)) / bet
    end do

    do j = n - 1, 1, -1
       u(j) = u(j) - e(j + 1) * u(j + 1)
    end do
 end subroutine tridag
