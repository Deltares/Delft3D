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

module m_ian_young_pt

implicit none

private

public :: ian_young_pt

contains

 subroutine ian_young_pt(U10, x, d, Hsig, Tsig)
    use m_physcoef
    implicit none
    double precision, intent(IN) :: d, U10, x
    double precision, intent(OUT) :: Hsig, Tsig
    double precision :: E, fp
    double precision :: delta, XX, A1, B1, epsilon, nu, A2, B2, ta1, ta2
    double precision, external :: tanhsafe
    XX = ag * x / U10**2 ! non-dim fetch
    delta = ag * d / U10**2 ! non-dim depth
    ! calculate nondimensional energy
    B1 = 3.13e-3 * XX**0.57
    A1 = 0.493 * delta**0.75
    ta1 = tanhsafe(A1)
    epsilon = 3.64e-3 * (tA1 * tanhsafe(B1 / ta1))**1.74
    ! calculate nondimensional frequency
    B2 = 5.215e-4 * XX**0.73
    A2 = 0.331 * delta**1.01
    ta2 = tanhsafe(A2)
    nu = 0.133 * (tA2 * tanhsafe(B2 / tA2))**(-0.37); 
    E = U10**4 * epsilon / ag**2 ! total energy from non-dim energy
    Hsig = 4 * sqrt(E) ! significant wave height
    fp = nu * ag / U10 ! peak freq from non-dim freq, Hz
    Tsig = 1d0 / fp
 end subroutine ian_young_pt

end module m_ian_young_pt
