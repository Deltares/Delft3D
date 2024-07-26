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

 subroutine QucPer3Dsigma(n12, LL, Lb, Lt, cs, sn, quk1) ! sum of (Q*uc cell centre upwind normal) at side n12 of basis link LL
    use m_flow ! advect the cell center velocities (dimension: m4/s2)
    use m_flowgeom ! leaving the cell = +
    use m_sferic
    implicit none

    integer, intent(in) :: n12, LL, Lb, Lt ! working for basis link LL
    double precision, intent(in) :: cs, sn
    double precision, intent(out) :: quk1(3, Lt - Lb + 1) !

    ! locals
    integer :: La, LLL, LLLL, Lb2, Lt2, Lk ! for links LL,
    integer :: k12, Lkin ! relevant node, 1 or 2, L/R
    double precision :: ucin ! velocity surplus

    double precision :: ucinx, uciny
    integer :: nn12

    double precision, external :: lin2nodx, lin2nody, nod2linx, nod2liny

    Quk1 = 0d0

    k12 = ln(n12, LL)
    do La = 1, nd(k12)%lnx ! loop over all attached links
       LLL = nd(k12)%ln(La)
       nn12 = 1; if (LLL > 0) nn12 = 2
       LLLL = iabs(LLL)

       Lb2 = Lbot(LLLL); Lt2 = Ltop(LLLL)
       do Lk = LB2, LT2

          if (qa(Lk) /= 0) then ! include own link

             ucinx = lin2nodx(LLLL, nn12, ucxu(Lk), ucyu(Lk))
             uciny = lin2nody(LLLL, nn12, ucxu(Lk), ucyu(Lk))

             if (jarhoxu > 0) then
                if (jasfer3D == 0) then
                   ucin = (ucxu(Lk) * cs + ucyu(Lk) * sn) * rhou(Lk) - u1(Lb + Lk - Lb2) * rhou(Lb + Lk - Lb2)
                else
                   ucin = (nod2linx(LL, n12, ucinx, uciny) * cs + nod2liny(LL, n12, ucinx, uciny) * sn) * rhou(Lk) - u1(Lb + Lk - Lb2) * rhou(Lb + Lk - Lb2)
                end if
             else
                if (jasfer3D == 0) then
                   ucin = ucxu(Lk) * cs + ucyu(Lk) * sn - u1(Lb + Lk - Lb2)
                else
                   ucin = nod2linx(LL, n12, ucinx, uciny) * cs + nod2liny(LL, n12, ucinx, uciny) * sn - u1(Lb + Lk - Lb2)
                end if
             end if
             if (LLL > 0) then ! incoming link
                ucin = -1d0 * ucin
             end if
             Lkin = min(Lk - Lb2 + 1, Lt - Lb + 1) ! for fixed layers just add to top index
             Quk1(1, Lkin) = Quk1(1, Lkin) + qa(Lk) * ucin

          end if

       end do

    end do

 end subroutine QucPer3Dsigma
