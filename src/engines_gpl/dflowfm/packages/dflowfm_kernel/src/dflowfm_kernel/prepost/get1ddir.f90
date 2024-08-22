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

 !> Gets the tangential direction vector for a 1D flow node,
 !! based on direction of the last regular 1D link connected to it.
 !! NOTE: only makes sense when there's only one single regular 1D
 !! link connected (i.e., 1D endpoint, maybe connected with kcu=3
 !! type link to 2D grid cell).
 subroutine get1Ddir(n1, xt, yt)
    use m_flowgeom
    use m_flow
    use m_sferic, only: jsferic, jasfer3D
    use m_missing, only: dmiss, dxymis
    use geometry_module, only: normalin, normalout

    implicit none
    integer, intent(in) :: n1 !< 1D flow node number
    double precision, intent(out) :: xt, yt !< x,y component of estimated tangential vector at this 1D flow node.

    integer :: n2, k, L, LL, ka, kb, k1, k2

    xt = 0d0; yt = 0d0; ka = 0; kb = 0; n2 = 0
    do k = 1, size(nd(n1)%ln)
       LL = nd(n1)%ln(k)
       L = abs(LL)
       if (kcu(L) /= 3) then
          k1 = ln(1, L); k2 = ln(2, L)
          n2 = k2
          if (k1 /= n1) n2 = k1
          if (ka == 0) then
             ka = n2
          else
             kb = n2
          end if
       end if
    end do
    if (kb == 0) kb = n1

    if (n2 > 0) then
       ! 1D regular channel point found: directly compute tangential channel vector
       call normalin(xz(n1), yz(n1), xz(n2), yz(n2), xt, yt, xu(L), yu(L), jsferic, jasfer3D, dxymis)
    else
       ! No other 1D channel points found: estimate 1D channel direction as perpendicular to this kcu3 link
       n2 = sum(abs(ln(1:2, L))) - n1
       call normalout(xz(n1), yz(n1), xz(n2), yz(n2), xt, yt, jsferic, jasfer3D, dmiss, dxymis)
    end if

 end subroutine get1Ddir
