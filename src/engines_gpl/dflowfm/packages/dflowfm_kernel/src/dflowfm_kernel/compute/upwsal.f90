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

module m_upwsal

implicit none

private

public :: upwsal

contains

 double precision function upwsal(L, k12) ! upwind salinity
    use m_flowgeom
    use m_flow
    implicit none
    integer :: L, k12

    double precision :: cl, sl, rl, ql, qls
    integer :: k, kk, LL, LLL, ku

    cl = csu(L); sl = snu(L)
    if (k12 == 2) then
       cl = -cl; sl = -sl
    end if

    k = ln(k12, L)

    ql = 0
    qls = 0
    do kk = 1, nd(k)%lnx
       LL = nd(k)%ln(kk)
       LLL = abs(LL)
       ku = ln(1, LLL)
       if (ku == k) ku = ln(2, LLL)

       rl = cl * csu(LLL) + sl * snu(LLL)
       if (LL > 0 .and. q1(LLL) > 0) then
          if (rl > 0) then
             ql = ql + rl * q1(LLL)
             qls = qls + rl * q1(LLL) * sa0(ku)
          end if
       else if (LL < 0 .and. q1(LLL) < 0) then
          if (rl < 0) then
             ql = ql + rl * q1(LLL)
             qls = qls + rl * q1(LLL) * sa0(ku)
          end if
       end if
    end do

    if (ql > 0) then
       upwsal = qls / ql
    else
       upwsal = sa0(k)
    end if

 end function upwsal

end module m_upwsal
