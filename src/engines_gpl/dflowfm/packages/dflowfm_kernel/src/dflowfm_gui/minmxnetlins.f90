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

  subroutine MINMXNETLINS()

     use m_netw
     use m_missing
     use m_depmax2, only: vmax=>vmax2, vmin=>vmin2, dv=>dv2, val=>val2, nv=>nv2, jaauto=>jaauto2
     use m_inview

     implicit none
     integer :: i
     integer :: k1
     integer :: k2
     integer :: l
     double precision :: rd
     double precision :: rmax
     double precision :: rmin
     double precision :: xp1
     double precision :: xp2
     double precision :: yp1
     double precision :: yp2
     double precision :: zp1
     double precision :: zp2

     if (JAAUTO > 0) then
        RMIN = 1.0d30
        linmin = 0
        RMAX = -1.0d30
        linmax = 0
        do L = 1, NUML
           K1 = KN(1, L)
           K2 = KN(2, L)
           if (RLIN(L) /= DMISS .and. K1 /= 0 .and. K2 /= 0) then
              XP1 = XK(K1)
              YP1 = YK(K1)
              ZP1 = ZK(K1)
              XP2 = XK(K2)
              YP2 = YK(K2)
              ZP2 = ZK(K2)
              if (INVIEW(XK(K1), YK(K1)) .or. INVIEW(XK(K2), YK(K2))) then
                 RD = RLIN(L)
                 if (RD < RMIN) then
                    RMIN = RD
                    LINMIN = L
                 end if
                 if (RD > RMAX) then
                    RMAX = RD
                    LINMAX = L
                 end if
              end if
           end if
        end do

        VMAX = RMAX
        VMIN = RMIN
     end if

     DV = VMAX - VMIN
     do I = 1, NV
        VAL(I) = VMIN + (I - 1) * DV / (NV - 1)
     end do

     return
  end subroutine MINMXNETLINS
