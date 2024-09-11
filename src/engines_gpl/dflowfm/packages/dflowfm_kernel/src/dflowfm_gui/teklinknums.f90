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

  subroutine TEKLINKNUMS(MET, NCOL)
     use M_MISSING
     use m_netw
     use m_drawthis
     use m_halt2
     implicit none
     integer :: MET, NCOL

     integer :: k1
     integer :: k2
     integer :: key
     integer :: l
     double precision :: vv
     logical :: invnod

     double precision XP, YP, ZP
     call SETCOL(NCOL)
     if (MET == 2 .or. MET >= 6 .and. MET <= 8) then
        LMOD = max(1, NUML / 100)
        do L = 1, NUML
           if (mod(L, LMOD) == 0) then
              call HALT2(KEY)
              if (KEY == 1) then
                 return
              end if
           end if
           VV = RLIN(L)
           if (VV /= dmiss) then
              K1 = KN(1, L)
              K2 = KN(2, L)
              if (K1 /= 0 .and. K2 /= 0) then
                 if (.not. INVNOD(K1) .and. .not. INVNOD(K2)) cycle
                 XP = 0.5d0 * (XK(K1) + XK(K2))
                 YP = 0.5d0 * (YK(K1) + YK(K2))
                 ZP = 0.5d0 * (ZK(K1) + ZK(K2))
                 if (NDRAW(7) == 2 .or. NDRAW(7) == 3 .or. (NDRAW(7) >= 10 .and. ndraw(7) /= 16 .and. ndraw(7) /= 17 .and. ndraw(7) /= 18)) then
                    call DHITEXT(int(VV), XP, YP)
                 else
                    call DHTEXT(VV, XP, YP, ZP)
                 end if
              end if
           end if
        end do
     end if
     return
  end subroutine TEKLINKNUMS
