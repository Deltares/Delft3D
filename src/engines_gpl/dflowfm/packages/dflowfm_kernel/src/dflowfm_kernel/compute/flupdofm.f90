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

!> Determines flow link' upwind/downwind parameters based on current velocities and water levels.
!! NOTE that this is purely for this flow link, independent of left-right orientation of the structure itself.
!! (Motivation: a single structure in 2D may be crossed by multiple flow links, with varying 1->2 orientation.)
module m_flupdofm

   implicit none

contains

   subroutine flupdofm(m, il, ir, istru, velheight, &
                       husb, hdsb, uu, ud, teken, relax)
      use precision, only: dp
      use m_strucs
      use m_flowgeom
      use m_flow

      implicit none
!
! Global variables
!
      integer, intent(in) :: m !< Flow link number, signed! If m < 0 then flow link is in opposite direction than structure left-right orientation.
      integer, intent(in) :: il, ir, istru
      logical, intent(in) :: velheight
      real(kind=dp), intent(in) :: relax

      real(kind=dp) :: hdsb
      real(kind=dp) :: husb
      real(kind=dp) :: teken
      real(kind=dp) :: ud
      real(kind=dp) :: uu

      real(kind=dp) :: tem
!real(kind=dp)               :: ucxku, ucyku
      integer :: L, k, LL, iflip

! Parameters:
! NR NAME              IO DESCRIPTION
!  7 crest             I  crest.
! 10 hd                O  Downstream water level at t=(n+1).
!  9 husb                O  Upstream water level at t=(n+1).
!  2 il                I  Grid point on left side of structure (lower
!                         index).
!  3 ir                I  Grid point on right side of structure (upper
!                         index).
!  4 istru             I  structure number
!  1 m                 I  Index of velocity point, signed! If m < 0 then flow link is in opposite direction than structure left-right orientation.
!  5 rholeft           O  Density of diluted water on left grid point.
!  6 rhoright          O  Density of diluted water on left grid point.
! 13 teken             O  Flow direction (+1/-1).
! 12 ud                O  Downstream velocity.
! 11 uu                O  Upstream velocity.
!  8 velheight         I  True if velicity height will be taken into account

      L = abs(m)
      iflip = max(0, sign(1, m)) ! iflip: 0 if flow link has same orientation as structure's left-right, 1 if opposite (because then the 'left' str point == ln(2,Lf))
      if (relax /= 1d0) then
         husb = s1(il) * relax + (1.d0 - relax) * strhis2(9 + iflip, istru) ! TODO: HK: strhis2 is not yet filled anywhere (no relaxation possible)
         hdsb = s1(ir) * relax + (1.d0 - relax) * strhis2(10 - iflip, istru)
      else
         husb = s1(il)
         hdsb = s1(ir)
      end if
      uu = 0.d0
      ud = 0.d0
      if (velheight) then
         uu = 0d0; ud = 0d0
         do k = 1, nd(il)%lnx
            LL = abs(nd(il)%ln(k))
            if (iadv(LL) /= 22) then ! any non-structure point
               uu = max(uu, abs(u1(LL)))
            end if
         end do

         do k = 1, nd(ir)%lnx
            LL = abs(nd(ir)%ln(k))
            if (iadv(LL) /= 22) then ! any non-structure point
               ud = max(ud, abs(u1(LL)))
            end if
         end do

         !uu = call getucxucynoweirs(il, ucxku, ucyku, 6)
         !uu = csu(L)*ucxku + snu(L)*ucyku
         !call getucxucynoweirs(ir, ucxku, ucyku, 6)
         !ud = csu(L)*ucxku + snu(L)*ucyku
      end if

      if (u1(L) > 0d0) then
         teken = 1d0
      else if (u1(L) < 0d0) then
         teken = -1d0
      else if (s1(iL) > s1(ir)) then
         teken = 1d0
      elseif (s1(iL) < s1(ir)) then
         teken = -1d0
      else ! s1(iL) == s1(ir)
         teken = -dble(sign(1, m)) ! account for orientation of flow link w.r.t. structure
      end if

      if (teken < 0) then
         tem = hdsb; hdsb = husb; husb = tem
         tem = ud; ud = uu; uu = tem
      end if

   end subroutine flupdofm

end module m_flupdofm
