!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_getseg1d

   implicit none

contains

   subroutine getseg1D(hpr, wu2, dz, ai, frcn, friction_type, wid, ar, conv, perim, jaconv) ! copy of above routine dressed out for 1D
      use precision, only: dp
      use m_get_chezy, only: get_chezy
      use m_flow, only: u1, v
      implicit none
      real(kind=dp), intent(in) :: hpr, wu2, dz, ai, frcn
      real(kind=dp), intent(out) :: wid, ar, conv, perim !
      integer, intent(in) :: friction_type, jaconv
      real(kind=dp) :: d83 = 2.666666_dp, d16 = 0.166666_dp, d23 = 0.666666_dp
      real(kind=dp) :: hp2, Cz, cman, hav
      real(kind=dp) :: d38 = 0.375_dp, d14 = 0.25_dp
      integer :: L

      ! for jaconv >= 1, this routine gets 1D conveyance
      ! this constant value, (1+(dz/dy)**2)**0.25 is computed once and is volume cell based instead of link based

      if (ai < 1.0e-3_dp) then
         wid = wu2
         ar = wid * hpr
      else if (hpr < dz) then
         wid = wu2 * hpr / dz
         ar = 0.5_dp * wid * hpr
      else
         wid = wu2
         hp2 = hpr - dz
         ar = wid * 0.5_dp * (hpr + hp2)
      end if

      if (jaconv == 0) then
         return
      else if (frcn == 0.0_dp) then
         conv = 0.0_dp; return
      else if (jaconv == 1) then ! hydraulic radius type

         if (ai < 1.0e-3_dp) then
            perim = wid
         else if (hpr < dz) then
            perim = sqrt(wid * wid + hpr * hpr)
         else
            perim = sqrt(wid * wid + (hpr - hp2) * (hpr - hp2))
         end if

      else if (jaconv >= 2) then ! 1D analytic conveyance type
         if (friction_type == 1) then
            cman = frcn
         else
            if (ai < 1.0e-3_dp) then
               hav = hpr
            else if (hpr < dz) then
               hav = 0.5_dp * hpr
            else
               hav = hpr - 0.5_dp * dz
            end if
            Cz = get_chezy(hav, frcn, u1(L), v(L), friction_type)
            cman = hav**d16 / Cz
         end if

         if (ai < 1.0e-3_dp) then ! see sysdoc 5 1D conveyance
            conv = (ar * hpr**d23) / (cman)
         else if (hpr < dz) then
            conv = (d38 * hpr**d83) / (cman * ai * (1.0_dp + ai * ai)**d14)
         else
            conv = (d38 * (hpr**d83 - hp2**d83)) / (cman * ai * (1.0_dp + ai * ai)**d14)
         end if

      end if
   end subroutine getseg1D

end module m_getseg1d
