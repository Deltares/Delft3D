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

module m_getseg1d

   implicit none

contains

   subroutine getseg1D(hpr, wu2, dz, ai, frcn, ifrctyp, wid, ar, conv, perim, jaconv) ! copy of above routine dressed out for 1D
      use precision, only: dp
      use m_get_cz
      implicit none
      real(kind=dp), intent(in) :: hpr, wu2, dz, ai, frcn
      real(kind=dp), intent(out) :: wid, ar, conv, perim !
      integer, intent(in) :: ifrctyp, jaconv
      real(kind=dp) :: d83 = 2.666666d0, d16 = 0.166666d0, d23 = 0.666666d0
      real(kind=dp) :: hp2, Cz, cman, hav
      real(kind=dp) :: d38 = 0.375d0, d14 = 0.25d0
      integer :: L

      ! for jaconv >= 1, this routine gets 1D conveyance
      ! this constant value, (1+(dz/dy)**2)**0.25 is computed once and is volume cell based instead of link based

      if (ai < 1d-3) then
         wid = wu2
         ar = wid * hpr
      else if (hpr < dz) then
         wid = wu2 * hpr / dz
         ar = 0.5d0 * wid * hpr
      else
         wid = wu2
         hp2 = hpr - dz
         ar = wid * 0.5d0 * (hpr + hp2)
      end if

      if (jaconv == 0) then
         return
      else if (frcn == 0d0) then
         conv = 0d0; return
      else if (jaconv == 1) then ! hydraulic radius type

         if (ai < 1d-3) then
            perim = wid
         else if (hpr < dz) then
            perim = sqrt(wid * wid + hpr * hpr)
         else
            perim = sqrt(wid * wid + (hpr - hp2) * (hpr - hp2))
         end if

      else if (jaconv >= 2) then ! 1D analytic conveyance type
         if (ifrctyp == 1) then
            cman = frcn
         else
            if (ai < 1d-3) then
               hav = hpr
            else if (hpr < dz) then
               hav = 0.5d0 * hpr
            else
               hav = hpr - 0.5d0 * dz
            end if
            call getcz(hav, frcn, ifrctyp, Cz, L)
            cman = hav**d16 / Cz
         end if

         if (ai < 1d-3) then ! see sysdoc 5 1D conveyance
            conv = (ar * hpr**d23) / (cman)
         else if (hpr < dz) then
            conv = (d38 * hpr**d83) / (cman * ai * (1d0 + ai * ai)**d14)
         else
            conv = (d38 * (hpr**d83 - hp2**d83)) / (cman * ai * (1d0 + ai * ai)**d14)
         end if

      end if
   end subroutine getseg1D

end module m_getseg1d
