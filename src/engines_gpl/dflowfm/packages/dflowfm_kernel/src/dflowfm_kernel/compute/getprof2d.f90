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
module m_get_prof2d
   implicit none
contains
   subroutine getprof2D(hpr, wu2, dz, ai, frcn, ifrctyp, wid, ar, aconv, jaconv, beta, deltaa, hyr)
      use precision, only: dp
      use m_flow, only: slotw2D
      use m_get_cz

      real(kind=dp), intent(in) :: hpr, wu2, dz, ai, frcn
      real(kind=dp), intent(out) :: wid, ar, aconv ! aconv = (a/conv)**2
      integer, intent(in) :: ifrctyp, jaconv
      real(kind=dp) :: d83 = 2.666666d0, d16 = 0.166666d0, d23 = 0.666666d0, d43 = 1.333333d0
      real(kind=dp) :: hp2, Cz, cman, per, hyr, hav, conv, beta, deltaa
      real(kind=dp) :: d38 = 0.375d0, d311 = 0.27272727d0, hpr83, hp283
      integer :: jac, L

      ! for jaconv >= 1, this routine gets conveyance, but without friction surface to horizontal plane surface ratio influence on conveyance
      ! this constant value, (1+(dz/dy)**2)**0.25 is computed once and is volume cell based instead of link based
      ! Aconv = (A/K)**2 = 1/(C.C.R), K=Conv=sum(ACsqrt(R))

      if (ai < 1d-3) then
         ! if (dz == 0d0) then
         wid = wu2; wid = wid + slotw2D ! wid = max(wid, slotw2d)
         ar = wid * hpr
         hyr = hpr
      else if (hpr < dz) then
         wid = wu2 * hpr / dz; wid = wid + slotw2D ! wid = max(wid, slotw2d)
         ar = 0.5d0 * wid * hpr
         if (jaconv == 1) then
            per = sqrt(hpr * hpr + wid * wid)
            hyr = ar / per
         end if
      else
         wid = wu2; wid = wid + slotw2D ! wid = max(wid, slotw2d)
         hp2 = hpr - dz
         ar = wid * 0.5d0 * (hpr + hp2)
         if (jaconv == 1) then
            per = sqrt(dz * dz + wid * wid)
            hyr = ar / per
         end if
      end if

      if (jaconv == 0) then
         return
      else if (frcn == 0d0) then
         aconv = 0d0; return
      else if (jaconv == 1) then ! hydraulic radius type

         call getcz(hyr, frcn, ifrctyp, Cz, L)
         aconv = 1d0 / (Cz * Cz * hyr)

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

         jac = jaconv
         if (jaconv == 3 .and. beta == 0d0) jac = 2
         if (jac == 2) then
            if (ai < 1d-3) then ! see sysdoc 5 1D conveyance
               aconv = (cman / hpr**d23)**2
            else if (hpr < dz) then
               aconv = (d43 * cman / hpr**d23)**2
            else
               aconv = (d43 * cman * (hpr * hpr - hp2 * hp2) / (hpr**d83 - hp2**d83))**2
            end if
         else
            if (ai < 1d-3) then ! see sysdoc 5 2D conveyance
               aconv = (cman / (beta * hpr**d23))**2
            else if (hpr < dz) then
               hpr83 = hpr**d83
               conv = (beta - hpr * deltaa / ai) * d38 * hpr83 + (deltaa * d311 / ai) * hpr * hpr83
               conv = conv / ai
               aconv = (cman * ar / conv)**2
            else
               hpr83 = hpr**d83; hp283 = hp2**d83
               conv = (beta - hpr * deltaa / ai) * d38 * (hpr83 - hp283) + (deltaa * d311 / ai) * (hpr * hpr83 - hp2 * hp283)
               conv = conv / ai
               aconv = (cman * ar / conv)**2
            end if
         end if
      end if
   end subroutine getprof2D
end module m_get_prof2d
