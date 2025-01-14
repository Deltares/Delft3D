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
module m_get_prof_1D_min
   use m_pipemin

   implicit none
contains
   subroutine getprof_1D_min(L, hpr, area, width) ! pressurepipe
      use precision, only: dp
      use m_flowgeom
      use unstruc_channel_flow

      integer :: L
      real(kind=dp) :: hpr ! hoogte in profiel
      real(kind=dp) :: area ! wet cross sectional area
      real(kind=dp) :: width ! width at water surface

      real(kind=dp) :: profw ! width  of profile
      real(kind=dp) :: profh ! height of profile
      real(kind=dp) :: area2, width2 ! second prof i.c. interpolation
      real(kind=dp) :: alfa, hh
      integer :: LL, ka, kb, itp

      area = 0d0; width = 0d0

      LL = L
      if (L > lnxi) then ! for 1D boundary links, refer to attached link
         LL = LBND1D(L)
      end if

      if (abs(kcu(ll)) == 1 .and. network%loaded) then !flow1d used only for 1d channels and not for 1d2d roofs and gullies
         call GetCSParsTotal(network%adm%line2cross(LL, 2), network%crs%cross, hpr, area, width, CS_TYPE_MIN)
         return
      end if

      if (prof1D(1, LL) >= 0) then ! direct profile based upon link value
         ka = 0; kb = 0 ! do not use profiles
         profw = prof1D(1, LL)
         profh = prof1D(2, LL)
         itp = prof1D(3, LL)
      else
         ka = -prof1D(1, LL); kb = -prof1D(2, LL)
         profw = profiles1D(ka)%width
         profh = profiles1D(ka)%height
         itp = profiles1D(ka)%ityp
      end if

! negative = closed
      if (itp == -1) then ! pipe
         call pipemin(hpr, profw, area, width)
      else if (itp < 0) then ! closed rest
         hh = hpr - profh
         if (hh > 0d0) then
            width = profw
            area = hh * width
         end if
      end if

      if (ka /= 0 .and. kb /= ka) then ! interpolate in profiles
         area2 = 0d0; width2 = 0d0
         profw = profiles1D(kb)%width
         profh = profiles1D(kb)%height
         itp = profiles1D(kb)%ityp
         alfa = prof1d(3, LL)
         if (itp == -1) then ! pipe
            call pipemin(hpr, profw, area2, width2)
         else ! rest
            hh = hpr - profh
            if (hh > 0d0) then
               width2 = profw
               area2 = hh * width2
            end if
         end if
         area = (1d0 - alfa) * area + alfa * area2
         width = (1d0 - alfa) * width + alfa * width2
      end if
   end subroutine getprof_1D_min
end module m_get_prof_1D_min
