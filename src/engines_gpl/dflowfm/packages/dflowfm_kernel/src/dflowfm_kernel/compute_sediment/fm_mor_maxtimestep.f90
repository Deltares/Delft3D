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

module m_fm_mor_maxtimestep

implicit none

private

public :: fm_mor_maxtimestep

contains

   subroutine fm_mor_maxtimestep()
      use m_flowtimes, only: dts
      use m_flow, only: eps10, jamapflowanalysis, kkcflmx, limitingTimestepEstimation
      use m_flowgeom, only: acl, ba, csu, snu, wu
      use m_sediment, only: dzbdtmax, kcsmor
      use m_fm_erosed, only: sxtot, sytot, cdryb, morfac, lsedtot
      use m_fm_erosed, only: ndx => ndx_mor
      use m_fm_erosed, only: nd => nd_mor
      use m_fm_erosed, only: ln => ln_mor

      implicit none

      integer :: k, k1, k2, kk, L, ised, ac1, ac2
      double precision :: dum, sx, sy, sL, dt, dtmaxmor, kkcflmxloc, mf

      dtmaxmor = huge(0d0)
      kkcflmxloc = 0
      mf = max(morfac, 1d0)

      do k = 1, ndx
         if (kcsmor(k) == 0) then
            cycle
         end if
         !
         dum = 0.d0
         do kk = 1, nd(k)%lnx
            L = abs(nd(k)%ln(kk))
            k1 = ln(1, L)
            k2 = ln(2, L)
            ac1 = acl(L)
            ac2 = 1d0 - ac1
            do ised = 1, lsedtot
               sx = (ac1 * sxtot(k1, ised) + ac2 * sxtot(k2, ised)) / cdryb(ised) * mf
               sy = (ac1 * sytot(k1, ised) + ac2 * sytot(k2, ised)) / cdryb(ised) * mf
               sL = csu(L) * sx + snu(L) * sy
               !
               if (k2 == k) sL = -sL
               !
               if (sL >= 0d0) then ! outgoing transport fluxes only
                  dum = dum + sL * wu(L)
               end if
            end do
         end do
         !
         if (dum > tiny(0d0)) then
            dt = dzbdtmax * ba(k) / max(dum, eps10) ! safety
            if (dt < dtmaxmor) then
               dtmaxmor = dt
               kkcflmxloc = k
            end if
         end if
      end do
      !
      if (dtmaxmor > dts) then
         dtmaxmor = dts
      else
         kkcflmx = kkcflmxloc ! overwrite cell number for numlimdt when new smallest timestep
         if (jamapFlowAnalysis > 0) then
            limitingTimestepEstimation(kkcflmx) = limitingTimestepEstimation(kkcflmx) + 1
         end if
      end if

      dts = dtmaxmor

   end subroutine fm_mor_maxtimestep

end module m_fm_mor_maxtimestep
