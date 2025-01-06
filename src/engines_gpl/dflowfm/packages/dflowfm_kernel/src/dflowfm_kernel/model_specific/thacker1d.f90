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

module m_thacker1d
use m_statisticsonemorepoint, only: statisticsonemorepoint
use m_statisticsnewstep, only: statisticsnewstep
use m_statisticsfinalise, only: statisticsfinalise


   implicit none

   private

   public :: thacker1d

contains

   subroutine thacker1d(ini, xz, yz, s1, bl, ndx, t)
      use precision, only: dp
      use m_netw, only: numk, xk, zk
      use m_sferic
      use m_physcoef, only: ag
      use m_flowparameters, only: ibedlevtyp
      use m_set_bobs
      use m_inview
      use m_movabs
      use m_lnabs

      integer :: ndx, ini
      real(kind=dp) :: dep, xz(ndx), yz(ndx), s1(ndx), bl(ndx), t
      integer :: is, k
      real(kind=dp) :: omeg, r, r0, rr0, psi, samp, st, ct, ux, uy, s1k, dif, xx, yy, period

      dep = 10d0
      fcorio = 0d0
      ! omeg   = twopi/(12*3600)  ! period = 12 hrs
      ! r0     = sqrt( 2d0*ag*dep/ ( omeg*( omeg+fcorio) ) )  ! Casulli 2007 (19) mind you, no - sign in front of fcorio

      r0 = 120d0
      omeg = sqrt(2d0 * ag * dep / (r0 * r0))
      period = twopi / omeg

      if (ini == 1) then
         if (ibedlevtyp == 3) then
            do k = 1, numk
               r = xk(k) - 150d0
               rr0 = (r * r) / (r0 * r0)
               zk(k) = -dep * (1d0 - rr0)
            end do
         else
            do k = 1, ndx
               r = xz(k) - 150d0
               rr0 = (r * r) / (r0 * r0)
               bl(k) = -dep * (1d0 - rr0)
            end do
         end if
         call setbobs()
      end if

      !psi    = 0.25d0*r0
      psi = 0.23d0 * r0
      samp = psi * dep / (r0 * r0)
      st = sin(omeg * t)
      ct = cos(omeg * t)
      is = 0
      call statisticsnewstep()
      do k = 1, ndx
!    r     = xz(k) - 150d0  ! sqrt(  xz(k)*xz(k) + yz(k)*yz(k) )
!    s1k   = samp*r*ct

         xx = xz(k) - 150d0; yy = 0
         s1k = samp * (2d0 * xx * ct - 2d0 * yy * st - psi * ct * ct)

         if (ini == 1) then
            s1(k) = max(bl(k), s1k)
         end if

         if (s1k > bl(k)) then
            dif = abs(s1(k) - s1k)
            if (inview(xz(k), yz(k))) then
               call statisticsonemorepoint(dif)
            end if
            if (is == 0) then
               call movabs(xz(k), s1k); is = 1
            else
               call lnabs(xz(k), s1k)
            end if
         end if
      end do
      call statisticsfinalise()

      ux = -psi * omeg * st
      uy = -psi * omeg * ct

   end subroutine thacker1d

end module m_thacker1d
