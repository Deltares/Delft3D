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
module m_refine
   use m_putarr, only: putarr
   use m_xyspln, only: xyspln
   use m_savegrd, only: savegrd
   use m_getspl2, only: getspl2

   implicit none
contains
   subroutine REFINE(M1, N1, M2, N2, NUM)
      use precision, only: dp
      use m_grid, only: ijyes, nc, mc, mmax, mnmax, nmax, xc, yc
      use m_gridsettings, only: mfac, nfac
      use messagehandling, only: LEVEL_DEBUG, mess
      use m_readyy
      use m_qnerror
      use m_increase_grid
      use m_isitu

      integer :: m1, n1, m2, n2, num
      real(kind=dp), allocatable :: XI2(:, :), XJ2(:, :), YI2(:, :), YJ2(:, :), XR(:, :), YR(:, :), XRH(:, :), YRH(:, :)
      integer :: NRM, NRN, MCR, NCR

      call mess(LEVEL_DEBUG, 'INTERPOLATION')
      call mess(LEVEL_DEBUG, 'DIMENSIONS OF GRID : ', MC, NC)

      if (MC == 0) then
         call QNERROR('First Create or Load a Grid', ' ', ' ')
         NUM = 0
         return
      end if

      NRM = M2 - M1
      NRN = N2 - N1
      MCR = MC - NRM + 1 + NRM * MFAC - 1
      NCR = NC - NRN + 1 + NRN * NFAC - 1

      call SAVEgrd()

      call increasegrid(mcr, ncr)

      allocate (xi2(mmax, nmax), xj2(mmax, nmax), yi2(mmax, nmax), yj2(mmax, nmax), &
                xr(mmax, nmax), yr(mmax, nmax), xrh(mmax, nmax), yrh(mmax, nmax))

      call READYY('INTERPOLATION', 0d0)
      call ISITU() !      X,      Y,     MC,  NC,    IJC,  IJYES)
      call READYY(' ', 0.10d0)

      call GETSPL2(Xc, XI2, XJ2, MC, NC, mmax, nmax)
      call READYY(' ', 0.15d0)

      call GETSPL2(Yc, YI2, YJ2, MC, NC, mmax, nmax)
      call READYY(' ', 0.20d0)

      if (MFAC /= 1 .or. NFAC /= 1) then
         call XYSPLN(Xc, Yc, XR, YR, &
                     XI2, YI2, XJ2, YJ2, XRH, YRH, &
                     mmax, nmax, mnmax, &
                     M1, N1, M2, N2, MC, NC, &
                     MFAC, NFAC, IJYES)
         call READYY(' ', 0.90d0)
      end if

      call PUTARR(XR, Xc, MMAX, NMAX)
      call PUTARR(YR, Yc, MMAX, NMAX)

      MC = MCR
      NC = NCR

      call READYY(' ', 1d0)
      call READYY(' ', -1d0)
      deallocate (XI2, XJ2, YI2, YJ2, XR, YR, XRH, YRH)

      return
   end subroutine refine
end module m_refine
