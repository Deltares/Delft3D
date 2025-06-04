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

module m_bermslopenudging

   implicit none

   private

   public :: bermslopenudging

contains

   subroutine bermslopenudging(error)
      use precision, only: dp
      use m_sediment
      use m_fm_erosed
      use m_flow, only: hu, epshu
      use m_flowgeom, only: lnx, ln, wu_mor
      use m_waves, only: hwav
      use m_flowparameters, only: jawave
      use m_debug
      use m_waveconst

      logical, intent(out) :: error

      integer :: L, k1, k2
      integer :: lsd
      real(kind=dp) :: hwavu, slope, flx, frc, fixf, trmag_u, slpfac

      error = .true.
      !
      ! Determine points where berm slope adjustment is used
      bermslopeindex = .false.
      bermslopeindexbed = .false.
      bermslopeindexsus = .false.
      !
      if (jawave > NO_WAVES) then
         do L = 1, lnx
            if (hu(L) < epshu) cycle
            k1 = ln(1, L); k2 = ln(2, L)
            hwavu = max(hwav(k1), hwav(k2))
            if (hwavu > bermslopegamma * hu(L)) then
               bermslopeindex(L) = .true.
            end if
         end do
      end if
      ! Criteria cannot be combined as hwav not allocated for jawave==0
      do L = 1, lnx
         if (hu(L) < epshu) cycle
         if (hu(L) < bermslopedepth) then ! to check: hu value up to date? Replace by weighted hs?
            bermslopeindex(L) = .true.
         else
            bermslopeindex(L) = .false. .or. bermslopeindex(L)
         end if
      end do
      !
      if (bermslopebed) then
         bermslopeindexbed = bermslopeindex
      end if
      if (bermslopesus) then
         bermslopeindexsus = bermslopeindex
      end if
      !
      do L = 1, lnx
         if (hu(L) <= epshu) cycle
         if (wu_mor(L) == 0) cycle
         if (.not. bermslopeindexbed(L) .and. .not. bermslopeindexsus(L)) cycle
         !
         k1 = ln(1, L); k2 = ln(2, L)
         !
         ! Transports positive outgoing
         !
         slope = max(hypot(e_dzdn(L), e_dzdt(L)), 1d-8)
         slpfac = bermslopefac * (-e_dzdn(L) + bermslope * e_dzdn(L) / slope) / max(morfac, 1d0)
         do lsd = 1, lsedtot
            !
            ! slope magnitude smaller than bermslope leads to transport away from the cell, ie outward
            ! minus sign because e_dzdn is defined as bl1-bl2 in fm_erosed
            if (bermslopeindexbed(L) .and. bed /= 0.0) then
               if (.not. has_bedload(stmpar%sedpar%tratyp(lsd))) cycle
               trmag_u = hypot(e_sbcn(L, lsd), e_sbct(L, lsd))
               flx = trmag_u * slpfac
               e_sbcn(L, lsd) = e_sbcn(L, lsd) - flx
               call getfracfixfac(L, k1, k2, lsd, e_sbcn(L, lsd), frc, fixf)
               e_sbcn(L, lsd) = e_sbcn(L, lsd) * frc * fixf
               !
               trmag_u = hypot(e_sbwn(L, lsd), e_sbwt(L, lsd))
               flx = trmag_u * slpfac
               e_sbwn(L, lsd) = e_sbwn(L, lsd) - flx
               call getfracfixfac(L, k1, k2, lsd, e_sbwn(L, lsd), frc, fixf)
               e_sbwn(L, lsd) = e_sbwn(L, lsd) * frc * fixf
            end if
            !
            if (bermslopeindexsus(L) .and. sus /= 0.0 .and. lsd <= lsed) then
               trmag_u = abs(e_ssn(L, lsd))
               flx = trmag_u * slpfac
               e_ssn(L, lsd) = e_ssn(L, lsd) - flx
               call getfracfixfac(L, k1, k2, lsd, e_ssn(L, lsd), frc, fixf)
               e_ssn(L, lsd) = e_ssn(L, lsd) * frc * fixf
               !
               trmag_u = hypot(e_sswn(L, lsd), e_sswt(L, lsd))
               flx = trmag_u * slpfac
               e_sswn(L, lsd) = e_sswn(L, lsd) - flx
               call getfracfixfac(L, k1, k2, lsd, e_sswn(L, lsd), frc, fixf)
               e_sswn(L, lsd) = e_sswn(L, lsd) * frc * fixf
            end if
         end do
      end do
      !
      error = .false.
      return

   end subroutine bermslopenudging

   subroutine getfracfixfac(L, k1, k2, lsd, transp, frc, fixf)
      use precision, only: dp
      use m_fm_erosed
      use m_flow, only: hu, epshu
      use m_flowgeom

      implicit none

      integer, intent(in) :: L, k1, k2, lsd
      real(kind=dp), intent(in) :: transp
      real(kind=dp), intent(out) :: frc, fixf

      if (L > lnxi .and. hu(L) > epshu) then
         fixf = fixfac(k2, lsd)
         frc = frac(k2, lsd)
      else
         if (transp >= 0) then
            fixf = fixfac(k1, lsd)
            frc = frac(k1, lsd)
         else
            fixf = fixfac(k2, lsd)
            frc = frac(k2, lsd)
         end if
      end if
   end subroutine getfracfixfac

end module m_bermslopenudging
