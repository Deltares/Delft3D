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

module m_isoscale2

   implicit none

contains

   subroutine ISOSCALE2() !   tekenen legenda
      use precision, only: dp
      use m_arrows, only: arrows
      use m_isoscaleunit, only: paramtex, unit
      use unstruc_colors, only: x1, x2, y1, y2, klscl, kltex, klvec
      use m_scalepos, only: scalesize, xsc, ysc, ndec
      use m_vfac, only: vfac
      use m_drawthis, only: ndraw
      use m_gtext, only: gtext
      use m_jgtext, only: jgtext
      use m_dispform_scale, only: dispformscale
      use m_box_nop, only: boxnop
      use m_fbox_nop, only: fboxnop
      use m_set_col, only: setcol
      use m_samples, only: ns
      use m_depmax2, only: vmax => vmax2, vmin => vmin2, val => val2, ncols => ncols2, nv => nv2

      real(kind=dp) :: hic
      integer :: i, j
      integer :: INC
      real(kind=dp) :: vfac2
      real(kind=dp) :: wi
      real(kind=dp) :: wic
      real(kind=dp) :: xleg
      real(kind=dp) :: xsc1
      real(kind=dp) :: xsc2
      real(kind=dp) :: yleg
      real(kind=dp) :: ysc1
      real(kind=dp) :: ysc2
      real(kind=dp) :: yt
      character TEXT2 * 10, FMT * 7
      character(LEN=8) :: TEX
      character(LEN=32) :: MINTEX, MAXTEX
      real INFOGRAPHICS

      if (NDRAW(12) == 1 .or. NDRAW(12) == 4) then
         return ! 1 = isoscale off
      end if
      if (NDRAW(29) <= 1 .and. NDRAW(7) <= 1) then
         if (ndraw(32) <= 0 .or. NS < 1) then
            return ! 1 = no, which linval
         end if
      end if

      call IGRCHARSIZE(real(SCALESIZE), real(SCALESIZE))
      WIC = INFOGRAPHICS(3)
      HIC = INFOGRAPHICS(4)

      INC = NV / 30 + 1 ! Max 30 color boxes, otherwise increment > 1

      WI = 10 * WIC + 1.8_dp * HIC
      if (XSC < 0.6_dp) then
         XSC1 = X1 + XSC * (X2 - X1)
      else
         XSC1 = X2 - (1 - XSC) * (X2 - X1) - WI
      end if
      XSC2 = XSC1 + WI
      YSC1 = Y1 + YSC * (Y2 - Y1)

      write (MINTEX, '("MIN:", E12.5)') VMIN
      write (MAXTEX, '("MAX:", E12.5)') VMAX

      if (VMAX > VMIN .and. NDRAW(11) >= 2) then
         YSC2 = min(YSC1 + (NV / INC + 1.0_dp) * HIC + 2.5_dp * HIC, Y2)
      else
         YSC2 = min(YSC1 + (1.0_dp) * HIC + 3.5_dp * HIC, Y2)
         XSC2 = XSC2 + 2 * WIC
      end if

      call SETCOL(KLSCL)
      call FBOXnop(XSC1, YSC1, XSC2, YSC2)

      call SETCOL(KLTEX)
      call BOXnop(XSC1, YSC1, XSC2, YSC2)

      call IGRCHARJUSTIFY('L')

      call GTEXT(PARAMTEX(2), XSC1 + WIC, YSC2 - 1 * HIC, KLTEX)
      call GTEXT(UNIT(2), XSC1 + WIC, YSC2 - 2 * HIC, KLTEX)

      if (VMAX > VMIN .and. NDRAW(11) >= 2) then
         if (abs(VMIN) > abs(VMAX)) then
            call DISPFORMscale(VMIN, FMT, NDEC)
         else
            call DISPFORMscale(VMAX, FMT, NDEC)
         end if

         XLEG = XSC1 + WIC
         J = 1
         do I = 1, NV, INC
            YLEG = YSC1 + J * HIC
            write (TEXT2(1:10), FMT) real(VAL(I))
            call JGTEXT(TEXT2, XLEG, YLEG, NCOLS(I), WIC, HIC, 0)
            J = J + 1
         end do
         TEXT2 = '          '
         call JGTEXT(TEXT2, XLEG, YLEG + HIC, NCOLS(NV + 1), WIC, HIC, 0)
      else
         call GTEXT(MAXTEX, XSC1 + WIC, YSC2 - 3 * HIC, KLTEX)
         call GTEXT(MINTEX, XSC1 + WIC, YSC2 - 4 * HIC, KLTEX)
      end if

      if (NDRAW(15) == 11 .or. NDRAW(15) == 13 .or. NDRAW(15) == 15 .or. NDRAW(15) == 16) then
         call SETCOL(KLSCL)
         YT = YSC1 - 5 * HIC
         call FBOXnop(XSC1, YT - 4 * HIC, XSC2, YT)
         call SETCOL(KLTEX)
         call BOXnop(XSC1, YT - 4 * HIC, XSC2, YT)
         VFAC2 = 0.3_dp * (XSC2 - XSC1)
         call SETCOL(KLVEC)
         call ARROWS(XSC1 + WIC, YT - 2 * HIC, 1.0_dp, 0.0_dp, 0.0_dp, VFAC2)
         TEX = ' 2.3 m/s'
         ! WRITE(TEX(1:4),'(F4.1)')  VFAC2/(DX*VFAC)
         write (TEX(1:4), '(F4.1)') real(VFAC2 / (VFAC))
         call IGRCHARJUSTIFY('R')
         call GTEXT(TEX, XSC2 - WIC, YT - 2 * HIC, KLTEX)
      end if

      return
   end

end module m_isoscale2
