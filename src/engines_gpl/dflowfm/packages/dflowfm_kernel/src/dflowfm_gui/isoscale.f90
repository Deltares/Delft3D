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

module m_isoscale

implicit none

contains

  subroutine ISOSCALE() !   COPY OF ISOSCALE, DIRTY BUT QUICK
     use unstruc_colors
     use m_isoscaleunit
     use m_flowgeom, only: ndx
     use m_netw, only: nump, numk
     use m_polygon, only: npl
     use unstruc_display, only: ndrawpol
     use m_depmax
     use m_hardcopy
     use m_scalepos
     use m_vfac
     use m_drawthis
     use m_gtext
     use m_jgtext
     use m_dispform_scale
     use m_box_nop
     use m_fbox_nop
     use m_set_col

     double precision :: hic
     integer :: i, j
     integer :: INC
     double precision :: wi
     double precision :: wic
     double precision :: xleg
     double precision :: xsc0
     double precision :: xsc1
     double precision :: xsc2
     double precision :: yleg
     double precision :: ysc1
     double precision :: ysc2

     character TEXT2 * 10, FMT * 7
     character(LEN=17) :: MINTEX, MAXTEX
     real INFOGRAPHICS

     if (NDRAW(12) == 2 .or. NDRAW(12) == 4) return

     if (NDRAW(8) <= 1 .and. NDRAW(28) <= 1 .and. ndrawpol <= 2) return

     if (max(ndx, nump, npl, numk) == 0) return

     call IGRCHARSIZE(real(SCALESIZE), real(SCALESIZE))
     WIC = dble(INFOGRAPHICS(3))
     HIC = dble(INFOGRAPHICS(4))

     INC = NV / 30 + 1 ! Max 30 color boxes, otherwise increment > 1

     WI = 11 * WIC + 1.8d0 * HIC
     XSC0 = 1 - XSC
     if (XSC0 < 0.6d0) then
        XSC1 = X1 + XSC0 * (X2 - X1)
     else
        XSC1 = X2 - (1 - XSC0) * (X2 - X1) - WI
     end if
     XSC2 = XSC1 + WI
     YSC1 = Y1 + YSC * (Y2 - Y1)

     MINTEX = 'MN=  '
     MAXTEX = 'MX=  '
     write (MINTEX(4:15), '(E11.4)') VMIN
     write (MAXTEX(4:15), '(E11.4)') VMAX

     if (VMAX > VMIN .and. NDRAW(19) >= 2) then
        YSC2 = min(YSC1 + (NV / INC + 1d0) * HIC + 2.5d0 * HIC, Y2)
     else
        YSC2 = min(YSC1 + (1d0) * HIC + 3.5d0 * HIC, Y2)
        XSC2 = XSC2 + 2 * WIC
     end if

     call SETCOL(KLSCL)
     call FBOXNOP(XSC1, YSC1, XSC2, YSC2)

     call SETCOL(KLTEX)
     call BOXNOP(XSC1, YSC1, XSC2, YSC2)

     call IGRCHARJUSTIFY('L')

     call GTEXT(PARAMTEX(1), XSC1 + WIC, YSC2 - 1 * HIC, KLTEX)
     call GTEXT(UNIT(1), XSC1 + WIC, YSC2 - 2 * HIC, KLTEX)

     if (VMAX > VMIN .and. NDRAW(19) >= 2) then
        if (abs(VMIN) > abs(VMAX)) then
           call DISPFORMscale(VMIN, FMT, NDEC)
        else
           call DISPFORMscale(VMAX, FMT, NDEC)
        end if

        XLEG = XSC1 + WIC
        J = 1
        do I = 1, NV, INC
           YLEG = YSC1 + J * HIC
           write (TEXT2(1:10), FMT) VAL(I)
           call JGTEXT(TEXT2, XLEG, YLEG, NCOLS(I), WIC, HIC, 0)
           J = J + 1
        end do
        TEXT2 = '          '
        call JGTEXT(TEXT2, XLEG, YLEG + HIC, NCOLS(NV + 1), WIC, HIC, 0)
     else
        call GTEXT(MAXTEX, XSC1 + WIC, YSC2 - 3 * HIC, KLTEX)
        call GTEXT(MINTEX, XSC1 + WIC, YSC2 - 4 * HIC, KLTEX)
     end if

     return
  end

end module m_isoscale
