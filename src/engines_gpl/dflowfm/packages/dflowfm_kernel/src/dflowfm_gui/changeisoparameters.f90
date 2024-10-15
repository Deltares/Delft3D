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

module m_changeisoparameters

implicit none

contains

      subroutine CHANGEISOPARAMETERS()
         use unstruc_colors
         use unstruc_display_data
         use dflowfm_version_module, only: company, product_name
         use m_depmax
         use m_helpnow
         use m_scalepos
         use m_depmax2
         use m_help
         use m_highlight_form_line

         double precision :: dvi, dvi2
         double precision :: dvnu
         integer :: i
         integer :: ifexit
         integer :: ifinit
         integer :: ih
         integer :: il
         integer :: imp
         integer :: inp
         integer :: ir
         integer :: iw
         integer :: ixp
         integer :: iyp
         integer :: key
         integer :: nbut
         integer :: nien
         integer :: nisn
         integer :: numfld
         integer :: numpar
         integer :: nvn
         double precision :: vmaxn
         double precision :: vminn
         parameter(NUMPAR=19, NUMFLD=2 * NUMPAR)
         integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
         character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
         integer, external :: infoinput
!
         NLEVEL = 3
         OPTION(1) = 'AUTOSCALE ON OR OFF                     '
         OPTION(2) = 'NUMBER OF ISOCOLOURS                    '
         OPTION(3) = 'MINIMUM ISOLINE VALUE                   '
         OPTION(4) = 'MAXIMUM ISOLINE VALUE                   '
         OPTION(5) = 'ISOLINE INTERVAL                        '
         OPTION(6) = 'COLOUR NUMBER OF FIRST COLOUR           '
         OPTION(7) = 'COLOUR NUMBER OF LAST  COLOUR           '
         OPTION(8) = 'X COOR LOWER LEFT CORNER OF LEGEND (0-1)'
         OPTION(9) = 'Y COOR LOWER LEFT CORNER OF LEGEND (0-1)'
         OPTION(10) = 'NUMBER OF DECIMALS COLOURSCALE LEGEND   '
         OPTION(11) = 'FONTSIZE COLOURSCALE LEGEND (0.5-1.5)   '
         OPTION(12) = 'Settings for secondary legend:          '
         OPTION(13) = '  AUTOSCALE ON OR OFF                   '
         OPTION(14) = '  NUMBER OF ISOCOLOURS                  '
         OPTION(15) = '  MINIMUM ISOLINE VALUE                 '
         OPTION(16) = '  MAXIMUM ISOLINE VALUE                 '
         OPTION(17) = '  ISOLINE INTERVAL                      '
         OPTION(18) = '  COLOUR NUMBER OF FIRST COLOUR         '
         OPTION(19) = '  COLOUR NUMBER OF LAST  COLOUR         '
!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
         HELPM(1) = 'INTEGER VALUE , AUTOSCALE OFF = 0, ON = 1                   '
         HELPM(2) = 'INTEGER VALUE =< 30                                         '
         HELPM(3) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
         HELPM(4) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
         HELPM(5) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
         HELPM(6) = 'INTEGER VALUE, STARTINDEX OF ISOCOLOURS (0-255) DEFAULT 46  '
         HELPM(7) = 'INTEGER VALUE, ENDINDEX OF ISOCOLOURS (0-255)   DEFAULT 224 '
         HELPM(8) = 'REAL VALUE (0-1), X COORDINATE LOWER LEFT CORNER LEGEND     '
         HELPM(9) = 'REAL VALUE (0-1), Y COORDINATE LOWER LEFT CORNER LEGEND     '
         HELPM(10) = 'INTEGER, NR OF DECIMALS IN COLOURSCALE LEGEND               '
         HELPM(11) = 'REAL VALUE, FONTSIZE OF COLOURSCALE LEGEND TEXT, DEFAULT 0.5'
         HELPM(12) = '                                                            '
         HELPM(13) = 'INTEGER VALUE , AUTOSCALE OFF = 0, ON = 1                   '
         HELPM(14) = 'INTEGER VALUE =< 30                                         '
         HELPM(15) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
         HELPM(16) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
         HELPM(17) = 'REAL VALUE, IF CHANGED, AUTOSCALE IS TURNED OFF             '
         HELPM(18) = 'INTEGER VALUE, STARTINDEX OF ISOCOLOURS (0-255) DEFAULT 46  '
         HELPM(19) = 'INTEGER VALUE, ENDINDEX OF ISOCOLOURS (0-255)   DEFAULT 224 '

         IR = 0
         do I = 1, NUMPAR
            IL = IR + 1
            IR = IL + 1
            IX(IL) = 13
            IX(IR) = 95
            IY(IL) = 2 * I
            IY(IR) = 2 * I
            IS(IL) = 82
            IS(IR) = 10
            IT(IL) = 1001
            if (I >= 3 .and. I <= 5 .or. I >= 15 .and. I <= 17) then
               ! Real values:
               IT(IR) = 6
            else
               ! Integer values:
               IT(IR) = 2
            end if
         end do
         IT(2 * 8) = 6
         IT(2 * 9) = 6
         IT(2 * 11) = 6

!     Initialise
         call IWinWordWrap('OFF')
         call ITEXTCOLOURN(HLPFOR, HLPBCK)
         call INHIGHLIGHT('WHITE', 'RED')
         IW = NPOS(3)
         IXP = NPOS(1) + (IWS - IW) / 2
         IYP = NPOS(2)
         IH = IHS - 9

!     Header of filewindow
         call IWinAction('FPC')
         call IWinOpen(IXP, IYP, IW, 1)
         call ITEXTCOLOURN(LBLFOR, LBLBCK)
         call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' ISOPARAMETER FORM')
         call ITEXTCOLOURN(HLPFOR, HLPBCK)
!
!     Explain keyfunctions in bottom window
         call IWinAction('FPC')
         call IWinOpen(IXP, IHS - 1, IW, 2)
         call IWinOutStringXY(1, 1, 'move = , Tab, confirm = Enter, no change = Esc, help = F3')
         call IWinOutStringXY(1, 2, 'right mouse = Enter, click outside window = Esc')

!     Filewindow is middelste window
         call IWinAction('FPC')
         call IWinOpen(IXP, IYP + 3, IW, IH)

         call InControlKey(29, 129)
         call InControlKey(30, 128)

!     NUMWNH = InfoWindow(1)
!     CALL IWinSelect(NUMWNH)

!     Define a new form by supplying arrays containing
!     field positions, sizes and types
         call IFORMDEFINE('W', NUMFLD, IX, IY, IS, IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
         call IFORMHELP(13, IH, 60)

         IR = 0
         do I = 1, NUMPAR
            IL = IR + 1
            IR = IL + 1
            call IFORMPUTSTRING(IL, OPTION(I))
            call IFORMPUTHELP(IR, HELPM(I))
            call IFORMATTRIBUTEN(IR, 0, 0, 7)
         end do
         call IFORMPUTINTEGER(2, JAAUTO)
         call IFORMPUTINTEGER(4, NV)
         call IFormPutDouble(6, VMIN, '(F10.3)')
         call IFormPutDouble(8, VMAX, '(F10.3)')
         DVI = DV / (NV - 1)
         call IFormPutDouble(10, DVI, '(F10.3)')
         call IFORMPUTINTEGER(12, NIS)
         call IFORMPUTINTEGER(14, NIE)
         call IFormPutDouble(16, XSC, '(F10.3)')
         call IFormPutDouble(18, YSC, '(F10.3)')
         call IFORMPUTINTEGER(20, NDEC)
         call IFormPutDouble(22, SCALESIZE, '(F10.3)')
         ! 2nd isocolour legend:
         call IFORMPUTINTEGER(26, JAAUTO2)
         call IFORMPUTINTEGER(28, NV2)
         call IFormPutDouble(30, VMIN2, '(F10.3)')
         call IFormPutDouble(32, VMAX2, '(F10.3)')
         DVI2 = DV2 / (NV2 - 1)
         call IFormPutDouble(34, DVI2, '(F10.3)')
         call IFORMPUTINTEGER(36, NIS2)
         call IFORMPUTINTEGER(38, NIE2)

!  Display the form with numeric fields left justified
!  and set the initial field to number 2
         call IOUTJUSTIFYNUM('L')
         IFEXIT = 2
         call IFormAttribute(IFEXIT - 1, 'BU', ' ', ' ')
         call IFORMSHOW()

30       continue
         IFINIT = IFEXIT
         call IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!     check for Help, Confirm, Quit
         KEY = INFOINPUT(55)
         if (KEY == -2) then
            NBUT = INFOINPUT(61)
            if (NBUT >= 1) then
               IMP = INFOINPUT(62) + 1
               INP = INFOINPUT(63) + 1
               if (IMP >= IXP .and. IMP < IXP + IW .and. &
                   INP >= IYP + 3 .and. INP < IYP + IH + 3 + 2) then
                  if (NBUT == 1) then
                     KEY = 21
                  else
                     KEY = 22
                  end if
               else
                  KEY = 23
               end if
            end if
         else if (KEY == -1) then
            KEY = INFOINPUT(57)
         end if
         if (KEY == 26) then
            WRDKEY = OPTION(IFEXIT / 2)
            call HELP(WRDKEY, NLEVEL)
         else if (KEY == 22 .or. KEY == 23) then
            if (KEY == 22) then
               call IFORMGETINTEGER(2, JAAUTO)
               JAAUTO = max(0, min(JAAUTO, 2))
               call IFORMGETINTEGER(4, NVN)
               call IFormGetDouble(6, VMINN)
               call IFormGetDouble(8, VMAXN)
               call IFormGetDouble(10, DVNU)
               call IFORMGETINTEGER(12, NISN)
               call IFORMGETINTEGER(14, NIEN)
               if (NV /= NVN .or. NIS /= NISN .or. NIE /= NIEN) then
                  NV = max(2, NVN)
                  NIS = max(1, min(NISN, 250))
                  NIE = max(NIS + NV + 1, min(NIEN, 254))
               end if

               call IFormGetDouble(16, XSC)
               call IFormGetDouble(18, YSC)
               XSC = max(0d0, min(XSC, 1d0))
               YSC = max(0d0, min(YSC, 1d0))
               call IFORMGETINTEGER(20, NDEC)
               if (NDEC > 7) NDEC = 7
               call IFormGetDouble(22, SCALESIZE)
               SCALESIZE = max(0d0, min(SCALESIZE, 1d0))

               if (DVNU /= DVI .or. VMAXN /= VMAX .or. VMINN /= VMIN) JAAUTO = 0

               if (JAAUTO == 0) then
                  DV = (NV - 1) * DVNU
                  if (VMIN /= VMINN .and. VMAX /= VMAXN) then
                     VMIN = VMINN
                     VMAX = VMAXN
                     DV = VMAX - VMIN
                  else if (VMAX /= VMAXN) then
                     VMAX = VMAXN
                     VMIN = VMAX - DV
                  else
                     VMIN = VMINN
                     VMAX = VMIN + DV
                  end if
                  do I = 1, NV
                     VAL(I) = VMIN + (I - 1) * DV / (NV - 1)
                  end do
               end if

               ! Secondary isocolour legend
               call IFORMGETINTEGER(26, JAAUTO2)
               JAAUTO2 = max(0, min(JAAUTO2, 1))
               call IFORMGETINTEGER(28, NVN)
               call IFormGetDouble(30, VMINN)
               call IFormGetDouble(32, VMAXN)
               call IFormGetDouble(34, DVNU)
               call IFORMGETINTEGER(36, NISN)
               call IFORMGETINTEGER(38, NIEN)
               if (NV2 /= NVN .or. NIS2 /= NISN .or. NIE2 /= NIEN) then
                  NV2 = max(2, NVN)
                  NIS2 = max(1, min(NISN, 250))
                  NIE2 = max(NIS2 + NV2 + 1, min(NIEN, 254))
               end if

               if (DVNU /= DVI2 .or. VMAXN /= VMAX2 .or. &
                   VMINN /= VMIN2) JAAUTO2 = 0

               if (JAAUTO2 == 0) then
                  DV2 = (NV2 - 1) * DVNU
                  if (VMIN2 /= VMINN .and. VMAX2 /= VMAXN) then
                     VMIN2 = VMINN
                     VMAX2 = VMAXN
                     DV2 = VMAX2 - VMIN2
                  else if (VMAX2 /= VMAXN) then
                     VMAX2 = VMAXN
                     VMIN2 = VMAX2 - DV2
                  else
                     VMIN2 = VMINN
                     VMAX2 = VMIN2 + DV2
                  end if
                  do I = 1, NV2
                     VAL2(I) = VMIN2 + (I - 1) * DV2 / (NV2 - 1)
                  end do
               end if
            end if
            call IWinClose(1)
            call IWinClose(1)
            call IWinClose(1)
            return
         else if (KEY == 21) then
            if (IFEXIT == 1 .or. IFEXIT == 3) then
               WRDKEY = HELPM(IFEXIT)
               call HELP(WRDKEY, NLEVEL)
            end if
         end if
         goto 30

      end

end module m_changeisoparameters
