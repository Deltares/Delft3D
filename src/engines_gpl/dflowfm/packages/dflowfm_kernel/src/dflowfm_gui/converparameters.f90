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

!----------------------------------------------------------------------
! subroutines from rgfstuff.f90
!----------------------------------------------------------------------
      subroutine CONVERPARAMETERS(JA)
         use M_MAPPROPARAMETERS
         use unstruc_display
         use m_sferic
         implicit none
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
         integer :: ja
         integer :: key
         integer :: l
         integer :: nbut
         integer :: nlevel
         integer :: numfld
         integer :: numpar
         parameter(NUMPAR=10, NUMFLD=2 * NUMPAR)
         integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
         character WRDKEY * 40, OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60, TEX * 132
         common / HELPNOW / WRDKEY, NLEVEL
         integer, external :: infoinput
         external :: highlight_form_line

!
         JA = 0
         NLEVEL = 3
         OPTION(1) = 'Type of Map Projection (0,1,2,3,4,-1)   '
         OPTION(2) = 'UTM Zone Nr (1-60)                      '
         OPTION(3) = 'Northern (1) or southern (0) hemisphere '
         OPTION(4) = 'Offset X-Direction                      '
         OPTION(5) = 'Offset Y-Direction                      '
         OPTION(6) = 'Rotation Left (deg)                     '
         OPTION(7) = 'X Scalefactor                           '
         OPTION(8) = 'Y Scalefactor                           '
         OPTION(9) = 'X centrepoint (deg) for stereographic   '
         OPTION(10) = 'Y centrepoint (deg) for stereographic   '

!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
         HELPM(1) = '0=Trans/Rot,1=UTM,2=Amer,3=RD(Parijs),4=MERC,-1=AFFINE.XYX  '
         HELPM(2) = 'Usually 0, Except When Type = 1 (UTM) and Cartesian         '
         HELPM(3) = 'Only used for UTM->latlon conversion                        '
         HELPM(4) = 'X = X + Offset X-Direction, Real Value (m) (Only for Type=0)'
         HELPM(5) = 'Y = Y + Offset Y-Direction, Real Value (m) (Only for Type=0)'
         HELPM(6) = 'Rotationcenter = Center of Grid            (Only for Type=0)'
         HELPM(7) = 'Dimensionsless ()                          (Only for Type=0)'
         HELPM(8) = 'Dimensionsless ()                          (Only for Type=0)'
         HELPM(9) = 'Degrees                                    (Only for Type=5)'
         HELPM(10) = 'Degrees                                    (Only for Type=5)'

         call SAVEKEYS()

         IR = 0
         do I = 1, NUMPAR
            IL = IR + 1
            IR = IL + 1
            IX(IL) = 13
!         IX(IR) = 53
            IX(IR) = 95
            IY(IL) = 2 * I
            IY(IR) = 2 * I
            IS(IL) = 82
            IS(IR) = 10
            IS(IR) = 10
            IT(IL) = 1001
            if (I <= 3) then
               IT(IR) = 2
            else
               IT(IR) = 6
            end if
         end do

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

         if (JSFERIC == 1) then
            TEX = 'Conversion from Spherical (deg) to Cartesian (m) Coordinates'
         else
            TEX = 'Conversion from Cartesian (m) to Spherical (deg) Coordinates'
         end if
         L = len_trim(TEX)
         call IWinOutCentre(1, TEX(1:L))

         call ITEXTCOLOURN(HLPFOR, HLPBCK)
!
!     Explain keyfunctions in bottom window
         call IWinAction('FPC')
         call IWinOpen(IXP, IHS - 1, IW, 2)
         call IWinOutStringXY(1, 1, 'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
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
         do i = 1, NUMPAR
            IL = IR + 1
            IR = IL + 1
            call IFORMPUTSTRING(IL, OPTION(I))
            call IFORMPUTHELP(IR, HELPM(I))
            call IFORMATTRIBUTEN(IR, 0, 0, 7)
         end do
         call IFORMPUTINTEGER(1 * 2, ITYPE)
         call IFORMPUTINTEGER(2 * 2, IZONE)
         call IFORMPUTINTEGER(3 * 2, IHEM)
         call IFormPutDouble(4 * 2, DELTX, '(F10.3)')
         call IFormPutDouble(5 * 2, DELTY, '(F10.3)')
         call IFormPutDouble(6 * 2, FI, '(F10.3)')
         call IFormPutDouble(7 * 2, XF, '(F10.3)')
         call IFormPutDouble(8 * 2, YF, '(F10.3)')
         call IFormPutDouble(9 * 2, xcstereo, '(F10.3)')
         call IFormPutDouble(10 * 2, ycstereo, '(F10.3)')

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
               call IFORMGETINTEGER(1 * 2, ITYPE)
               call IFORMGETINTEGER(2 * 2, IZONE)
               call IFORMGETINTEGER(3 * 2, IHEM)
               call IFormGetDouble(4 * 2, DELTX)
               call IFormGetDouble(5 * 2, DELTY)
               call IFormGetDouble(6 * 2, FI)
               call IFormGetDouble(7 * 2, XF)
               call IFormGetDouble(8 * 2, YF)
               call IFormGetDouble(9 * 2, Xcstereo)
               call IFormGetDouble(10 * 2, ycstereo)
               CSE = cos(DG2RD * FI)
               SNE = sin(DG2RD * FI)
               JA = 1
            end if
            call IWinClose(1)
            call IWinClose(1)
            call IWinClose(1)
            call RESTOREKEYS()
            return
         else if (KEY == 21) then
            if (IFEXIT == 1 .or. IFEXIT == 3) then
               WRDKEY = HELPM(IFEXIT)
               call HELP(WRDKEY, NLEVEL)
            end if
         end if
         goto 30

      end subroutine CONVERPARAMETERS
