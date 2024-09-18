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

      subroutine TEXTPARAMETERS()
         use unstruc_display
         use dflowfm_version_module, only: company, product_name
         use m_helpnow
         use m_textim
         use m_save_keys
         use m_restore_keys
         
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
         integer :: key
         integer :: nbut
         integer :: numfld
         integer :: numpar
         integer, external :: infoinput
         external :: highlight_form_line

         parameter(NUMPAR=9, NUMFLD=2 * NUMPAR)
         integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
         character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
!
         NLEVEL = 3
         OPTION(1) = 'LINE 1:'
         OPTION(2) = 'LINE 2:'
         OPTION(3) = 'LINE 3:'
         OPTION(4) = 'FNTSIZ:'
         OPTION(5) = 'XPOS  :'
         OPTION(6) = 'YPOS  :'
         OPTION(7) = 'SIZE TM'
         OPTION(8) = 'XPOS TM'
         OPTION(9) = 'YPOS TM'

!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
         HELPM(1) = 'FIRST TEXTLINE                                              '
         HELPM(2) = 'SECOND TEXTLINE                                             '
         HELPM(3) = 'THIRD TEXTLINE                                              '
         HELPM(4) = 'FONTSIZE, ONLY FOR TEXTLINES, DEFAULT FONTSIZE = 0.5        '
         HELPM(5) = 'RELATIVE SCREEN X POSITION, 0 = LEFT, 1 = RIGHT             '
         HELPM(6) = 'RELATIVE SCREEN Y POSITION, 0 = BOTTOM, 1 = TOP             '
         HELPM(7) = 'FONTSIZE, FOR TIME/DATE IN ANIMATE INCREMENTAL              '
         HELPM(8) = 'SCREEN X POSITION, FOR TIME/DATE IN ANIMATE INCREMENTAL     '
         HELPM(9) = 'SCREEN Y POSITION, FOR TIME/DATE IN ANIMATE INCREMENTAL     '

         IR = 0
         do I = 1, NUMPAR
            IL = IR + 1
            IR = IL + 1
            IX(IL) = 2
!         IX(IR) = 14
            IX(IR) = 56
            IY(IL) = 2 * I
            IY(IR) = 2 * I
!         IS(IL) = 40
            IS(IL) = 82
            IS(IR) = 60
            IT(IL) = 1001
            IT(IR) = 1
            if (I >= 4) then
               IS(IR) = 5
               IT(IR) = 6
            end if
         end do

         call SAVEKEYS()
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
         call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' PARAMETER FORM')
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

         call IFORMPUTSTRING(2 * 1, TXLIN(1))
         call IFORMPUTSTRING(2 * 2, TXLIN(2))
         call IFORMPUTSTRING(2 * 3, TXLIN(3))
         call IFormPutDouble(2 * 4, TXSIZE, '(F5.2)')
         call IFormPutDouble(2 * 5, TXXpos, '(F5.2)')
         call IFormPutDouble(2 * 6, TXYpos, '(F5.2)')
         call IFormPutDouble(2 * 7, TXTIMSIZE, '(F5.2)')
         call IFormPutDouble(2 * 8, TXTIMX, '(F5.2)')
         call IFormPutDouble(2 * 9, TXTIMY, '(F5.2)')

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
               call IFORMGETSTRING(2 * 1, TXLIN(1))
               call IFORMGETSTRING(2 * 2, TXLIN(2))
               call IFORMGETSTRING(2 * 3, TXLIN(3))
               call IFormGetDouble(2 * 4, TXSIZE)
               call IFormGetDouble(2 * 5, TXXpos)
               call IFormGetDouble(2 * 6, TXYpos)
               call IFormGetDouble(2 * 7, TXTIMSIZE)
               call IFormGetDouble(2 * 8, TXTIMX)
               call IFormGetDouble(2 * 9, TXTIMY)
               TXSIZE = max(0d0, min(TXSIZE, 10d0))
               TXXpos = max(0d0, min(TXXpos, 1d0))
               TXYpos = max(0d0, min(TXYpos, 1d0))
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

      end
