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

!> spline2curvi parameter menu
subroutine change_spline2curvi_param(jacancelled)
   use M_GRIDSETTINGS
   use unstruc_display
   use dflowfm_version_module, only: company, product_name
   use m_spline2curvi
   use m_helpnow
   use m_save_keys
   use m_restore_keys

   implicit none
   integer, intent(out) :: jacancelled !< Whether or not (1/0) user has pressed 'Esc' in parameter screen.

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
   integer :: numfldactual
   integer :: numparactual

   integer, parameter :: NUMPAR = 14, NUMFLD = 2 * NUMPAR
   integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
   character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
   integer, external :: infoinput
   external :: highlight_form_line

   jacancelled = 0
   NLEVEL = 4

   OPTION(1) = 'MAXIMUM NUMBER OF GRIDCELLS ALONG SPLINE'; IT(1 * 2) = 2
   OPTION(2) = 'MAXIMUM NUMBER OF GRIDCELLS PERP. SPLINE'; IT(2 * 2) = 2
   OPTION(3) = 'ASPECT RATIO OF FIRST GRID LAYER        '; IT(3 * 2) = 6
   OPTION(4) = 'GRID LAYER HEIGHT GROWTH FACTOR         '; IT(4 * 2) = 6
   OPTION(5) = 'MAXIMUM GRID LENGTH ALONG CENTER SPLINE '; IT(5 * 2) = 6
   OPTION(6) = 'CURVATURE-ADAPTED GRID SPACING     (0,1)'; IT(6 * 2) = 2
   OPTION(7) = 'GROW GRID OUTSIDE FIRST PART       (0,1)'; IT(7 * 2) = 2
   OPTION(8) = 'MAX. NUM. OF GRIDCELL PERP. IN UNI. PART'; IT(8 * 2) = 2
   OPTION(9) = '                                        '; IT(9 * 2) = 2
   OPTION(10) = 'GRIDPTS. ON TOP OF EACH OTHER TOLERANCE '; IT(10 * 2) = 6
   OPTION(11) = 'MINIMUM ABS. SINE OF CROSSING ANGLES   '; IT(11 * 2) = 6
   OPTION(12) = 'PREVENT COLL.S W/OTHER GRIDPARTS  (0,1) '; IT(12 * 2) = 2
   OPTION(13) = '                                        '; IT(13 * 2) = 2
   OPTION(14) = 'UNIFORM GRIDSIZE (NETBND2GRID ONLY) (m) '; IT(14 * 2) = 6
!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
   HELPM(1) = 'INTEGER VALUE <                                             '
   HELPM(2) = 'INTEGER VALUE <                                             '
   HELPM(3) = 'REAL    VALUE <                                             '
   HELPM(4) = 'REAL    VALUE <                                             '
   HELPM(5) = 'REAL    VALUE <                                             '
   HELPM(6) = 'INTEGER VALUE <                                             '
   HELPM(7) = 'INTEGER VALUE <                                             '
   HELPM(8) = 'INTEGER VALUE <                                             '
   HELPM(9) = '                                                            '
   HELPM(10) = 'REAL    VALUE <                                             '
   HELPM(11) = 'REAL    VALUE <                                             '
   HELPM(12) = 'INTEGER VALUE <                                             '
   HELPM(13) = '                                                            '
   HELPM(14) = 'REAL    VALUE <                                             '

   call SAVEKEYS()
   NUMPARACTUAL = NUMPAR
   NUMFLDACTUAL = 2 * NUMPARACTUAL

   IR = 0
   do I = 1, NUMPARACTUAL
      IL = IR + 1; IR = IL + 1
      IS(IL) = 82; IS(IR) = 10
      IX(IL) = 10; IX(IR) = 100
      IY(IL) = 2 * I; IY(IR) = 2 * I
      IT(IL) = 1001 ! ir staat hierboven
   end do

   ! Initialise
   call IWinWordWrap('OFF')
   call ITEXTCOLOURN(HLPFOR, HLPBCK)
   call INHIGHLIGHT('WHITE', 'RED')
   IW = NPOS(3)
   IXP = NPOS(1) + (IWS - IW) / 2
   IYP = NPOS(2)
   IH = IHS - 9

   ! Header of filewindow
   call IWinAction('FPC')
   call IWinOpen(IXP, IYP, IW, 1)
   call ITEXTCOLOURN(LBLFOR, LBLBCK)
   call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' PARAMETER FORM')
   call ITEXTCOLOURN(HLPFOR, HLPBCK)

   ! Explain keyfunctions in bottom window
   call IWinAction('FPC')
   call IWinOpen(IXP, IHS - 1, IW, 2)
   call IWinOutStringXY(1, 1, 'move = , Tab, confirm = Enter, no change = Esc, help = F3')
   call IWinOutStringXY(1, 2, 'right mouse = Enter, click outside window = Esc')

   ! Filewindow is middelste window
   call IWinAction('FPC')
   call IWinOpen(IXP, IYP + 3, IW, IH)
   call InControlKey(29, 129)
   call InControlKey(30, 128)

   ! Define a new form by supplying arrays containing Field positions, sizes and types
   call IFORMDEFINE('W', NUMFLDACTUAL, IX, IY, IS, IT)

   ! Define a help field and define help strings for 2 of the 4 input fields
   call IFORMHELP(13, IH, 60)

   IR = 0
   do I = 1, NUMPARACTUAL
      IL = IR + 1
      IR = IL + 1
      call IFORMPUTSTRING(IL, OPTION(I))
      call IFORMPUTHELP(IR, HELPM(I))
      call IFORMATTRIBUTEN(IR, 0, 0, 7)
   end do

   call IFORMPUTINTEGER(2 * 1, MFAC)
   call IFORMPUTINTEGER(2 * 2, NFAC)
   call IFORMPUTDOUBLE(2 * 3, daspect, '(F7.3)')
!   CALL IFORMPUTDOUBLE  (2*4, maxaspect, '(F7.3)')
   call IFORMPUTDOUBLE(2 * 4, dgrow, '(F7.3)')
   call IFORMPUTDOUBLE(2 * 5, dwidth, '(F11.3)')
   call IFORMPUTINTEGER(2 * 6, jacurv)
   call IFORMPUTINTEGER(2 * 7, jaoutside)
   call IFORMPUTINTEGER(2 * 8, NFACUNIMAX)
!   CALL IFORMPUTINTEGER (2*9, idum               )
   call IFORMPUTDOUBLE(2 * 10, dtolLR, '(F11.5)')
   call IFORMPUTDOUBLE(2 * 11, dtolcos, '(F11.3)')
   call IFORMPUTINTEGER(2 * 12, jaCheckFrontCollision)
   call IFORMPUTDOUBLE(2 * 14, dunigridsize, '(F11.3)')

   NFAC = max(NFAC, NFACUNIMAX)

   ! Display the form with numeric fields left justified and set the initial field to number 2
   call IOUTJUSTIFYNUM('L')
   IFEXIT = 2
   call IFormAttribute(IFEXIT - 1, 'BU', ' ', ' ')
   call IFORMSHOW()

30 continue
   IFINIT = IFEXIT
   call IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
   ! check for Help, Confirm, Quit
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
         call IFORMGETINTEGER(2 * 1, MFAC)
         call IFORMGETINTEGER(2 * 2, NFAC)
         call IFORMGETDOUBLE(2 * 3, daspect)
!           CALL IFORMGETDOUBLE  (2*4 , maxaspect)
         call IFORMGETDOUBLE(2 * 4, dgrow)
         call IFORMGETDOUBLE(2 * 5, dwidth)
         call IFORMGETINTEGER(2 * 6, jacurv)
         call IFORMGETINTEGER(2 * 7, jaoutside)
         call IFORMGETINTEGER(2 * 8, NFACUNIMAX)
!           CALL IFORMGETINTEGER (2*9 , idum)
         call IFORMGETDOUBLE(2 * 10, dtolLR)
         call IFORMGETDOUBLE(2 * 11, dtolcos)
         call IFORMGETINTEGER(2 * 12, jaCheckFrontCollision)
         call IFORMGETDOUBLE(2 * 14, dunigridsize)
      elseif (KEY == 23) then
         jacancelled = 1
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

end subroutine change_spline2curvi_param
