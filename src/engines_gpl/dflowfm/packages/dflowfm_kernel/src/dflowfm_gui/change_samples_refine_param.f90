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

!> refinecellsandfaces2 parameter menu
module m_change_samples_refine_param

implicit none

contains

subroutine change_samples_refine_param(jacancelled)
   use unstruc_colors
   use unstruc_display_data
   use dflowfm_version_module, only: company, product_name
   use m_samples_refine
   use m_ec_interpolationsettings, only: interpolationtype
   use network_data, only: NUMITCOURANT
   use m_helpnow
   use m_save_keys
   use m_restore_keys
   use m_help
   use m_highlight_form_line

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

   integer, parameter :: NUMPAR = 16, NUMFLD = 2 * NUMPAR
   integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
   character OPTION(NUMPAR) * 60, HELPM(NUMPAR) * 60
   character(len=60) :: text
   integer, external :: infoinput

   jacancelled = 0
   NLEVEL = 4

   text = ''
   write (text, "('TYPE: RIDGES (', I1, '), WAVE COURANT NUMBER (', I1,  ')')") ITYPE_RIDGE, ITYPE_WAVECOURANT

   OPTION(1) = text; IT(1 * 2) = 2
   OPTION(2) = ''; IT(2 * 2) = 0
   OPTION(3) = 'RIDGE DETECTION'; IT(3 * 2) = 0
   OPTION(4) = 'CELL SIZE * TYPICAL OBSTACLE HEIGHT   [m2]'; IT(4 * 2) = 6
   OPTION(5) = 'MINIMUM     TYPICAL OBSTACLE HEIGHT   [m] '; IT(5 * 2) = 6
   OPTION(6) = 'MINIMUM CELL EDGE LENGTH              [m] '; IT(6 * 2) = 6
   OPTION(7) = 'NUMBER OF SAMPLE SMOOTHING ITERATIONS [-] '; IT(7 * 2) = 2
   OPTION(8) = '                                          '; IT(8 * 2) = 0
   OPTION(9) = 'WAVE COURANT NUMBER                       '; IT(9 * 2) = 0
   OPTION(10) = 'MAXIMUM TIME-STEP        Dt_maxcour   [s] '; IT(10 * 2) = 6
   OPTION(11) = 'MINIMUM CELL EDGE LENGTH Dx_mincour   [m] '; IT(11 * 2) = 6
   OPTION(12) = 'DIRECTIONAL REFINEMENT (1) OR NOT (0)     '; IT(12 * 2) = 2
   OPTION(13) = 'USE SAMPLES OUTSIDE CELL (1) OR NOT (0)   '; IT(13 * 2) = 2
   OPTION(14) = 'Number of non-interactive refine cycles ()'; IT(14 * 2) = 2
   OPTION(15) = 'Interpolationtype 2 or 4                ()'; IT(15 * 2) = 2
   OPTION(16) = 'Numitcourant smoothing cycles           ()'; IT(16 * 2) = 2

   HELPM(1) = 'INTEGER VALUE <                                             '
   HELPM(2) = '                                                            '
   HELPM(3) = '                                                            '
   HELPM(4) = 'REAL    VALUE <                                             '
   HELPM(5) = 'REAL    VALUE <                                             '
   HELPM(6) = 'REAL    VALUE <                                             '
   HELPM(7) = 'INTEGER VALUE <                                             '
   HELPM(8) = '                                                            '
   HELPM(9) = '                                                            '
   HELPM(10) = 'REAL    VALUE <                                             '
   HELPM(11) = 'REAL    VALUE <                                             '
   HELPM(12) = 'INTEGER VALUE <                                             '
   HELPM(13) = 'INTEGER VALUE <                                             '
   HELPM(14) = '0=interactive, > 0=automatic nr of ref. cycles              '
   HELPM(15) = '2=averaging, 4=bilinarc                                     '
   HELPM(16) = '                                                            '

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

      if (IT(IR) == 0) then
         IS(IR) = 0
      end if
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

   call IFORMPUTINTEGER(2 * 1, irefinetype, '(F12.3)')
   call IFORMPUTDOUBLE(2 * 4, threshold, '(F12.3)')
   call IFORMPUTDOUBLE(2 * 5, thresholdmin, '(F12.3)')
   call IFORMPUTDOUBLE(2 * 6, hmin, '(F12.3)')
   call IFORMPUTINTEGER(2 * 7, Nsamplesmooth)

   call IFORMPUTDOUBLE(2 * 10, Dt_maxcour, '(F12.3)')
   call IFORMPUTDOUBLE(2 * 11, Dx_mincour, '(F12.3)')
   call IFORMPUTINTEGER(2 * 12, jadirectional)
   call IFORMPUTINTEGER(2 * 13, jaoutsidecell)
   call IFORMPUTINTEGER(2 * 14, numrefcycles)
   call IFORMPUTINTEGER(2 * 15, interpolationtype)
   call IFORMPUTINTEGER(2 * 16, numitcourant)

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
         call IFORMGETINTEGER(2 * 1, irefinetype)
         call IFORMGETDOUBLE(2 * 4, threshold)
         call IFORMGETDOUBLE(2 * 5, thresholdmin)
         call IFORMGETDOUBLE(2 * 6, hmin)
         call IFORMGETINTEGER(2 * 7, Nsamplesmooth)

         call IFORMGETDOUBLE(2 * 10, Dt_maxcour)
         call IFORMGETDOUBLE(2 * 11, Dx_mincour)
         call IFORMGETINTEGER(2 * 12, jadirectional)
         call IFORMGETINTEGER(2 * 13, jaoutsidecell)
         call IFORMGETINTEGER(2 * 14, numrefcycles)
         call IFORMGETINTEGER(2 * 15, interpolationtype)
         call IFORMGETINTEGER(2 * 16, numitcourant)

      elseif (KEY == 23) then
         jacancelled = 1
      end if
      call IWinClose(1)
      call IWinClose(1)
      call IWinClose(1)
      call RESTOREKEYS()
      goto 1234
   else if (KEY == 21) then
      if (IFEXIT == 1 .or. IFEXIT == 3) then
         WRDKEY = HELPM(IFEXIT)
         call HELP(WRDKEY, NLEVEL)
      end if
   end if
   goto 30

1234 continue
   if (Nsamplesmooth /= Nsamplesmooth_last) then
      iHesstat = iHesstat_DIRTY
   end if

   return
end subroutine change_samples_refine_param

end module m_change_samples_refine_param
