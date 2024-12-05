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

module m_changetimeparameters

   implicit none

contains

   subroutine CHANGETIMEPARAMETERS()
      use m_flowtimes
      use unstruc_colors
      use unstruc_display_data
      use dflowfm_version_module, only: company, product_name
      use unstruc_messages, only: msgbuf, msg_flush
      use m_flow, only: squ2d, ndkx
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      use m_help
      use m_highlight_form_line
      implicit none
      integer :: numpar, numfld, numparactual, numfldactual
      parameter(NUMPAR=14, NUMFLD=2 * NUMPAR)
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
      integer, external :: infoinput
!
      integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
      integer :: nbut, imp, inp, ierr

      NLEVEL = 4
      i = 1
      OPTION(i) = 'Dt_user                             (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Dt_max                              (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Use automatic time step             ( ) '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'Tstart_user                         (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Tstop_user                          (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'HisInterval                         (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'MapInterval                         (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'RstInterval                         (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'WaqInterval                         (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Initial timestep                    (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Current time                        (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Smoothing time boundaries Tlfsmo    (s) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Dtfacmax                            ( ) '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Tspinupturblogprof                  ( ) '; it(2 * i) = 6; i = i + 1

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

      i = 1
      HELPM(i) = 'User timestep (rythm of external forcing updates)           '; i = i + 1
      HELPM(i) = 'Max timestep                                                '; i = i + 1
      HELPM(i) = ' 1=2D V/Qouth, 3=3D Vk/Qouthk, 5=3D Vk/Qouhvk, 8=5, kt-1    '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1
      HELPM(i) = 'dt = min(dtnew, dtfacmax*dtold)                             '; i = i + 1
      HELPM(i) = '                                                            '; i = i + 1

      call SAVEKEYS()

      NUMPARACTUAL = NUMPAR
      NUMFLDACTUAL = 2 * NUMPARACTUAL

      IR = 0
      do I = 1, NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         IX(IL) = 13
         IX(IR) = 95
         IY(IL) = 2 * I
         IY(IR) = 2 * I
         IS(IL) = 82
         IS(IR) = 10
         IT(IL) = 1001
      end do

!  Initialise
      call IWinWordWrap('OFF')
      call ITEXTCOLOURN(HLPFOR, HLPBCK)
      call INHIGHLIGHT('WHITE', 'RED')
      IW = NPOS(3)
      IXP = NPOS(1) + (IWS - IW) / 2
      IYP = NPOS(2)
      IH = IHS - 9

!  Header of filewindow
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP, IW, 1)
      call ITEXTCOLOURN(LBLFOR, LBLBCK)
      call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' PARAMETER FORM')
      call ITEXTCOLOURN(HLPFOR, HLPBCK)
!
!  Explain keyfunctions in bottom window
      call IWinAction('FPC')
      call IWinOpen(IXP, IHS - 1, IW, 2)
      call IWinOutStringXY(1, 1, 'move = ., Tab, confirm = Enter, no change = Esc, help = F3')
      call IWinOutStringXY(1, 2, 'right mouse = Enter, click outside window = Esc')

!  Filewindow is middelste window
      call IWinAction('FPC')
      call IWinOpen(IXP, IYP + 3, IW, IH)

      call InControlKey(29, 129)
      call InControlKey(30, 128)

!  NUMWNH = InfoWindow(1)
!  CALL IWinSelect(NUMWNH)

!  Define a new form by supplying arrays containing
!  field positions, sizes and types
      call IFORMDEFINE('W', NUMFLDACTUAL, IX, IY, IS, IT)

!  Define a help field and define help strings
!  for 2 of the 4 input fields
      call IFORMHELP(13, IH, 60)

      IR = 0
      do I = 1, NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         call IFORMPUTSTRING(IL, OPTION(I))
         call IFORMPUTHELP(IR, HELPM(I))
         call IFORMATTRIBUTEN(IR, 0, 0, 7)
      end do

      i = 1
      call IFormPutDouble(2 * i, dt_user, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, dt_max, '(F10.3)'); i = i + 1
      call IFORMPUTINTEGER(2 * i, ja_timestep_auto); i = i + 1
      call IFormPutDouble(2 * i, tstart_user, '(F10.0)'); i = i + 1
      call IFormPutDouble(2 * i, tstop_user, '(F10.0)'); i = i + 1
      call IFormPutDouble(2 * i, ti_his, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, ti_map, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, ti_rst, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, ti_waq, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, dt_init, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, time1, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, Tlfsmo, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, Dtfacmax, '(F10.3)'); i = i + 1
      call IFormPutDouble(2 * i, Tspinupturblogprof, '(F10.3)'); i = i + 1

      ! Display the form with numeric fields left justified
      ! and set the initial field to number 2
      call IOUTJUSTIFYNUM('L')
      IFEXIT = 2
      call IFormAttribute(IFEXIT - 1, 'BU', ' ', ' ')
      call IFORMSHOW()

30    continue
      IFINIT = IFEXIT
      call IFormEditUser(IFINIT, IFEXIT, highlight_form_line)
!  check for Help, Confirm, Quit
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

            i = 1
            call IFormGetDouble(2 * i, dt_user); i = i + 1
            call IFormGetDouble(2 * i, dt_max); i = i + 1
            call IFORMgeTINTEGER(2 * i, ja_timestep_auto); i = i + 1
            call IFormGetDouble(2 * i, tstart_user); i = i + 1
            call IFormGetDouble(2 * i, tstop_user); i = i + 1
            call IFormGetDouble(2 * i, ti_his); i = i + 1
            call IFormGetDouble(2 * i, ti_map); i = i + 1
            call IFormGetDouble(2 * i, ti_rst); i = i + 1
            call IFormGetDouble(2 * i, ti_waq); i = i + 1
            call IFormGetDouble(2 * i, dt_init); i = i + 1
            call IFormGetDouble(2 * i, Time1); i = i + 1
            call IFormGetDouble(2 * i, Tlfsmo); i = i + 1
            call IFormGetDouble(2 * i, Dtfacmax); i = i + 1
            call IFormGetDouble(2 * i, Tspinupturblogprof); i = i + 1

            if (dt_max > dt_user) then
               dt_max = dt_user
               write (msgbuf, '(a,f9.6,a)') 'DtMax should be <= DtUser. It has been reset to: ', dt_max
               call msg_flush()
            end if

            if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4) then
               if (.not. allocated(Squ2D)) allocate (squ2D(ndkx), stat=ierr)
            end if
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

   end subroutine CHANGETIMEPARAMETERS

end module m_changetimeparameters
