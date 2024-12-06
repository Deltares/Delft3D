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

module m_changenumericalparameters
   use m_confrm

   implicit none

contains

   subroutine CHANGENUMERICALPARAMETERS()
      use m_flow
      use unstruc_colors
      use unstruc_display_data
      use m_reduce, only: epscg
      use dflowfm_version_module, only: company, product_name
      use m_fixedweirs, only: nfxw
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      use m_help
      use m_highlight_form_line

      integer :: numpar, numfld, numparactual, numfldactual
      parameter(NUMPAR=24, NUMFLD=2 * NUMPAR)
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
      integer, external :: infoinput
!
      integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key, ja, niadvec
      integer :: nbut, imp, inp

      NLEVEL = 4
      OPTION(1) = 'COURANT NR                           ( )'; it(2 * 1) = 6
      OPTION(2) = 'IADVEC                                  '; it(2 * 2) = 2
      OPTION(3) = 'IADVEC1D                                '; it(2 * 3) = 2
      OPTION(4) = 'Limtyp scalar   transport               '; it(2 * 4) = 2
      OPTION(5) = 'Limtyp hu                               '; it(2 * 5) = 2
      OPTION(6) = 'Limtyp momentum transport               '; it(2 * 6) = 2
      OPTION(7) = 'itstep                                  '; it(2 * 7) = 2
      OPTION(8) = 'teta                                ( ) '; it(2 * 8) = 6
      OPTION(9) = 'icgsolver                           ( ) '; it(2 * 9) = 2
      OPTION(10) = 'Transport Method                    ( ) '; it(2 * 10) = 2
      OPTION(11) = 'Salinity included 0/1               ( ) '; it(2 * 11) = 2
      OPTION(12) = 'Temperature model nr, 0=no, 5=heatflx() '; it(2 * 12) = 2
      OPTION(13) = 'Anti creep                          ( ) '; it(2 * 13) = 2
      OPTION(14) = '                                    ( ) '; it(2 * 14) = 6
      OPTION(15) = 'irov 0,1,2,3                        ( ) '; it(2 * 15) = 2
      OPTION(16) = 'icorio, 0, 5=org def., even=2D weigh( ) '; it(2 * 16) = 2
      OPTION(17) = 'jatidep tidal potential forcing 0/1 ( ) '; it(2 * 17) = 2
      OPTION(18) = 'EpsCG, CG solver stop criterion     ( ) '; it(2 * 18) = 6
      OPTION(19) = 'Epshu, flooding criterion           (m) '; it(2 * 19) = 6
      OPTION(20) = 'JaExplicitsinks                     ( ) '; it(2 * 20) = 2
      OPTION(21) = 'Corioadamsbashfordfac               ( ) '; it(2 * 21) = 6
      OPTION(22) = 'Newcorio                            ( ) '; it(2 * 22) = 2
      OPTION(23) = 'Barocterm                           ( ) '; it(2 * 23) = 2
      OPTION(24) = 'Barocadamsbashfordfac               ( ) '; it(2 * 24) = 6

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

      HELPM(1) = 'Total    COURANT                                            '
      HELPM(2) = '0=N0, 33=Full Perot, 1=wenn tot, 2=wenn inoutdif, 3 =       '
      HELPM(3) = 'see iadvec                                                  '
      HELPM(4) = '0=No, 1=Minmod, 2=VanLeer, 3=Koren, 4=Monotonized Central  '
      HELPM(5) = '0=No, 1=Minmod, 2=VanLeer, 3=Koren, 4=Monotonized Central, 21=central'
      HELPM(6) = '0=No, 1=Minmod, 2=VanLeer, 3=Koren, 4=Monotonized Central  '
      HELPM(7) = '2=implicit pressure, 1=no pressure, 0 = only transport      '
      HELPM(8) = '0.5 < teta =< 1.0                                           '
      HELPM(9) = '1 = GS_OMP, 2 = GS_OMPthreadsafe, 3 = GS, 4 = SaadILUD      '
      HELPM(10) = '0=Herman transport, 1=transport module (default), 2=no      '
      HELPM(11) = '0=no salinity, 1=yes salinity                               '
      HELPM(12) = 'Temperature model nr, 0=no temp, 5=heat flux 3=excess       '
      HELPM(13) = '0=No, 1=Yes anticreep  only in sigma layers                 '
      HELPM(14) = 'default 1d-8                                                '
      HELPM(15) = '0=free slip, 1 =partial slip, 2=no slip, 3 =hydraul. smooth '
      HELPM(16) = '0=no 5=default, 3,4 no weights, 5-10 Olga, 25-30 Ham        '
      HELPM(17) = '0=no tidal potential, 1=yes tidal potential                 '
      HELPM(18) = 'Guus, if max(abs(r/rk) < epscg , or Saad L2norm < epscg     '
      HELPM(19) = 'hu > epshu: link flows                                      '
      HELPM(20) = '1=expl, 0 = impl                                            '
      HELPM(21) = '>0 = Adams Bashford, standard= 0.5, only for Newcorio=1     '
      HELPM(22) = '0=prior to 27-11-2019, 1=no normal forcing on open bnds, 12#'
      HELPM(23) = '3=default, 4=new                                            '
      HELPM(24) = '>0 = Adams Bashford, standard= 0.5, only for Baroctimeint=4 '

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

      NIADVEC = IADVEC
      call IFormPutDouble(2 * 1, CFLmx, '(F8.3)')
      call IFORMPUTINTEGER(2 * 2, NIADVEC)
      call IFORMPUTINTEGER(2 * 3, IADVEC1D)
      call IFORMPUTINTEGER(2 * 4, Limtypsa)
      call IFORMPUTINTEGER(2 * 5, Limtyphu)
      call IFORMPUTINTEGER(2 * 6, Limtypmom)
      call IFORMPUTINTEGER(2 * 7, itstep)
      call IFormPutDouble(2 * 8, teta0, '(F10.3)')
      call IFORMPUTinteger(2 * 9, icgsolver)
      call IFORMPUTinteger(2 * 11, jasal)
      call IFORMPUTinteger(2 * 12, jatem)
      call IFORMPUTinteger(2 * 13, jacreep)
      call IFORMPUTdouble(2 * 14, epsmaxlev, '(e10.5)')
      call IFORMPUTinteger(2 * 15, irov)
      call IFORMPUTinteger(2 * 16, icorio)
      call IFORMPUTinteger(2 * 17, jatidep)
      call IFormPutDouble(2 * 18, epscg, '(e10.5)')
      call IFormPutDouble(2 * 19, epshu, '(e10.5)')
      call IFORMPUTinteger(2 * 20, jaexplicitsinks)
      call IFormputDouble(2 * 21, Corioadamsbashfordfac, '(e10.5)')
      call IFormputinteger(2 * 22, Newcorio)
      call IFormputinteger(2 * 23, Jabarocterm)
      call IFormputDouble(2 * 24, Barocadamsbashfordfac, '(e10.5)')

      !  Display the form with numeric fields left justified
      !  and set the initial field to number 2
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

            call IFormgetDouble(2 * 1, CFLmx)
            call IFORMgeTINTEGER(2 * 2, NIADVEC)
            call IFORMgeTINTEGER(2 * 3, IADVEC1D)
            call IFORMgeTINTEGER(2 * 4, Limtypsa); limtypsa = max(0, min(limtypsa, 30))
            call IFORMgeTINTEGER(2 * 5, Limtyphu); limtyphu = max(0, min(limtyphu, 30))
            call IFORMgeTINTEGER(2 * 6, Limtypmom); limtypmom = max(0, min(limtypmom, 30))
            call IFORMgeTINTEGER(2 * 7, itstep)
            call IFormgetDouble(2 * 8, teta0)
            call IFORMgeTinteger(2 * 9, icgsolver)
            call IFORMgeTinteger(2 * 11, jasal)
            call IFORMgeTinteger(2 * 12, jatem)
            call IFORMgeTinteger(2 * 13, jacreep)
            call IFORMgeTdouble(2 * 14, epsmaxlev)
            call IFORMgeTinteger(2 * 15, irov)
            call IFORMgeTinteger(2 * 16, icorio)
            call IFORMgeTinteger(2 * 17, jatidep)
            call IFormgetDouble(2 * 18, epscg)
            call IFormgetDouble(2 * 19, epshu)
            call IFORMgeTinteger(2 * 20, jaexplicitsinks)
            call IFormgetDouble(2 * 21, Corioadamsbashfordfac)
            call IFormgetinteger(2 * 22, Newcorio)
            call IFormgetinteger(2 * 23, Jabarocterm)
            call IFormgetDouble(2 * 24, Barocadamsbashfordfac)

            epshs = 0.2d0 * epshu ! minimum waterdepth for setting cfu
            if (niadvec /= iadvec) then
               if (nfxw > 0) then
                  call confrm('If Fixedweirs present, please reinitialise the model', ja)
               end if
               iadvec = niadvec
               call iadvecini()
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

   end subroutine CHANGENUMERICALPARAMETERS

end module m_changenumericalparameters
