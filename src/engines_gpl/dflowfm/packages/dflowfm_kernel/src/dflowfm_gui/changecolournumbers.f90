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

 subroutine CHANGEcolournumbers()
    use m_netw
    use M_FLOW
    use m_flowgeom
    use m_sferic
    use m_wind
    use unstruc_display
    use m_reduce
    use dflowfm_version_module, only: company, product_name
    use unstruc_messages
    use m_fixedweirs
    use m_observations
    implicit none

    integer :: numpar, numfld, numparactual, numfldactual
    parameter(NUMPAR=34, NUMFLD=2 * NUMPAR)
    integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
    character WRDKEY * 40, OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
    integer :: nlevel
    common / HELPNOW / WRDKEY, NLEVEL
    integer, external :: infoinput
    external :: highlight_form_line
!
    integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
    integer :: nbut, imp, inp
    integer :: KR, KG, KB, KL

    KR = 0; KG = 0; KB = 0; KL = -1

    NLEVEL = 4
    OPTION(1) = 'NCOLDG=31          DESIGN GRID          '; it(2 * 1) = 2
    OPTION(2) = 'NCOLRG=212         PREVIOUS STATE GRID  '; it(2 * 2) = 2
    OPTION(3) = 'NCOLDN=3           DESIGN NET           '; it(2 * 3) = 2
    OPTION(4) = 'NCOLRN=211         PREVIOUS STATE NET   '; it(2 * 4) = 2
    OPTION(5) = 'NCOLNN=205         NETNODES             '; it(2 * 5) = 2
    OPTION(6) = 'NCOLSP=204         SPLINES              '; it(2 * 6) = 2
    OPTION(7) = 'NCOLLN=120         LAND BOUNDARY        '; it(2 * 7) = 2
    OPTION(8) = 'NCOLTX=210         TEXTLINES            '; it(2 * 8) = 2
    OPTION(9) = 'NCOLPL=221         POLYGON              '; it(2 * 9) = 2
    OPTION(10) = 'NCOLCRS=230        CROSS SECTIONS       '; it(2 * 10) = 2
    OPTION(11) = 'NCOLTHD=231        THIN DAMS            '; it(2 * 11) = 2
    OPTION(12) = 'NCOLFXW=232        FIXED WEIRS          '; it(2 * 12) = 2
    OPTION(13) = 'NCOLHL=31          HIGHLIGHT NODES/LINKS'; it(2 * 13) = 2
    OPTION(14) = 'KLVEC=4            VECTORS 110          '; it(2 * 14) = 2
    OPTION(15) = 'KLPROF=222         PROFILES             '; it(2 * 15) = 2
    OPTION(16) = 'KLSCL=221          ISOSCALE LEGEND      '; it(2 * 16) = 2
    OPTION(17) = 'KLTEX=3            NUMBERS              '; it(2 * 17) = 2
    OPTION(18) = 'KLOBS=221          OBSERVATION POINTS   '; it(2 * 18) = 2
    OPTION(19) = '                                        '; it(2 * 19) = 2
    OPTION(20) = 'Change RGB of colour Nr                 '; it(2 * 20) = 2
    OPTION(21) = 'R                                       '; it(2 * 21) = 2
    OPTION(22) = 'G                                       '; it(2 * 22) = 2
    OPTION(23) = 'B                                       '; it(2 * 23) = 2
    OPTION(24) = '                                        '; it(2 * 24) = 2
    OPTION(25) = 'R SCREEN                                '; it(2 * 25) = 2
    OPTION(26) = 'G SCREEN                                '; it(2 * 26) = 2
    OPTION(27) = 'B SCREEN                                '; it(2 * 27) = 2
    OPTION(28) = '                                        '; it(2 * 28) = 2
    OPTION(29) = 'R PLOT                                  '; it(2 * 29) = 2
    OPTION(30) = 'G PLOT                                  '; it(2 * 30) = 2
    OPTION(31) = 'B PLOT                                  '; it(2 * 31) = 2
    OPTION(32) = '                                        '; it(2 * 32) = 2
    OPTION(33) = 'JaFahrenheit                            '; it(2 * 33) = 2
    OPTION(34) = 'KLSRC=233         SORSIN                '; it(2 * 34) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

    HELPM(1) = '0<= ncol <=255                                              '
    HELPM(2) = '0<= ncol <=255                                              '
    HELPM(3) = '0<= ncol <=255                                              '
    HELPM(4) = '0<= ncol <=255                                              '
    HELPM(5) = '0<= ncol <=255                                              '
    HELPM(6) = '0<= ncol <=255                                              '
    HELPM(7) = '0<= ncol <=255                                              '
    HELPM(8) = '0<= ncol <=255                                              '
    HELPM(9) = '0<= ncol <=255                                              '
    HELPM(10) = '0<= ncol <=255                                              '
    HELPM(11) = '0<= ncol <=255                                              '
    HELPM(12) = '0<= ncol <=255                                              '
    HELPM(13) = '0<= ncol <=255                                              '
    HELPM(14) = '0<= ncol <=255                                              '
    HELPM(15) = '0<= ncol <=255                                              '
    HELPM(16) = '0<= ncol <=255                                              '
    HELPM(17) = '0<= ncol <=255                                              '
    HELPM(18) = '0<= ncol <=255                                              '
    HELPM(1:31) = '0<= ncol <=255                                              '
    HELPM(32:33) = '0/1                                                         '
    HELPM(34) = '0<= ncol <=255                                              '

    call SAVEKEYS()
    NUMPARACTUAL = NUMPAR
    NUMFLDACTUAL = 2 * NUMPARACTUAL

    IR = 0
    do I = 1, NUMPARACTUAL
       IL = IR + 1
       IR = IL + 1
       IX(IL) = 13
       IX(IR) = 95
       IY(IL) = I
       IY(IR) = I
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

    call IFormPutINTEGER(2 * 1, NCOLDG)
    call IFORMPUTINTEGER(2 * 2, NCOLRG)
    call IFORMPUTINTEGER(2 * 3, NCOLDN)
    call IFORMPUTINTEGER(2 * 4, NCOLRN)
    call IFORMPUTINTEGER(2 * 5, NCOLNN)
    call IFORMPUTINTEGER(2 * 6, NCOLSP)
    call IFORMPUTINTEGER(2 * 7, NCOLLN)
    call IFORMPUTINTEGER(2 * 8, NCOLTX)
    call IFORMPUTINTEGER(2 * 9, NCOLPL)
    call IFORMPUTINTEGER(2 * 10, NCOLCRS)
    call IFORMPUTINTEGER(2 * 11, NCOLTHD)
    call IFORMPUTINTEGER(2 * 12, NCOLFXW)
    call IFORMPUTINTEGER(2 * 13, NCOLHL)
    call IFORMputINTEGER(2 * 14, KLVEC)
    call IFORMputINTEGER(2 * 15, KLPROF)
    call IFORMPUTINTEGER(2 * 16, KLSCL)
    call IFORMputINTEGER(2 * 17, KLTEX)
    call IFORMputINTEGER(2 * 18, KLOBS)

    call IFORMputINTEGER(2 * 20, KL)
    call IFORMputINTEGER(2 * 21, KR)
    call IFORMputINTEGER(2 * 22, KG)
    call IFORMputINTEGER(2 * 23, KB)

    call IFORMPUTINTEGER(2 * 25, NREDS)
    call IFORMPUTINTEGER(2 * 26, NGREENS)
    call IFORMPUTINTEGER(2 * 27, NBLUES)

    call IFORMPUTINTEGER(2 * 29, NREDP)
    call IFORMPUTINTEGER(2 * 30, NGREENP)
    call IFORMPUTINTEGER(2 * 31, NBLUEP)

    call IFORMPUTINTEGER(2 * 33, Jafahrenheit)
    call IFORMPUTINTEGER(2 * 34, KLSRC)

    !  Display the form with numeric fields left justified
    !  and set the initial field to number 2
    call IOUTJUSTIFYNUM('L')
    IFEXIT = 2
    call IFormAttribute(IFEXIT - 1, 'BU', ' ', ' ')
    call IFORMSHOW()

30  continue
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

          call IFORMGETINTEGER(2 * 1, NCOLDG)
          call IFORMGETINTEGER(2 * 2, NCOLRG)
          call IFORMGETINTEGER(2 * 3, NCOLDN)
          call IFORMGETINTEGER(2 * 4, NCOLRN)
          call IFORMGETINTEGER(2 * 5, NCOLNN)
          call IFORMGETINTEGER(2 * 6, NCOLSP)
          call IFORMGETINTEGER(2 * 7, NCOLLN)
          call IFORMGETINTEGER(2 * 8, NCOLTX)
          call IFORMGETINTEGER(2 * 9, NCOLPL)
          call IFORMGETINTEGER(2 * 10, NCOLCRS)
          call IFORMGETINTEGER(2 * 11, NCOLTHD)
          call IFORMGETINTEGER(2 * 12, NCOLFXW)
          call IFORMGETINTEGER(2 * 13, NCOLHL)
          call IFORMGEtINTEGER(2 * 14, KLVEC)
          call IFORMGEtINTEGER(2 * 15, KLPROF)
          call IFORMGETINTEGER(2 * 16, KLSCL)
          call IFORMGEtINTEGER(2 * 17, KLTEX)
          call IFORMGEtINTEGER(2 * 18, KLOBS)

          call IFORMGETINTEGER(2 * 20, KL); KL = min(255, KL)
          call IFORMGETINTEGER(2 * 21, KR); KR = min(255, max(0, KR))
          call IFORMGETINTEGER(2 * 22, KG); KG = min(255, max(0, KG))
          call IFORMGETINTEGER(2 * 23, KB); KB = min(255, max(0, KB))

          call IFORMGETINTEGER(2 * 25, NREDS); NREDS = min(255, max(0, NREDS))
          call IFORMGETINTEGER(2 * 26, NGREENS); NGREENS = min(255, max(0, NGREENS))
          call IFORMGETINTEGER(2 * 27, NBLUES); NBLUES = min(255, max(0, NBLUES))

          call IFORMGETINTEGER(2 * 29, NREDP); NREDP = min(255, max(0, NREDP))
          call IFORMGETINTEGER(2 * 30, NGREENP); NGREENP = min(255, max(0, NGREENP))
          call IFORMGETINTEGER(2 * 31, NBLUEP); NBLUEP = min(255, max(0, NBLUEP))

          call IFORMGEtINTEGER(2 * 33, JaFahrenheit)
          call IFORMGEtINTEGER(2 * 34, KLSRC)

          if (KL > 0) then
             call IGRPALETTERGB(KL, KR, KG, KB)
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

 end subroutine CHANGEcolournumbers
