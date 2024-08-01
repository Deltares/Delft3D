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

      subroutine CHANGEGRIDPARAMETERS()
         use M_GRID
         use M_GRIDSETTINGS
         use m_sferic
         use unstruc_display
         use m_polygon
         use dflowfm_version_module, only: company, product_name
         implicit none

         integer :: numpar, numfld, numparactual, numfldactual
         parameter(NUMPAR=15, NUMFLD=2 * NUMPAR)
         integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
         integer :: nlevel
         character WRDKEY * 40, OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
         common / HELPNOW / WRDKEY, NLEVEL
         integer, external :: infoinput
         external :: highlight_form_line
!
         integer :: ip, ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
         integer :: nbut, imp, inp, k
         double precision :: phi

         NLEVEL = 3
         OPTION(1) = 'M-REFINEMENT FACTOR                     '
         OPTION(2) = 'N-REFINEMENT FACTOR                     '
         OPTION(3) = 'NR SMOOTHING ITERATIONS                 '
         OPTION(4) = 'SMOOTHING PARAMETER                     '
         OPTION(5) = 'ATTRACTION/REPULSION PARAMETER          '
         OPTION(6) = 'PASSIVE GRID OR GRID FIXED IN PASTE     '
         OPTION(7) = 'GO BACK TO STARTUP DIRECTORY YES/NO     '
         OPTION(8) = 'LINE OR SPLINE REPRESENTATION  (0.0-1.0)'
         OPTION(9) = 'EQUIDISTANT OR SMOOTH INTERPOL (0.0-1.0)'
         OPTION(10) = 'INCREASE FACTOR IN LINE MIRROR  (0.1-10)'
         OPTION(11) = 'Spherical or Cartesian coordinates (1 0)'
         OPTION(12) = 'DRAW STEREOGRAPHIC OR NO PROJECTION(1 0)'
!     pillar grid
         option(13) = 'PILLAR RADIUS (m)                       '
         option(14) = 'PILLAR X-COORDINATE                     '
         option(15) = 'PILLAR Y-COORDINATE                     '
!
!      123456789012345678901234567890123456789012345678901234567890
!               1         2         3         4         5         6
         HELPM(1) = 'INTEGER VALUE <                                             '
         HELPM(2) = 'INTEGER VALUE <                                             '
         HELPM(3) = 'SMOOTHING, EDIT  : (0.0 - 100)  DEFAULT = 20,  INTERMEDIATE '
         HELPM(4) = 'SMOOTHING  EDIT  : (0.0 - 1.0)  DEFAULT = 0.2, INTERMEDIATE '
         HELPM(5) = 'ATTRACT./REPULS. : (0.0 - 0.5)  DEFAULT = 0.1, INTERMEDIATE '
         HELPM(6) = 'GRID PASTE       : (0.0 - 1.0)  0.0: GRID FIXED, 1.0:PASSIVE'
         HELPM(7) = 'ALWAYS BACK TO STARTUP DIRECTORY (1) OR KEEP NEW DIR. (0)   '
         HELPM(8) = 'STRAIGHT LINES REPRESENTATION = 0, CURVED LINES = 1         '
         HELPM(9) = 'SPLINES TO GRID  : (0.0 - 1.0) DEFAULT = 1.0, SMOOTH INTERP.'
         HELPM(10) = 'GRID SIZE INCREASE IN LINE MIRROR, 1.0 = EQUAL SIZE         '
         HELPM(11) = '1 = Spherical, 0 = Cartesian                                '
         HELPM(12) = '1 = STEREOGRAPHIC PROJECTION , 0 = NO PROJECTION            '
         HELPM(13) = 'SET RADIUS TO 0 FOR NO PILLAR                               '
         HELPM(14) = '                                                            '
         HELPM(14) = '                                                            '

         call SAVEKEYS()
         IP = 20
         write (HELPM(1) (IP:IP + 4), '(I5)') min(MMAX - 1, 1 + (MMAX - 1) / max(1, (MC - 1)))
         write (HELPM(2) (IP:IP + 4), '(I5)') min(NMAX - 1, 1 + (NMAX - 1) / max(1, (NC - 1)))

         if (JDEMO == 1) then
            NUMPARACTUAL = 6
         else
            NUMPARACTUAL = NUMPAR
         end if
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
            if (I <= 3 .or. I == 7 .or. I == 10 .or. I == 11 .or. I == 12) then
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
         call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' PARAMETER FORM')
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
         call IFORMDEFINE('W', NUMFLDACTUAL, IX, IY, IS, IT)

!     Define a help field and define help strings
!     for 2 of the 4 input fields
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
         call IFORMPUTINTEGER(2 * 3, ITSMO)
         call IFormPutDouble(2 * 4, CSMO, '(F5.3)')
         call IFormPutDouble(2 * 5, RFAC, '(F5.3)')
         call IFormPutDouble(2 * 6, BAAS2, '(F5.3)')
         call IFORMPUTINTEGER(2 * 7, KEEPSTARTDIR)
         call IFormPutDouble(2 * 8, SPLFAC, '(F5.3)')
         call IFormPutDouble(2 * 9, SPLFAC2, '(F5.3)')
         call IFormPutDouble(2 * 10, FACMIR, '(F5.3)')
         call IFORMPUTINTEGER(2 * 11, jsferic)
         call IFORMPUTINTEGER(2 * 12, jsferTEK)
         call IFormPutDouble(2 * 13, pil_rad, '(F7.3)')
         call IFormPutDouble(2 * 14, pil_x, '(F7.3)')
         call IFormPutDouble(2 * 15, pil_y, '(F7.3)')

!   Diplay the form with numeric fields left justified
!   an set the initial field to number 2
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
               call IFORMGETINTEGER(2 * 1, MFAC)
               call IFORMGETINTEGER(2 * 2, NFAC)
               call IFORMGETINTEGER(2 * 3, ITSMO)
               call IFormGetDouble(2 * 4, CSMO)
               call IFormGetDouble(2 * 5, RFAC)
               call IFormGetDouble(2 * 6, BAAS2)
               call IFORMGETINTEGER(2 * 7, KEEPSTARTDIR)
               call IFormGetDouble(2 * 8, SPLFAC)
               call IFormGetDouble(2 * 9, SPLFAC2)
               call IFormGetDouble(2 * 10, FACMIR)
               call IFORMGETINTEGER(2 * 11, jsferic)
               call IFORMGETINTEGER(2 * 12, jsferTEK)
               call IFormGetDouble(2 * 13, pil_rad)
               call IFormGetDouble(2 * 14, pil_x)
               call IFormGetDouble(2 * 15, pil_y)

               KEEPSTARTDIR = max(0, KEEPSTARTDIR)
               KEEPSTARTDIR = min(1, KEEPSTARTDIR)
               !MFAC = MAX(1,MFAC)
               !NFAC = MAX(1,NFAC)
               CSMO = max(0d0, CSMO)
               RFAC = max(0d0, RFAC)
               BAAS2 = max(0d0, min(BAAS2, 1d0))
               SPLFAC = max(0d0, min(SPLFAC, 1d0))
               SPLFAC2 = max(0d0, min(SPLFAC2, 1d0))
               FACMIR = max(0.1d0, min(FACMIR, 10d0))
               jsferic = max(0, min(jsferic, 1))

               if (pil_rad < 0d0) then ! cre
                  if (maxpol < mfac + 1) then
                     call increasepol(mfac + 1, 0)
                  end if
                  pil_rad = abs(pil_rad)
                  do k = 1, mfac + 1
                     phi = twopi * (dble(k - 1) / dble(mfac))
                     xpl(k) = pil_x + pil_rad * cos(phi)
                     ypl(k) = pil_y + pil_rad * sin(phi)
                  end do
                  npl = mfac + 1
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

      end subroutine CHANGEGRIDPARAMETERS
