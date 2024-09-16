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

   subroutine CHANGENUMERICALPARAMETERS4()
      use M_FLOW
      use m_flowgeom
      use unstruc_display
      use dflowfm_version_module, only: company, product_name
      use unstruc_messages
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      implicit none

      integer :: numpar, numfld, numparactual, numfldactual
      parameter(NUMPAR=22, NUMFLD=2 * NUMPAR)
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD), L
      character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
      integer, external :: infoinput
      external :: highlight_form_line
!
      integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
      integer :: nbut, imp, inp
      double precision :: h1, h5, h7, w1, w5, w7

      NLEVEL = 4
!   OPTION( 1) = 'vicouv_filter                    (m2/s)' ; it(2* 1) = 6
      OPTION(1) = 'filter                           ( )   '; it(2 * 1) = 2
      OPTION(2) = 'filter order                     ( )   '; it(2 * 2) = 2
      OPTION(3) = 'hh1DUNI                          (m)   '; it(2 * 3) = 6
      OPTION(4) = 'Uniformtyp1D                     (m)   '; it(2 * 4) = 2
      OPTION(5) = 'wu1DUNI5                         (m)   '; it(2 * 5) = 6
      OPTION(6) = 'hh1DUNI5                         (m)   '; it(2 * 6) = 6
      OPTION(7) = 'Uniformtyp1D5                    (m)   '; it(2 * 7) = 2
      OPTION(8) = 'wu1DUNI7                         (m)   '; it(2 * 8) = 6
      OPTION(9) = 'hh1DUNI7                         (m)   '; it(2 * 9) = 6
      OPTION(10) = 'Uniformtyp1D7                    (m)   '; it(2 * 10) = 2
      OPTION(11) = 'japiaczek33                      ( )   '; it(2 * 11) = 2
      OPTION(12) = 'Expchistem                       ( )   '; it(2 * 12) = 6
      OPTION(13) = 'Uchistem                         ( )   '; it(2 * 13) = 6
      OPTION(14) = 'Expchileaf                       ( )   '; it(2 * 14) = 6
      OPTION(15) = 'Uchileaf                         ( )   '; it(2 * 15) = 6
      OPTION(16) = 'Cdleaf                           ( )   '; it(2 * 16) = 6
      OPTION(17) = 'Arealeaf                         ( )   '; it(2 * 17) = 6
      OPTION(18) = 'jaPure1D                         ( )   '; it(2 * 18) = 2
      OPTION(19) = 'Bedslopedir                      ( )   '; it(2 * 19) = 6
      OPTION(20) = 'Bedwidth                         (m)   '; it(2 * 20) = 6
      OPTION(21) = 'Bedwaveamplitude                 (m)   '; it(2 * 21) = 6
      OPTION(22) = 'Bedwavelength                    (m)   '; it(2 * 22) = 6

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

      HELPM(1) = 'Distance coeff (0.8)                                    ( ) '
      HELPM(2) = 'Uniform 1D width                                        ( ) '
      HELPM(3) = 'Uniform 1D height                                       ( ) '
      HELPM(4) = 'Uniform proftyp kn3=1,6 1=circle, 2=rect A/P, 3=rect K  ( ) '
      HELPM(5) = 'Uniform width  of 1D2D connection type 5 streetinlets   ( ) '
      HELPM(6) = 'Uniform height of 1D2D connection type 5 streetinlets   ( ) '
      HELPM(7) = 'Uniform proftyp kn3=5,  1=circle, 2=rect A/P, 3=rect K  ( ) '
      HELPM(8) = 'Uniform width  of 1D2D connection type 7 roofgutterpipes( ) '
      HELPM(9) = 'Uniform height of 1D2D connection type 7 roofgutterpipes( ) '
      HELPM(10) = 'Uniform proftyp kn3=7,  1=circle, 2=rect A/P, 3=rect K  ( ) '
      HELPM(11) = '0=no, 1 = yes                                           ( ) '
      HELPM(12) = '                                                        ( ) '
      HELPM(13) = '                                                        ( ) '
      HELPM(14) = '                                                        ( ) '
      HELPM(15) = '                                                        ( ) '
      HELPM(16) = '                                                        ( ) '
      HELPM(17) = '                                                        ( ) '
      HELPM(18) = '!< 0 = org 1D advec, 1 = pure1D using vol1_f, 2 = vol1  ( ) '
      HELPM(19) = '0 = to E, 90 = to N                                     ( ) '
      HELPM(20) = '                                                        ( ) '
      HELPM(21) = '                                                        ( ) '
      HELPM(22) = '                                                        ( ) '

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

!   CALL IFORMputdouble  (2* 1 ,vicouv_filter, '(F7.3)' )
      call IFORMputinteger(2 * 1, jafilter)
      write (6, *) filterorder
      call IFORMputinteger(2 * 2, filterorder)
      w1 = wu1DUNI; h1 = hh1DUNI
      w5 = wu1DUNI5; h5 = hh1DUNI5
      w7 = wu1DUNI7; h7 = hh1DUNI7
      call IFORMputdouble(2 * 3, hh1DUNI, '(F7.3)')
      call IFORMputinteger(2 * 4, iproftypuni)
      call IFORMputdouble(2 * 5, wu1DUNI5, '(F7.3)')
      call IFORMputdouble(2 * 6, hh1DUNI5, '(F7.3)')
      call IFORMputinteger(2 * 7, iproftypuni5)
      call IFORMputdouble(2 * 8, wu1DUNI7, '(F7.3)')
      call IFORMputdouble(2 * 9, hh1DUNI7, '(F7.3)')
      call IFORMputinteger(2 * 10, iproftypuni7)
      call IFORMputinteger(2 * 11, japiaczek33)
      call IFORMputdouble(2 * 12, Expchistem, '(F7.3)')
      call IFORMputdouble(2 * 13, Uchistem, '(F7.3)')
      call IFORMputdouble(2 * 14, Expchileaf, '(F7.3)')
      call IFORMputdouble(2 * 15, Uchileaf, '(F7.3)')
      call IFORMputdouble(2 * 16, Cdleaf, '(F7.3)')
      call IFORMputdouble(2 * 17, Arealeaf, '(F7.3)')
      call IFORMputinteger(2 * 18, jaPure1D)
      call IFORMputdouble(2 * 19, Bedslopedir, '(F7.3)')
      call IFORMputdouble(2 * 20, Bedwidth, '(F7.3)')
      call IFORMputdouble(2 * 21, Bedwaveamplitude, '(F7.3)')
      call IFORMputdouble(2 * 22, Bedwavelength, '(F7.3)')

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

!          CALL IFORMGETdouble  (2* 1 , vicouv_filter)
            call IFORMgetinteger(2 * 1, jafilter)
            call IFORMgetinteger(2 * 2, filterorder)
            call IFORMgetdouble(2 * 3, hh1DUNI)
            call IFORMgetinteger(2 * 4, iproftypuni)
            call IFORMgetdouble(2 * 5, wu1DUNI5)
            call IFORMgetdouble(2 * 6, hh1DUNI5)
            call IFORMgetinteger(2 * 7, iproftypuni5)
            call IFORMgetdouble(2 * 8, wu1DUNI7)
            call IFORMgetdouble(2 * 9, hh1DUNI7)
            call IFORMgetinteger(2 * 10, iproftypuni7)
            call IFORMgetinteger(2 * 11, japiaczek33)
            call IFORMgetdouble(2 * 12, Expchistem)
            call IFORMgetdouble(2 * 13, Uchistem)
            call IFORMgetdouble(2 * 14, Expchileaf)
            call IFORMgetdouble(2 * 15, Uchileaf)
            call IFORMgetdouble(2 * 16, Cdleaf)
            call IFORMgetdouble(2 * 17, Arealeaf)
            call IFORMgetinteger(2 * 18, jaPure1D)
            call IFORMgetdouble(2 * 19, Bedslopedir)
            call IFORMgetdouble(2 * 20, Bedwidth)
            call IFORMgetdouble(2 * 21, Bedwaveamplitude)
            call IFORMgetdouble(2 * 22, Bedwavelength)

            do L = 1, Lnx1D
               if (prof1D(1, L) >= 0) then ! only direct profiles
                  if (kcu(L) == 1) then
                     if (wu1DUNI /= w1) then
                        prof1D(1, L) = wu1DUNI
                     else if (hh1DUNI /= h1) then
                        prof1D(2, L) = hh1DUNI
                     end if
                  else if (kcu(L) == 5) then
                     if (wu1DUNI5 /= w5) then
                        prof1D(1, L) = wu1DUNI5
                     else if (hh1DUNI5 /= h5) then
                        prof1D(2, L) = hh1DUNI5
                     end if
                  else if (kcu(L) == 7) then
                     if (wu1DUNI7 /= w7) then
                        prof1D(1, L) = wu1DUNI7
                     else if (hh1DUNI7 /= h7) then
                        prof1D(2, L) = hh1DUNI7
                     end if
                  end if
                  wu(L) = prof1D(1, L)
                  if (abs(prof1D(3, L)) == 1) then ! circles are round
                     prof1D(2, L) = prof1D(1, L)
                  end if

               end if
            end do

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

   end subroutine CHANGENUMERICALPARAMETERS4
