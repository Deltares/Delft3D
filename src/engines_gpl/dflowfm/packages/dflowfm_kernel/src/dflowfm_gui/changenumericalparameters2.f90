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

module m_changenumericalparameters2

   implicit none

contains

   subroutine CHANGENUMERICALPARAMETERS2()
      use m_flow
      use unstruc_colors
      use unstruc_display_data
      use dflowfm_version_module, only: company, product_name
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      use m_help
      use m_highlight_form_line

      integer :: numpar, numfld, numparactual, numfldactual
      parameter(NUMPAR=27, NUMFLD=2 * NUMPAR)
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
      integer, external :: infoinput
!
      integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
      integer :: nbut, imp, inp

      NLEVEL = 4; i = 1
      OPTION(i) = 'ITURBULENCEMODEL                     ( )'; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'JAUSTARINT                           ( )'; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'jabaroctimeint                          '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'JAVAKEPS                                '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'IDENSFORM                               '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'JARHOXU                                 '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'JAVASAL                                 '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'IFIXEDWEIRSCHEME                        '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'Tsigma                                  '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Cffacver                                '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Cffachormom                             '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Cfexphormom                             '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Cfconhormom                             '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Javatem                                 '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'Javiuplus3D                             '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'Jaqaisq1                                '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'Addksources                             '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Initialise rho, if not, first barocl = 0'; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'jaLogprofatubndin                       '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'javau                                   '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'jacomp                                  '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'Drop2D                                  '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'Drop3D                                  '; it(2 * i) = 6; i = i + 1
      OPTION(i) = 'jaStructurelayersactive                 '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'jarhointerfaces                         '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'maxitpresdens                           '; it(2 * i) = 2; i = i + 1
      OPTION(i) = 'jabaroczlaybed, (fix for zed problem)   '; it(2 * i) = 2; i = i + 1

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

      i = 1
      HELPM(i) = '0=no, 1 = constant, 2 = algebraic, 3 = k-eps, 4 = k-tau     '; i = i + 1
      HELPM(i) = '0123                                                        '; i = i + 1
      HELPM(i) = '1 = expl, -2; abashford, -3 = ab3, -5 = adv rho             '; i = i + 1
      HELPM(i) = '0 = NO, 3 = VERT IMPL, HOR EXPL                             '; i = i + 1
      HELPM(i) = '0 = no, 1 = eckart                                          '; i = i + 1
      HELPM(i) = '0 = no, 1 = YES                                             '; i = i + 1
      HELPM(i) = '0=No, 1=Upwe, 2=Cente, 3=Upwi, 4=Centi, 5=4,3, 6=MCexpl     '; i = i + 1
      HELPM(i) = '0=No, 6=subgrid, 7=rajaratnam, 8=Tabelb, 9=Willemontenotyet '; i = i + 1
      HELPM(i) = 'Sigma adaptation timescale, only for layertype == 4         '; i = i + 1
      HELPM(i) = '1 = yes, 0 = no                                             '; i = i + 1
      HELPM(i) = '0=never switch off ho term vertical                         '; i = i + 1
      HELPM(i) = '0=never switch off ho term hor mom                          '; i = i + 1
      HELPM(i) = '0=exp of                                                    '; i = i + 1
      HELPM(i) = '0=constant to be added to                                   '; i = i + 1
      HELPM(i) = '0=No, 1=Upwe, 2=Cente, 3=Upwi, 4=Centi, 5=4,3, 6=MCexpl     '; i = i + 1
      HELPM(i) = '0=no, 1 = yes                                               '; i = i + 1
      HELPM(i) = '0=no, 1 = yes                                               '; i = i + 1
      HELPM(i) = '0=no, 1 = yes                                               '; i = i + 1
      HELPM(i) = 'at ubnd in: 0 = uniform U1, 1 = log U1, 2 = also k-eps      '; i = i + 1
      HELPM(i) = '0=no, 3 = impli upw, 5 = Quickest                           '; i = i + 1
      HELPM(i) = '0=standard, 1 = use csu snu in weights, 2 = scalarx,y banf  '; i = i + 1
      HELPM(i) = '0=no, 1 = yes                                               '; i = i + 1
      HELPM(i) = '0=no, 1 = yes                                               '; i = i + 1
      HELPM(i) = '0=no, 1 = yes                                               '; i = i + 1
      HELPM(i) = '0=centers, 1 = interfaces                                   '; i = i + 1
      HELPM(i) = 'max nr of rho/pressure iterations, only for idensform > 10  '; i = i + 1
      HELPM(i) = 'default now 1, was 0, keyword will disappear in future      '; i = i + 1

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
      call IFormPutINTEGER(2 * i, ITURBULENCEMODEL); i = i + 1
      call IFORMPUTINTEGER(2 * i, JAUSTARINT); i = i + 1
      call IFORMPUTINTEGER(2 * i, jabaroctimeint); i = i + 1
      call IFORMPUTINTEGER(2 * i, JAVAKEPS); i = i + 1
      call IFORMPUTINTEGER(2 * i, IDENSFORM); i = i + 1
      call IFORMPUTINTEGER(2 * i, JARHOXU); i = i + 1
      call IFORMPUTINTEGER(2 * i, JAVASAL); i = i + 1
      call IFORMPUTINTEGER(2 * i, ifixedweirscheme); i = i + 1
      call IFORMPUTdouble(2 * i, Tsigma, '(F7.3)'); i = i + 1
      call IFORMPUTdouble(2 * i, Cffacver, '(F7.3)'); i = i + 1
      call IFORMPUTdouble(2 * i, Cffachormom, '(F7.3)'); i = i + 1
      call IFORMPUTdouble(2 * i, Cfexphormom, '(F7.3)'); i = i + 1
      call IFORMPUTdouble(2 * i, Cfconhormom, '(F7.3)'); i = i + 1
      call IFORMPUTINTEGER(2 * i, JAVATEM); i = i + 1
      call IFORMputINTEGER(2 * i, javiuplus3D); i = i + 1
      call IFORMputINTEGER(2 * i, jaqaisq1); i = i + 1
      call IFORMputdouble(2 * i, addksources, '(F7.3)'); i = i + 1
      call IFORMputINTEGER(2 * i, jainirho); i = i + 1
      call IFORMputINTEGER(2 * i, jaLogprofatubndin); i = i + 1
      call IFORMputINTEGER(2 * i, javau); i = i + 1
      call IFORMputINTEGER(2 * i, jacomp); i = i + 1
      call IFORMPUTdouble(2 * i, drop2D, '(F7.3)'); i = i + 1
      call IFORMPUTdouble(2 * i, drop3D, '(F7.3)'); i = i + 1
      call IFORMputINTEGER(2 * i, jastructurelayersactive); i = i + 1
      call IFORMputINTEGER(2 * i, jarhointerfaces); i = i + 1
      call IFORMputINTEGER(2 * i, maxitpresdens); i = i + 1
      call IFORMputINTEGER(2 * i, jabaroczlaybed); i = i + 1

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

            i = 1
            call IFORMGETINTEGER(2 * i, ITURBULENCEMODEL); i = i + 1
            call IFORMGETINTEGER(2 * i, JAUSTARINT); i = i + 1
            call IFORMGETINTEGER(2 * i, jabaroctimeint); i = i + 1
            call IFORMGETINTEGER(2 * i, JAVAKEPS); i = i + 1
            call IFORMGETINTEGER(2 * i, IDENSFORM); i = i + 1
            call IFORMGETINTEGER(2 * i, JARHOXU); i = i + 1
            call IFORMGETINTEGER(2 * i, JAVASAL); i = i + 1
            call IFORMGETINTEGER(2 * i, IFIXEDWEIRSCHEME); i = i + 1
            call IFORMGETdouble(2 * i, Tsigma); i = i + 1
            call IFORMGETdouble(2 * i, Cffacver); i = i + 1
            call IFORMGETdouble(2 * i, Cffachormom); i = i + 1
            call IFORMGETdouble(2 * i, Cfexphormom); i = i + 1
            call IFORMGETdouble(2 * i, Cfconhormom); i = i + 1
            call IFORMGETINTEGER(2 * i, JAVATEM); i = i + 1
            call IFORMGETINTEGER(2 * i, javiuplus3D); i = i + 1
            call IFORMGETINTEGER(2 * i, jaqaisq1); i = i + 1
            call IFORMGETdouble(2 * i, addksources); i = i + 1
            call IFORMGETINTEGER(2 * i, jainirho); i = i + 1
            call IFORMGETINTEGER(2 * i, jaLogprofatubndin); i = i + 1
            call IFORMGETINTEGER(2 * i, javau); i = i + 1
            call IFORMGETINTEGER(2 * i, jacomp); i = i + 1
            call IFORMGETdouble(2 * i, Drop2D); i = i + 1
            call IFORMGETdouble(2 * i, Drop3D); i = i + 1
            call IFORMGETINTEGER(2 * i, jastructurelayersactive); i = i + 1
            call IFORMGETINTEGER(2 * i, jarhointerfaces); i = i + 1
            call IFORMGetINTEGER(2 * i, maxitpresdens); i = i + 1
            call IFORMGetINTEGER(2 * i, jabaroczlaybed); i = i + 1

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

   end subroutine CHANGENUMERICALPARAMETERS2

end module m_changenumericalparameters2
