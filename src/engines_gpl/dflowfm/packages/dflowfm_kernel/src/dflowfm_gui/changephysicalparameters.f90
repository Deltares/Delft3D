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

module m_changephysicalparameters

   implicit none

contains

   subroutine CHANGEPHYSICALPARAMETERS()
      use precision, only: dp
      use m_flow
      use m_flowgeom, only: ndx
      use unstruc_colors
      use unstruc_display_data
      use dflowfm_version_module, only: company, product_name
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      use m_help
      use m_highlight_form_line

      integer :: numpar, numfld, numparactual, numfldactual
      parameter(NUMPAR=15, NUMFLD=2 * NUMPAR)
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
      integer, external :: infoinput
!
      integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key, ierr
      integer :: nbut, imp, inp
      real(kind=dp) :: frcuniorg

      NLEVEL = 4
      OPTION(1) = 'frcuni                                  '; it(2 * 1) = 6
      OPTION(2) = 'ifrctypuni Friction formulation         '; it(2 * 2) = 2
      OPTION(3) = 'Windspeed     (m/s)                     '; it(2 * 3) = 6
      OPTION(4) = 'Winddirection ( ) 90= to East 0=to North'; it(2 * 4) = 6
      OPTION(5) = 'vicouv                           (m2/s) '; it(2 * 5) = 6
      OPTION(6) = 'Vicoww                           (m2/s) '; it(2 * 6) = 6
      OPTION(7) = 'Dicouv                           ( )    '; it(2 * 7) = 6
      OPTION(8) = 'Dicoww                           ( )    '; it(2 * 8) = 6
      OPTION(9) = 'Verticall Wall Nikuradse         (m)    '; it(2 * 9) = 6
      OPTION(10) = 'Smagorinsky                      ( )    '; it(2 * 10) = 6
      OPTION(11) = 'Elder                            ( )    '; it(2 * 11) = 6
      OPTION(12) = 'uniform friction coefficient 1D         '; it(2 * 12) = 6
      OPTION(13) = 'uniform friction coefficient 1D2D intern'; it(2 * 13) = 6
      OPTION(14) = 'uniform friction coefficient 1D groundly'; it(2 * 14) = 6
      OPTION(15) = 'uniform rainfall              (mm/hr)   '; it(2 * 15) = 6

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

      HELPM(1) = 'uniform friction coefficient                                '
      HELPM(2) = ' 0=Chz, 1=Mann, 2=White-Col, 3=White-Col-Waqua, 10=Glass    '
      HELPM(3) = '                                                            '
      HELPM(4) = '                                                            '
      HELPM(5) = 'background horizontal viscosity                             '
      HELPM(6) = 'background vertical   viscosity (0: no vert. visc. at all)  '
      HELPM(7) = 'background horizontal diffusivity                           '
      HELPM(8) = 'background vertical   diffusivity (0: no vert. diff. at all)'
      HELPM(9) = 'VERTICAL WALL NIKURADSE ROUGHNESS, (wall_z0 = KS/30)     (M)'
      HELPM(10) = 'vicuv = vicuv + ( (Smagorinsky*dx)**2)*Strainrate_S, eg 0.1 '
      HELPM(11) = 'vicuv = vicuv +    Elder*0.009*H*U                   eg 1.0 '
      HELPM(12) = 'uniform friction coefficient 1D                             '
      HELPM(13) = 'uniform friction coefficient 1D2D internal Link             '
      HELPM(14) = 'uniform friction coefficient 1D groundlayer                 '
      HELPM(15) = '(if non-zero overrides ext forcings)                        '

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

      frcuniorg = frcuni
      call IFormPutDouble(2 * 1, frcuni, '(F8.3)')
      call IFORMPUTinteger(2 * 2, ifrctypuni)
      call IFormPutDouble(2 * 3, windsp, '(F8.3)')
      call IFormPutDouble(2 * 4, winddir, '(F8.3)')
      call IFormPutDouble(2 * 5, vicouv, '(e9.2)')
      call IFormPutDouble(2 * 6, vicoww, '(e8.3)')
      call IFORMPUTdouble(2 * 7, dicouv, '(e8.3)')
      call IFORMPUTdouble(2 * 8, dicoww, '(e8.3)')
      call IFormPutDouble(2 * 9, wall_ks, '(F8.3)')
      call IFormPutDouble(2 * 10, Smagorinsky, '(F8.3)')
      call IFormPutDouble(2 * 11, Elder, '(F8.3)')
      call IFormPutDouble(2 * 12, frcuni1D, '(F8.3)')
      call IFormPutDouble(2 * 13, frcuni1D2D, '(F8.3)')
      call IFormPutDouble(2 * 14, frcuni1Dgrounlay, '(F8.3)')
      call IFormPutDouble(2 * 15, rainuni, '(F8.3)')

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

            call IFormGetDouble(2 * 1, frcuni)
            call IFORMGETinteger(2 * 2, ifrctypuni)
            call IFormGetDouble(2 * 3, windsp)
            call IFormGetDouble(2 * 4, winddir)
            call IFormGetDouble(2 * 5, vicouv)
            call IFormGetDouble(2 * 6, vicoww)
            call IFORMGetdouble(2 * 7, dicouv)
            call IFORMGetdouble(2 * 8, dicoww)
            call IFormGetDouble(2 * 9, wall_ks)
            call IFormGetDouble(2 * 10, Smagorinsky)
            call IFormGetDouble(2 * 11, Elder)
            call IFormGetDouble(2 * 12, frcuni1D)
            call IFormGetDouble(2 * 13, frcuni1D2D)
            call IFormGetDouble(2 * 14, frcuni1Dgrounlay)
            call IFormGetDouble(2 * 15, rainuni)

            if (allocated(frcu) .and. frcuniorg /= frcuni) then
               frcu = frcuni
            end if

            if (rainuni > 0d0) then
               if (.not. allocated(rain)) then
                  allocate (rain(ndx), stat=ierr); rain = 0d0
                  call aerr('rain(ndx)', ierr, ndx)
               end if
               jarain = 1; jaqin = 1
            end if

            wall_z0 = wall_ks / 30d0
            if (windsp /= 0d0) then
               call setuniformwind()
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

   end subroutine CHANGEPHYSICALPARAMETERS

end module m_changephysicalparameters
