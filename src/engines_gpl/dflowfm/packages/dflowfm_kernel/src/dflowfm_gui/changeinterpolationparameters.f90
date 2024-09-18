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

   subroutine CHANGEINTERPOLATIONPARAMETERS()
      use m_ec_interpolationsettings
      use M_SAMPLES, only: mxsam
      use m_arcinfo, only: mca
      use unstruc_display
      use dflowfm_version_module, only: company, product_name
      use m_helpnow
      use m_save_keys
      use m_restore_keys

      implicit none
      integer :: numpar, numfld, numparactual, numfldactual
      parameter(NUMPAR=8, NUMFLD=2 * NUMPAR)
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 45, HELPM(NUMPAR) * 60
      integer, external :: infoinput
      external :: highlight_form_line
!
      integer :: ir, il, iw, ixp, iyp, ih, i, ifexit, ifinit, key
      integer :: nbut, imp, inp

      NLEVEL = 4
      OPTION(1) = 'INTERPOLATIONTYPE (1=TRI,2=AVE,3=CURV. TRI)'; it(2 * 1) = 2
      OPTION(2) = 'JTEKINTERPOLATIONPROCESS (0/1)             '; it(2 * 2) = 2
      OPTION(3) = 'IAV, AVERAGINGTYPE                         '; it(2 * 3) = 2
      OPTION(4) = 'NUMMIN, MINUMUM NR OF POINTS IN AV         '; it(2 * 4) = 2
      OPTION(5) = 'RCEL, RELATIVE SEARCH CELL SIZE            '; it(2 * 5) = 6
      OPTION(6) = 'Interpolate_to , 1=bathy, 2=ZK, 3=S1, 4=ZC '; it(2 * 6) = 2
      OPTION(7) = 'Percentileminmax, average min or max Perc %'; it(2 * 7) = 6
      OPTION(8) = 'Mxsam                                      '; it(2 * 8) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6

      HELPM(1) = '1 = TRIANGULATION 2= CELL AVERAGING, 3=USE CURV.GRID FOR TRI'
      HELPM(2) = 'SHOW INTERPOLATION PROCESS 0-No, 1=Yes                      '
      HELPM(3) = '1=AVER., 2=CLOSEST POINT, 3=MAX, 4=MIN, 5=INV. DIST. WEIGHT '
      HELPM(4) = 'MINIMUM NR OF POINTS NEEDED INSIDE CELL TO HANDLE CELL      '
      HELPM(5) = 'DEFAULT 1.0 = ACTUAL CELL SIZE, 2.0 = TWICE AS LARGE        '
      HELPM(6) = '1=? , 2=network, 3=waterlevels, 4=curvigrid                 '
      HELPM(7) = 'if Perc>0, for iav == 3 or iav==4, av of hih or low percenti'
      HELPM(8) = 'if >0, use bilin instead of triinterp                      '

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

      call IFormPutINTEGER(2 * 1, INTERPOLATIONTYPE)
      call IFORMPUTinteger(2 * 2, JTEKINTERPOLATIONPROCESS)
      call IFormPutINTEGER(2 * 3, IAV)
      call IFormPutINTEGER(2 * 4, NUMMIN)
      call IFormPutDouble(2 * 5, RCEL, '(F8.3)')
      call IFormPutINTEGER(2 * 6, Interpolate_to)
      call IFormPutDouble(2 * 7, percentileminmax, '(F8.3)')
      call IFormPutINTEGER(2 * 8, Mxsam)

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
            call IFormGEtINTEGER(2 * 1, INTERPOLATIONTYPE)
            call IFORMGETinteger(2 * 2, JTEKINTERPOLATIONPROCESS)
            call IFormGEtINTEGER(2 * 3, IAV)
            call IFormGEtINTEGER(2 * 4, NUMMIN)
            call IFormGEtDouble(2 * 5, RCEL)
            call IFormGEtINTEGER(2 * 6, Interpolate_to)
            call IFormGEtDouble(2 * 7, Percentileminmax)
            call IFormGEtinteger(2 * 8, Mxsam)
            if (mxsam == 0) mca = 0

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

   end subroutine CHANGEINTERPOLATIONPARAMETERS
