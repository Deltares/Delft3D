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

module m_changeorthoparameters

implicit none

contains

   subroutine changeorthoPARAMETERS()
      use m_orthosettings
      use unstruc_colors
      use unstruc_display_data
      use m_netw
      use dflowfm_version_module, only: company, product_name
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      use m_help
      use m_highlight_form_line

      implicit none
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

      integer, parameter :: NUMPAR = 15, NUMFLD = 2 * NUMPAR
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 82, HELPM(NUMPAR) * 102
      integer, external :: infoinput

      NLEVEL = 4
      OPTION(1) = 'ITERATIONS ORTHOGONALISE, ATTRACT. PARAM'; IT(1 * 2) = 2
      OPTION(2) = 'ITERATIONS ORTHOGONALISE, BOUNDARY      '; IT(2 * 2) = 2
      OPTION(3) = 'ITERATIONS ORTHOGONALISE, INNER AREA    '; IT(3 * 2) = 2
      OPTION(4) = 'ORTHOGONALISE <-> SMOOTH;      1.0<->0.0'; IT(4 * 2) = 6
      OPTION(5) = 'minimum ortho<->smooth on bdy; 1.0<->0.0'; IT(5 * 2) = 6
      OPTION(6) = 'circumormasscenter;            1.0<->0.0'; IT(6 * 2) = 6
      OPTION(7) = 'smoother <-> area homogenizer; 1.0<->0.0'; IT(7 * 2) = 6
      OPTION(8) = 'project to (land)boundary               '; IT(8 * 2) = 2
      OPTION(9) = 'cornernode cosine threshold             '; IT(9 * 2) = 6
      OPTION(10) = 'mesh-adaptation method                  '; IT(10 * 2) = 2
      OPTION(11) = 'mesh-refinement factor;        0.0<->1.0'; IT(11 * 2) = 6
      OPTION(12) = 'smooth. iters. ''solution''    in adapt.'; IT(12 * 2) = 2
      OPTION(13) = 'smooth. iters. monitor mat.    in adapt.'; IT(13 * 2) = 2
      OPTION(14) = 'curvi-like <-> pure ortho;     0.0<->1.0'; IT(14 * 2) = 6
      OPTION(15) = 'keep circumcenters (1) or not (0)       '; IT(15 * 2) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
      HELPM(1) = &
         'Nr. of outer iterations in orthogonalise                    '
      HELPM(2) = &
         'Nr. of boundary iterations in orthogonalise = ITATP*ITBN    '
      HELPM(3) = &
         'Nr. of inner iterations in orthogonalise = ITATP*ITBND*ITIN '
      HELPM(4) = &
         'Balance between orthogonalisation and Laplacian smoothing   '
      HELPM(5) = &
         'Minimum balance between orthogonalisation and Laplacian smoothing on the boundary'
      HELPM(6) = &
         'CIRCUMCENTER = 1, MASSCENTER = 0                            '
      HELPM(7) = &
         'Balance between smoothing and cell-area homogenization      '
      HELPM(8) = &
         '0:no, 1:to org netb, 2:netb to Ldb, 3:''2''+inner net to Ldb, 4:whole net, 5:ok'
      HELPM(9) = &
         'corner if cosine of boundary edge angle < -threshold        '
      HELPM(10) = &
         '0: Winslow; 1: arc-length; 2: harmonic map                  '
      HELPM(11) = &
         'Concentration of mesh in refined region                     '
      HELPM(12) = &
         'Number of smoothing iterations of ''solution'' u in adapt.  '
      HELPM(13) = &
         'Number of smoothing iterations of monitor matrix G in adapt.'
      HELPM(14) = &
         'Pure orthogonalisation versus curvi-grid-like orth. in quads'
      HELPM(15) = &
         'keep circumcenters (1) or not (0)                           '

      call SAVEKEYS()
      NUMPARACTUAL = NUMPAR
      NUMFLDACTUAL = 2 * NUMPARACTUAL

      IR = 0
      do I = 1, NUMPARACTUAL
         IL = IR + 1; IR = IL + 1
         IS(IL) = 82; IS(IR) = 10
         IX(IL) = 10; IX(IR) = 92
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
!   CALL IFORMHELP(13,IH,60)
      call IFORMHELP(13, IH, 102)

      IR = 0
      do I = 1, NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         call IFORMPUTSTRING(IL, OPTION(I))
         call IFORMPUTHELP(IR, HELPM(I))
         call IFORMATTRIBUTEN(IR, 0, 0, 7)
      end do

      call IFORMpuTINTEGER(2 * 1, ITATP)
      call IFORMpuTINTEGER(2 * 2, ITBND)
      call IFORMpuTINTEGER(2 * 3, ITIN)
      call IFormputDouble(2 * 4, ATPF, '(F7.3)')
      call IFormputDouble(2 * 5, ATPF_B, '(F7.3)')
      call IFormputDouble(2 * 6, CIRCUMORMASSCENTER, '(F7.3)')
      call IFORMputDouble(2 * 7, SMOOTHORAREA, '(F7.3)')
      call IFORMpuTINTEGER(2 * 8, JAPROJECT)
      call IFORMpuTDouble(2 * 9, CORNERCOS, '(F7.3)')
      call IFORMpuTINTEGER(2 * 10, ADAPT_METHOD)
      call IFORMputDouble(2 * 11, ADAPT_BETA, '(F7.3)')
      call IFORMpuTINTEGER(2 * 12, ADAPT_NITER_U)
      call IFORMpuTINTEGER(2 * 13, ADAPT_NITER_G)
      call IFORMputDouble(2 * 14, ORTHO_PURE, '(F7.3)')
      call IFormputINTEGER(2 * 15, keepcircumcenters)

      ! Display the form with numeric fields left justified and set the initial field to number 2
      call IOUTJUSTIFYNUM('L')
      IFEXIT = 2
      call IFormAttribute(IFEXIT - 1, 'BU', ' ', ' ')
      call IFORMSHOW()

30    continue
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
            call IFORMGETINTEGER(2 * 1, ITATP)
            call IFORMGETINTEGER(2 * 2, ITBND)
            call IFORMGETINTEGER(2 * 3, ITIN)
            call IFormGetDouble(2 * 4, ATPF)
            call IFormGetDouble(2 * 5, ATPF_B)
            call IFormGETDouble(2 * 6, CIRCUMORMASSCENTER)
            call IFORMGETDouble(2 * 7, SMOOTHORAREA)
            call IFORMGETINTEGER(2 * 8, JAPROJECT)
            call IFORMGETDOUBLE(2 * 9, CORNERCOS)
            call IFORMGETINTEGER(2 * 10, ADAPT_METHOD)
            call IFORMGETDouble(2 * 11, ADAPT_BETA)
            call IFORMGETINTEGER(2 * 12, ADAPT_NITER_U)
            call IFORMGETINTEGER(2 * 13, ADAPT_NITER_G)
            call IFORMGETDouble(2 * 14, ORTHO_PURE)
            call IFormGetInteger(2 * 15, keepcircumcenters)
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

   end subroutine changeorthoPARAMETERS

end module m_changeorthoparameters
