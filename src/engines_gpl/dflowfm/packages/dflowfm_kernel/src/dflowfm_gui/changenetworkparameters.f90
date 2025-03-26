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

module m_changenetworkparameters
   use m_filemenu

   implicit none

contains

   subroutine changenetworkPARAMETERS()
      use m_sferic, only: jamidlat
      use network_data
      use unstruc_colors
      use unstruc_display_data
      use m_missing
      use dflowfm_version_module, only: company, product_name
      use unstruc_model, only: md_dryptsfile
      use m_helpnow
      use m_save_keys
      use m_restore_keys
      use m_help
      use m_highlight_form_line

      integer :: i
      integer :: ierror
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
      integer :: jins_old ! netcell administration out of date if jins changes
      integer :: iselect, minp
      character(len=128) select(3)

      integer, parameter :: NUMPAR = 23, NUMFLD = 2 * NUMPAR
      integer IX(NUMFLD), IY(NUMFLD), IS(NUMFLD), IT(NUMFLD)
      character OPTION(NUMPAR) * 40, HELPM(NUMPAR) * 60
      integer, external :: infoinput

      jins_old = jins

      NLEVEL = 4
      OPTION(1) = 'SELECT INSIDE POLYGON (1/0), 1 = INSIDE '; IT(1 * 2) = 2
      !OPTION(2) = 'TRIANGLEMINANGLE                       ' ; IT( 2*2)  = 6
      OPTION(2) = 'jadelnetlinktyp                         '; IT(2 * 2) = 2
      OPTION(3) = 'TRIANGLEMAXANGLE                        '; IT(3 * 2) = 6
      OPTION(4) = 'TRIANGLESIZEFACTOR, MAX.INSIDE/ AV.EDGE '; IT(4 * 2) = 6
      OPTION(5) = 'limit center; 1.0:in cell <-> 0.0:on c/g'; IT(5 * 2) = 6
      OPTION(6) = 'cosphiutrsh in geominit (good orhto)    '; IT(6 * 2) = 6
      OPTION(7) = 'remove small links       0.0->          '; IT(7 * 2) = 6
      OPTION(8) = 'TIME CONSUMING NETWORK CHECKS YES/NO 1/0'; IT(8 * 2) = 2
      OPTION(9) = 'NR OF SMOOTH. ITER. IN COURANT NETWORK  '; IT(9 * 2) = 2
      OPTION(10) = 'SMALLEST CELLSIZE IN COURANT NETWORK    '; IT(10 * 2) = 6
      OPTION(11) = 'REMOVE SMALL TRIANGLES, TRIAREAREMFRAC  '; IT(11 * 2) = 6
      OPTION(12) = 'REFINE NETWORK (QUADS) DIRECTION: 0,-1,1'; IT(12 * 2) = 2
      OPTION(13) = 'Merge nodes closer than tooclose (m)    '; IT(13 * 2) = 6
      OPTION(14) = 'Connect 1D end nodes to branch if closer'; IT(14 * 2) = 6
      OPTION(15) = 'Uniform DX in copy landb to 1D netw     '; IT(15 * 2) = 6
      OPTION(16) = 'snap-to-landbdy tolerance, netboundary  '; IT(16 * 2) = 6
      OPTION(17) = 'snap-to-landbdy tolerance, inner network'; IT(17 * 2) = 6
      OPTION(18) = 'max nr of faces allowed in removesmallfl'; IT(18 * 2) = 2
!   OPTION(19)= 'dry/illegal/cutcells file (*.pol, *.lst)' ; IT(19*2)  = 4
      if (len_trim(md_dryptsfile) == 0) then
         OPTION(19) = 'DRY CELL FILE (none)'
      else
         OPTION(19) = 'DRY CELL FILE ('//trim(md_dryptsfile(1:min(len_trim(md_dryptsfile), 25)))//')'
      end if
      IT(19 * 2) = 4
      OPTION(20) = '1D2D link generation algorithm          '; IT(20 * 2) = 2
      OPTION(21) = 'Lateral algorithm search radius         '; IT(21 * 2) = 6
      OPTION(22) = 'Use middle latitude (1/0)               '; IT(22 * 2) = 2
      OPTION(23) = 'Circumcenter (1/2/3)                    '; IT(23 * 2) = 2

!   123456789012345678901234567890123456789012345678901234567890
!            1         2         3         4         5         6
      HELPM(1) = &
         '1=inside, 0 = outside polygon (TO BE RESET AFTER USE)       '
      HELPM(2) = &
         '0 = delete nodes, > 0 = linktypkn3 to delete                '
      HELPM(3) = &
         '                                                            '
      HELPM(4) = &
         'MAX. INSIDE TRIANGLE SIZE / AVERAGE SIZE ON POLYGON         '
      HELPM(5) = &
         '                                                            '
      ! 'in geominit, 1.0=inside, on edge ,  0.9=inside close to edge'

      HELPM(6) = &
         'No flow model created if cosphiu > cosphiutrsh              '

      HELPM(7) = &
         '0.0 = remove no links, 0.1=remove links < 0.1 sqrt(baL+baR) '
      HELPM(8) = &
         '                                                            '
      HELPM(9) = &
         'NR OF SMOOTH. ITERATIONS IN COURANT NETWORK, SAMPLES RQUIRED'
      HELPM(10) = &
         'SMALLEST CELLSIZE IN COURANT NETWORK, SAMPLES REQUIRED      '
      HELPM(11) = &
         'SMALL TRIANGLE REMOVED IF TRIAREA < AV. ADJACENT AREAS      '
      HELPM(12) = &
         '0=BOTH DIRECTIONS, -1 = ONLY THIS, 1 = ONLY THAT            '
      HELPM(13) = &
         'Used in merge nodes on top of each other                    '
      HELPM(14) = &
         'than xx (m) to branch node, used in mergenodesontop         '
      HELPM(15) = &
         'used in copylandboundaryto1Dnetwork                         '
      HELPM(16) = &
         'tolerance in snap-to-landbdy, netboundary only (meshwidths) '
      HELPM(17) = &
         'tolerance in snap-to-landbdy, inner network    (meshwidths) '
      HELPM(18) = &
         'max nr of faces allowed in removesmallflowlinks             '
      HELPM(19) = &
         'choose                                                      '
      write (HELPM(20), '(I0,A,I0,A,I0,A)') &
         I1D2DTP_1TO1, ': default (1-to-1), ', I1D2DTP_1TON_EMB, ': embedded 1-to-n, ', I1D2DTP_1TON_LAT, ': lateral 1-to-n.'
      HELPM(22) = &
         '1 = yes, 0 = no                                             '
      HELPM(23) = &
         'iterate per 1=edge, 2=loop, 3=loop incl. outline            '

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
      call IFORMHELP(13, IH, 60)

      IR = 0
      do I = 1, NUMPARACTUAL
         IL = IR + 1
         IR = IL + 1
         call IFORMPUTSTRING(IL, OPTION(I))
         call IFORMPUTHELP(IR, HELPM(I))
         call IFORMATTRIBUTEN(IR, 0, 0, 7)
      end do

      call IFORMpuTINTEGER(2 * 1, jins)
      !CALL IFormputDouble (2*2 , TRIANGLEMINANGLE  ,   '(F7.3)')
      call IFormputinteger(2 * 2, jadelnetlinktyp)
      call IFormputDouble(2 * 3, TRIANGLEMAXANGLE, '(F7.3)')
      call IFormputDouble(2 * 4, TRIANGLESIZEFAC, '(F7.3)')
      call IFormputDouble(2 * 5, dcenterinside, '(F7.3)')
      call IFormputDouble(2 * 6, cosphiutrsh, '(F7.3)')
      call IFormputDouble(2 * 7, removesmalllinkstrsh, '(F7.3)')
      call IFORMpuTINTEGER(2 * 8, JOCHECKNET)
      call IFORMpuTINTEGER(2 * 9, numitcourant)
      call IFormputDouble(2 * 10, SMALLESTSIZEINCOURANT, '(F7.0)')
      call IFormputDouble(2 * 11, TRIAREAREMFRAC, '(F7.3)')
      call IFORMpuTINTEGER(2 * 12, M13QUAD)
      call IFormputDouble(2 * 13, Tooclose, '(F7.3)')
      call IFormputDouble(2 * 14, connect1dend, '(F7.3)')
      call IFormputDouble(2 * 15, Unidx1D, '(F7.3)')
      call IFormputDouble(2 * 16, DCLOSE_bound, '(F7.3)')
      call IFormputDouble(2 * 17, DCLOSE_whole, '(F7.3)')
      call IFormputinteger(2 * 18, maxfaceallow)

      call IFORMPUTSTRING(2 * 19, md_dryptsfile)
      iselect = 1
      select(1) = 'use'
      select(2) = 'new'
      select(3) = 'none'
      call IFORMPUTMENU(2 * 19, select, 3, iselect)

      call IFormputinteger(2 * 20, imake1d2dtype)
      call IFormputDouble(2 * 21, searchRadius1D2DLateral, '(F7.3)')
      call IFormputinteger(2 * 22, jamidlat)
      call IFormputinteger(2 * 23, circumcenter_method)

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
                  KEY = 22
               else
                  KEY = 23
               end if
            else
               KEY = 24
            end if
         end if
      else if (KEY == -1) then
         KEY = INFOINPUT(57)
      end if
      if (KEY == 26) then
         WRDKEY = OPTION(IFEXIT / 2)
         call HELP(WRDKEY, NLEVEL)
      else if (KEY == 23 .or. KEY == 24) then
         if (KEY == 23) then
            ! netcell administration out of date if jins changes
            call IFORMGETINTEGER(2 * 1, jins)
            if (jins /= jins_old) netstat = NETSTAT_CELLS_DIRTY
            jins_old = jins

            !CALL IFormGetDouble  (2*2 , TRIANGLEMINANGLE)
            call IFormGetinteger(2 * 2, jadelnetlinktyp)
            call IFormGetDouble(2 * 3, TRIANGLEMAXANGLE)
            call IFormGetDouble(2 * 4, TRIANGLESIZEFAC)
            call IFormGetDouble(2 * 5, dcenterinside)
            call IFormgetDouble(2 * 6, cosphiutrsh)
            call IFormGetDouble(2 * 7, removesmalllinkstrsh)
            call IFORMGETINTEGER(2 * 8, JOCHECKNET)
            call IFORMGETINTEGER(2 * 9, numitcourant)
            call IFormGetDouble(2 * 10, SMALLESTSIZEINCOURANT)
            call IFormGetDouble(2 * 11, TRIAREAREMFRAC)
            call IFORMGETINTEGER(2 * 12, M13QUAD)
            call IFormGetDouble(2 * 13, Tooclose)
            call IFormGetDouble(2 * 14, connect1dend)
            call IFormGetDouble(2 * 15, Unidx1D)
            call IFormGetDouble(2 * 16, DCLOSE_BOUND)
            call IFormGetDouble(2 * 17, DCLOSE_WHOLE)
            call IFormGetinteger(2 * 18, maxfaceallow)

            call IFORMGETSTRING(2 * 19, md_dryptsfile)
            call IFORMGETMENU(2 * 19, iselect)
            if (iselect == 2) then
               minp = 2 ! select file only
               call filemenu(minp, md_dryptsfile, ierror)
            else if (iselect == 3) then
               md_dryptsfile = ''
            end if
            iselect = 1

            call IFormGetinteger(2 * 20, imake1d2dtype)
            call IFormGetDouble(2 * 21, searchRadius1D2DLateral)
            call IFormGetinteger(2 * 22, jamidlat)
            call IFormGetinteger(2 * 23, circumcenter_method)

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

   end subroutine changenetworkPARAMETERS

end module m_changenetworkparameters
