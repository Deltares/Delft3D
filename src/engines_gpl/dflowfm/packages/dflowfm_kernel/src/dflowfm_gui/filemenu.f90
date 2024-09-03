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

      subroutine FILEMENU(mrgf, filnam, ierror)
         use unstruc_display
         use dflowfm_version_module, only: company, product_name
         use unstruc_files, only: filnammenu
         use m_helpc
         use m_startdir
         use m_qnerror
         implicit none
         integer, intent(inout) :: mrgf !<  call with mrgf = 0 means LOAD, mrgf = 1  means SAVE, mrgf = 2 means get filename only
         character(len=*), intent(inout) :: filnam
         integer, intent(out) :: ierror !<  return value -1 = ESC
         integer :: ih, ihl, imenuscroll, imp, infoinput, inp, iw, ixp, iyp, jatab, jazekr, key, l, len
         integer :: maxfil, nahead, nbut, nlevel, numdir, numf, numfil, numtop, numtxi

!     Gives menu with files filnam

         parameter(MAXFIL=2000)
         integer IFDATE(MAXFIL), IFSIZE(MAXFIL)
         character FILIST(MAXFIL) * 86, WRDKEY * 40
         character DIR * 86, CURDIR * 86, DIR2 * 86, FILNAM2 * 86
         logical JA
         integer jaopen ! open file (1) or not (0)

         ierror = 0 ! Default: no error
         filnammenu = ' '
         jaopen = 1
         if (mrgf == 2) then
            mrgf = 0
            jaopen = 0
         end if

!     Initialise
         call IWinWordWrap('OFF')
         call ITEXTCOLOURN(HLPFOR, HLPBCK)
         call INHIGHLIGHT('WHITE', 'RED')

         L = index(FILNAM, '.')
         IW = NPOS(3)
         IXP = NPOS(1) + (IWS - IW) / 2
         IYP = NPOS(2)
         IH = IHS - 9

         IHL = IH - 1
         NUMTXI = NUMTXT - IHL
         NAHEAD = 1
         NUMTOP = 1
         NUMF = 1
         JAZEKR = 0
         JATAB = 0
!
         call IOSDIRNAME(CURDIR)
         DIR = CURDIR
!
!     Header of filewindow
         call IWinAction('FPC')
         call IWinOpen(IXP, IYP, IW, 1)
         call ITEXTCOLOURN(LBLFOR, LBLBCK)
         call IWinOutCentre(1, trim(company)//'-'//trim(product_name)//' FILEMENU')
         call ITEXTCOLOURN(HLPFOR, HLPBCK)
!
!     Explain keyfunctions in bottom window
         call IWinAction('FPC')
         call IWinOpen(IXP, IHS - 1, IW, 2)
         call IWinOutStringXY(1, 1, 'Up or down arrow; confirm = Enter/left,right mouse;')
         call IWinOutStringXY(1, 2, 'help = F1; toggle between fields = Tab; quit = Esc')
!
!     Filewindow is middelste window
         call IWinAction('FPC')
         call IWinOpen(IXP, IYP + 3, IW, IH)
!
         call ITEXTCOLOURN(LBLFOR, LBLBCK)
         call IWinOutStringXY(2, 7, 'NAME / DIRECTORY                                               SIZE        DATE   TIME')
!

         if (MRGF == 0) then
            call IOutStringXY(IXP + 1, IYP + 3, 'LOAD FILENAME')
         else
            call IOutStringXY(IXP + 1, IYP + 3, 'SAVE FILENAME')
         end if
         L = len_trim(FILNAM)
         call IOutStringXY(IXP + 15, IYP + 3, '('//FILNAM(1:L)//')')

         call IOutStringXY(IXP + 1, IYP + 6, 'DIRECTORY')

         call ITEXTCOLOUR('BWHITE', 'BLU')
         call IOutStringXY(IXP + 1, IYP + 4, FILNAM)
         call IOutStringXY(IXP + 1, IYP + 7, DIR)
         call ITEXTCOLOURN(HLPFOR, HLPBCK)

!     CALL IOutStringXY(IXP+47,IYP+6,'choose file in LEFT WINDOW')
!     CALL IOutStringXY(IXP+47,IYP+7,'or use TAB to toggle to')
!     CALL ITEXTCOLOUR('WHITE','BBLU')
!     CALL IOutStringXY(IXP+54,IYP+7,'TAB')
!     CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!     CALL IOutStringXY(IXP+47,IYP+8,'NAME or DIRECTORY')

20       continue
         call UPDATEFILES(FILNAM, FILIST, NUMFIL, NUMDIR, IFDATE, IFSIZE, IXP, IYP, IH)

         call TIMLIN()
         if (JATAB == 0) then
            call ITEXTCOLOUR('BWHITE', 'BLU')
            call INHIGHLIGHT('BLACK', 'WHITE')
            NUMF = IMenuScroll(FILIST, NUMFIL, IXP, IYP + 10, ' ', IH - 7, 0, NUMF)
         else if (JATAB == 1) then
            call INHIGHLIGHT('BLACK', 'WHITE')
            FILNAM2 = FILNAM
            call InStringXYDEF(IXP + 1, IYP + 4, ' ', 0, FILNAM2, LEN)
            call ITEXTCOLOUR('BWHITE', 'BLU')
            call IOutStringXY(IXP + 1, IYP + 4, FILNAM2)
            if (index(FILNAM2, '*') /= 0) then
               if (FILNAM2 /= FILNAM) then
                  FILNAM = FILNAM2
                  JATAB = 0
                  goto 20
               end if
            else
               FILNAM = FILNAM2
            end if
         else if (JATAB == 2) then
            DIR2 = DIR
            call INHIGHLIGHT('BLACK', 'WHITE')
            call InStringXYDEF(IXP + 1, IYP + 7, ' ', 0, DIR2, LEN)
            call ITEXTCOLOUR('BWHITE', 'BLU')
            call IOutStringXY(IXP + 1, IYP + 7, DIR2)
            if (DIR2 /= DIR) then
               call IOSDIRCHANGE(DIR2)
               DIR = ' '
               call IOSDIRNAME(DIR)
!           IF (INFOERROR(3) .NE. 0) THEN
               if (DIR /= DIR2) then
                  call QNERROR('DIRECTORY', DIR2, 'DOES NOT EXIST')
               else
!              DIR   = DIR2
                  call ITEXTCOLOUR('BWHITE', 'BLU')
                  call IOutStringXY(IXP + 1, IYP + 7, DIR)
                  JATAB = 0
               end if
               goto 20
            end if
         end if

         call TIMLIN()
!
         KEY = InfoInput(55)

         if (KEY == -2) then
            NBUT = INFOINPUT(61)
            if (NBUT >= 1) then
               IMP = INFOINPUT(62) + 1
               INP = INFOINPUT(63) + 1
               if (IMP >= IXP .and. IMP < IXP + IW .and. &
                   INP >= IYP + 3 .and. INP < IYP + IH + 3 + 2) then
                  if (INP <= 7) then
                     JATAB = 1
                  else if (INP <= 10) then
                     JATAB = 2
                  else if (INP >= 12) then
                     JATAB = 0
                  end if
               else
                  KEY = 23 ! Buiten scherm = Esc
               end if
            end if
         else if (KEY == -1) then
            KEY = INFOINPUT(57)
         end if

         if (KEY == 24) then ! F1 = HELP
            NLEVEL = 1
            WRDKEY = 'FILE-MENU INSTRUCTIONS'
            call HELP(WRDKEY, NLEVEL)
         else if (KEY == 23) then ! Esc
            ierror = -1
            goto 9999
         else if (KEY == 27) then ! Tab
            JATAB = JATAB + 1
            if (JATAB == 3) JATAB = 0
         else if (KEY == 21 .or. KEY == 22) then !Linker of rechter muis

            if (JATAB == 0) then
               if (NUMF <= NUMDIR) then
                  call IOSDIRCHANGE(FILIST(NUMF) (1:54))
                  DIR = ' '
                  call IOSDIRNAME(DIR)
                  call ITEXTCOLOUR('BWHITE', 'BLU')
                  call IOutStringXY(IXP + 1, IYP + 7, DIR)
                  goto 20
               else
                  write (FILNAM, '(A)') FILIST(NUMF) (1:54)
               end if
            end if

            L = len_trim(FILNAM)
            if (L == 0) goto 20
            inquire (FILE=FILNAM(1:L), EXIST=JA)

            if (MRGF == 0) then
               if (.not. JA) then
                  call DENY(IXP, IYP)
               else
                  JAZEKR = 1
               end if
            else if (MRGF == 1) then
               if (JA) then
                  call CONFRM(' FILE ALREADY EXISTS. OVERWRITE ANYWAY ? ', JAZEKR)
               else
                  JAZEKR = 1
               end if
            end if
!
            if (JAZEKR == 1) then
               if (index(FILNAM, '*') /= 0) goto 20
               if (DIR /= CURDIR) call IOSDIRCHANGE(DIR)
               if (jaopen == 1) then
                  call NEWFIL(MRGF, FILNAM)
               else
                  if (ierror /= 0) then
                     FILNAM = ''
                  end if
               end if
               goto 9999
            end if
         end if

         goto 20
!
9999     continue
         if (KEEPSTARTDIR == 1) then
            if (DIR /= CURDIR) call IOSDIRCHANGE(CURDIR)
         end if
         call IWinClose(1)
         call IWinClose(1)
         call IWinClose(1)
         filnammenu = filnam
         return
      end
