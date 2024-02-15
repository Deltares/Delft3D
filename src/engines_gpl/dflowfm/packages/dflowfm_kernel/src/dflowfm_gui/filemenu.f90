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

      SUBROUTINE FILEMENU(mrgf,filnam,ierror)
      use unstruc_display
      use dflowfm_version_module, only : company, product_name
      use unstruc_files, only : filnammenu
      implicit none
      integer,           intent(inout) :: mrgf    !<  call with mrgf = 0 means LOAD, mrgf = 1  means SAVE, mrgf = 2 means get filename only
      character(len=*),  intent(inout) :: filnam
      integer,           intent(  out) :: ierror  !<  return value -1 = ESC
      integer :: ih, ihl, imenuscroll, imp, infoinput, inp, iw, ixp, iyp, jatab, jazekr, keepstartdir, key, l, len
      integer :: maxfil, maxhlp, nahead, nbut, nlevel, numdir, numf, numfil, numtop, numtxi, numtxt

!     Gives menu with files filnam


      PARAMETER (MAXHLP = 2000, MAXFIL = 2000)
      INTEGER IFDATE(MAXFIL), IFSIZE(MAXFIL)
      CHARACTER  HLPTXT(MAXHLP)*80,FILIST(MAXFIL)*86,WRDKEY*40
      CHARACTER  DIR*86, CURDIR*86, DIR2*86,FILNAM2*86
      LOGICAL JA
      COMMON /HELPC/     HLPTXT,NUMTXT
      COMMON /STARTDIR/  KEEPSTARTDIR

      integer jaopen ! open file (1) or not (0)

      ierror     = 0 ! Default: no error
      filnammenu = ' '
      jaopen     = 1
      if ( mrgf.eq.2 ) then
         mrgf = 0
         jaopen = 0
      end if


!     Initialise
      CALL IWinWordWrap('OFF')
      CALL ITEXTCOLOURN(HLPFOR, HLPBCK)
      CALL INHIGHLIGHT('WHITE','RED')

      L      = INDEX(FILNAM,'.')
      IW     = NPOS(3)
      IXP    = NPOS(1) + (IWS-IW)/2
      IYP    = NPOS(2)
      IH     = IHS - 9

      IHL    = IH - 1
      NUMTXI = NUMTXT - IHL
      NAHEAD = 1
      NUMTOP = 1
      NUMF   = 1
      JAZEKR = 0
      JATAB  = 0
!
      CALL IOSDIRNAME(CURDIR)
      DIR = CURDIR
!
!     Header of filewindow
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP,IW,1)
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutCentre(1,trim(company)//'-'//trim(product_name)// ' FILEMENU')
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!
!     Explain keyfunctions in bottom window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IHS-1,IW,2)
      CALL IWinOutStringXY(1,1,'Up or down arrow; confirm = Enter/left,right mouse;')
      CALL IWinOutStringXY(1,2,'help = F1; toggle between fields = Tab; quit = Esc')
!
!     Filewindow is middelste window
      CALL IWinAction('FPC')
      CALL IWinOpen(IXP,IYP+3,IW,IH)
!
      CALL ITEXTCOLOURN(LBLFOR,LBLBCK)
      CALL IWinOutStringXY(2,7,'NAME / DIRECTORY                                               SIZE        DATE   TIME')
!

      IF (MRGF .EQ. 0) THEN
         CALL IOutStringXY(IXP+1,IYP+3,'LOAD FILENAME')
      ELSE
         CALL IOutStringXY(IXP+1,IYP+3,'SAVE FILENAME')
      ENDIF
      L = len_trim(FILNAM)
      CALL IOutStringXY(IXP+15,IYP+3,'('//FILNAM(1:L)//')')

      CALL IOutStringXY(IXP+1,IYP+6,'DIRECTORY')

      CALL ITEXTCOLOUR('BWHITE','BLU')
      CALL IOutStringXY(IXP+1,IYP+4, FILNAM)
      CALL IOutStringXY(IXP+1,IYP+7, DIR)
      CALL ITEXTCOLOURN(HLPFOR,HLPBCK)

!     CALL IOutStringXY(IXP+47,IYP+6,'choose file in LEFT WINDOW')
!     CALL IOutStringXY(IXP+47,IYP+7,'or use TAB to toggle to')
!     CALL ITEXTCOLOUR('WHITE','BBLU')
!     CALL IOutStringXY(IXP+54,IYP+7,'TAB')
!     CALL ITEXTCOLOURN(HLPFOR,HLPBCK)
!     CALL IOutStringXY(IXP+47,IYP+8,'NAME or DIRECTORY')

   20 CONTINUE
      CALL UPDATEFILES(FILNAM,FILIST,NUMFIL,NUMDIR,IFDATE,IFSIZE,IXP,IYP,IH)

      CALL TIMLIN()
      IF (JATAB .EQ. 0) THEN
         CALL ITEXTCOLOUR('BWHITE','BLU')
         CALL INHIGHLIGHT('BLACK','WHITE')
         NUMF=IMenuScroll(FILIST,NUMFIL,IXP,IYP+10,' ',IH-7,0,NUMF)
      ELSE IF (JATAB .EQ. 1) THEN
         CALL INHIGHLIGHT('BLACK','WHITE')
         FILNAM2 = FILNAM
         CALL InStringXYDEF(IXP+1,IYP+4,' ',0,FILNAM2,LEN)
         CALL ITEXTCOLOUR('BWHITE','BLU')
         CALL IOutStringXY(IXP+1,IYP+4,FILNAM2)
         IF (INDEX(FILNAM2,'*') .NE. 0) THEN
            IF (FILNAM2 .NE. FILNAM) THEN
               FILNAM = FILNAM2
               JATAB  = 0
               GOTO 20
            ENDIF
         ELSE
            FILNAM = FILNAM2
         ENDIF
      ELSE IF (JATAB .EQ. 2) THEN
         DIR2 = DIR
         CALL INHIGHLIGHT('BLACK','WHITE')
         CALL InStringXYDEF(IXP+1,IYP+7,' ',0,DIR2,LEN)
         CALL ITEXTCOLOUR('BWHITE','BLU')
         CALL IOutStringXY(IXP+1,IYP+7,DIR2)
         IF (DIR2 .NE. DIR) THEN
            CALL IOSDIRCHANGE(DIR2)
            DIR = ' '
            CALL IOSDIRNAME(DIR)
!           IF (INFOERROR(3) .NE. 0) THEN
            IF (DIR .NE. DIR2) THEN
               CALL QNERROR('DIRECTORY',DIR2,'DOES NOT EXIST')
            ELSE
!              DIR   = DIR2
               CALL ITEXTCOLOUR('BWHITE','BLU')
               CALL IOutStringXY(IXP+1,IYP+7, DIR)
               JATAB = 0
            ENDIF
            GOTO 20
         ENDIF
      ENDIF

      CALL TIMLIN()
!
      KEY = InfoInput(55)

      IF (KEY .EQ. -2) THEN
          NBUT = INFOINPUT(61)
          IF (NBUT .GE. 1) THEN
             IMP = INFOINPUT(62) + 1
             INP = INFOINPUT(63) + 1
             IF (IMP .GE. IXP .AND. IMP .LT. IXP+IW .AND.    &
                 INP .GE. IYP+3   .AND. INP .LT. IYP+IH+3+2  ) THEN
                 IF (INP .LE. 7) THEN
                    JATAB = 1
                 ELSE IF (INP .LE. 10) THEN
                    JATAB = 2
                 ELSE IF (INP .GE. 12) THEN
                    JATAB = 0
                 ENDIF
             ELSE
                KEY = 23   ! Buiten scherm = Esc
             ENDIF
          ENDIF
      ELSE IF (KEY .EQ. -1) THEN
         KEY = INFOINPUT(57)
      ENDIF

      IF (KEY .EQ. 24) THEN        ! F1 = HELP
         NLEVEL = 1
         WRDKEY = 'FILE-MENU INSTRUCTIONS'
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 23) THEN   ! Esc
         ierror = -1
         GOTO 9999
      ELSE IF (KEY .EQ. 27) THEN   ! Tab
         JATAB = JATAB + 1
         IF (JATAB .EQ. 3) JATAB = 0
      ELSE IF (KEY .EQ. 21 .OR. KEY .EQ. 22) THEN   !Linker of rechter muis

         IF (JATAB .EQ. 0) THEN
            IF (NUMF .LE. NUMDIR) THEN
               CALL IOSDIRCHANGE( FILIST(NUMF)(1:54) )
               DIR = ' '
               CALL IOSDIRNAME(DIR)
               CALL ITEXTCOLOUR('BWHITE','BLU')
               CALL IOutStringXY(IXP+1,IYP+7, DIR)
               GOTO 20
            ELSE
               WRITE(FILNAM,'(A)') FILIST(NUMF)(1:54)
            ENDIF
         ENDIF

         L = len_trim(FILNAM)
         IF (L .EQ. 0) GOTO 20
         INQUIRE (FILE = FILNAM(1:L), EXIST = JA)

         IF (MRGF .EQ. 0) THEN
            IF (.NOT. JA) THEN
               CALL DENY(IXP,IYP)
            ELSE
               JAZEKR = 1
            ENDIF
         ELSE IF (MRGF .EQ. 1) THEN
            IF (JA) THEN
               CALL CONFRM(' FILE ALREADY EXISTS. OVERWRITE ANYWAY ? ', JAZEKR)
            ELSE
               JAZEKR = 1
            ENDIF
         ENDIF
!
         IF (JAZEKR .EQ. 1) THEN
            IF (INDEX(FILNAM,'*') .NE. 0) GOTO 20
            IF (DIR .NE. CURDIR) CALL IOSDIRCHANGE(DIR)
            if ( jaopen.eq.1 ) then
               CALL NEWFIL(MRGF,FILNAM)
            else
               if ( ierror /= 0 ) then
                  FILNAM = ''
               end if
            end if
            GOTO 9999
         ENDIF
      ENDIF

      GOTO 20
!
 9999 CONTINUE
      IF (KEEPSTARTDIR .EQ. 1) THEN
         IF (DIR .NE. CURDIR) CALL IOSDIRCHANGE(CURDIR)
      ENDIF
      CALL IWinClose(1)
      CALL IWinClose(1)
      CALL IWinClose(1)
      filnammenu = filnam
      RETURN
      END
