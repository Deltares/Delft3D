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

      SUBROUTINE BOTLIN(JA,NUMB,KEY)
      use m_devices
      use unstruc_display
      implicit none
      integer :: imenuhoriz
      integer :: infoinput
      integer :: iw
      integer :: ja
      integer :: key
      integer :: li
      integer :: maxop
      integer :: maxopt
      integer :: nlevel
      integer :: nput
      integer :: numb
      integer :: nwhat
      PARAMETER (MAXOP = 64)
      CHARACTER*14 OPTION(MAXOP), TEX*14
      CHARACTER WRDKEY*40
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer, save :: lastmenuheight = 1

   10 CONTINUE
      LI = IHS
      IW = IWS
      if (lastmenuheight == 2) then
        CALL ITEXTCOLOUR('BLACK','WHITE')
        call IClearLine(IHS-1) ! Clear second-last line to erase old menus.
      end if
      lastmenuheight = 1 ! Default (only some are two lines)
      IF (NUMB .EQ. 0) THEN
         OPTION(1) =  'CONTINUE     ;'
         OPTION(2) =  'F1 = help    ;'
         OPTION(3) =  'F2 = history ;'
         OPTION(4) =  'P/PRINTSCREEN;'
         OPTION(5) =  'STOP         ;'
         MAXOPT = 5
      ELSE IF (NUMB .EQ. 1) THEN
         OPTION(1) =  ' = choose  ;'
         OPTION(2) =  'F1 = help    ;'
         OPTION(3) =  'F2 = history ;'
         OPTION(4) =  'Esc= exit    ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 2) THEN
         if (jafullbottomline== 1) then
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERT ;'
         OPTION(3) =  'R   = REPLACE;'
         OPTION(4) =  'D   = DELETE ;'
         OPTION(5) =  'X   = SPLIT  ;'
         OPTION(6) =  'e   = ERASEPL;'
         OPTION(7) =  'E   = invERAS;'
         OPTION(8) =  'F   = REFINE ;'
         OPTION(9) =  'M   = MERGE  ;'
         OPTION(10)=  'L   = TO LAND;'
         OPTION(11)=  'N   = TO NET ;'
         OPTION(12)=  'w/W = dropwat;'
         OPTION(13)=  'b/B = droplnd;'
         OPTION(14)=  'TAB = DCURSOR;'
         OPTION(15)=  'ESC = UNDO   ;'
         OPTION(16)=  'Z   = ZOOMIN ;'
         maxopt    = 16
         lastmenuheight = 2
         else
         OPTION(1) =  'I=INS '
         OPTION(2) =  'R=REPL '
         OPTION(3) =  'D=DEL '
         OPTION(4) =  'X=SPLIT '
         OPTION(5) =  'e=ERAS '
         OPTION(6) =  'E=inve '
         OPTION(7) =  'F=REF '
         OPTION(8) =  'M=MERG '
         OPTION(9) =  'L=TOLA '
         OPTION(10)=  'N=TONE '
         OPTION(11)=  'w/W=wat '
         OPTION(12)=  'b/B=lnd '
         maxopt    = min(12,iws/12)
         endif
      ELSE IF (NUMB .EQ. 3) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'TAB = DCURSOR;'
         OPTION(3) =  'ESC = UNDO   ;'
         OPTION(4) =  'Z   = ZOOMIN ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 4) THEN
         OPTION(1) =  '+   = DEEPER ;'
         OPTION(2) =  '-   = SHALLOW;'
         OPTION(3) =  'ESC = UNDO   ;'
         OPTION(4) =  'Z   = ZOOMIN ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 5) THEN
         OPTION(1) =  'LMS = WINDOW ;'
         OPTION(2) =  'RMS = DEFAULT;'
         OPTION(3) =  'Z = ZOOM OUT ;'
         OPTION(4) =  '+   = LARGER ;'
         OPTION(5) =  '-   = SMALLER;'
         OPTION(6) =  'ESC = UNDO   ;'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 6) THEN
!        editgridlineBLOK
         OPTION(1) =  'F1 = help    ;'
         OPTION(2) =  'F2 = history ;'
         OPTION(3) =  'P/PRINTSCREEN;'
         OPTION(4) =  'ESC = UNDO   ;'
         OPTION(5) =  'CLICK GRIDLINE'
         OPTION(6) =  'AND INFLUENCE '
         OPTION(7) =  'READY=RIGHT MS'
         MAXOPT    =  7
      ELSE IF (NUMB .EQ. 7) THEN
!        editgridshift
         OPTION(1) =  'F1 = help    ;'
         OPTION(2) =  'F2 = history ;'
         OPTION(3) =  'P/PRINTSCREEN;'
         OPTION(4) =  'ESC = UNDO   ;'
         OPTION(5) =  'SHIFT THE     '
         OPTION(6) =  'INDICATED LINE'
         OPTION(7) =  'READY=RIGHT MS'
         MAXOPT    =  7
      ELSE IF (NUMB .EQ. 8) THEN
!        editgridBLOK
         OPTION(1) =  'F1 = help    ;'
         OPTION(2) =  'F2 = history ;'
         OPTION(3) =  'P/PRINTSCREEN;'
         OPTION(4) =  'ESC = UNDO   ;'
         OPTION(5) =  'CLICK A BLOCK;'
         OPTION(6) =  'READY=RIGHT MS'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 9) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERT ;'
         OPTION(3) =  'R   = REPLACE;'
         OPTION(4) =  'D   = DELETE ;'
         OPTION(5) =  'ESC = UNDO   ;'
         OPTION(6) =  'Z   = ZOOMIN ;'
         OPTION(7) =  'NEW SPLINE RM;'
         OPTION(8) =  'C   = COPY   ;'
         OPTION(9) =  'M   = MOVE   ;'
         OPTION(10) = 'X   = DEL SPL;'
         OPTION(11) = 'L   = TO LAND;'
         MAXOPT    =  11
      ELSE IF (NUMB .EQ. 10) THEN
         OPTION(1) =  'A = ANCHOR; '
         OPTION(2) =  'I = INSERT; '
         OPTION(3) =  'R = REPLACE;'
         OPTION(4) =  'D = DELETE; '
         OPTION(5) =  'M = MERGE;  '  ! FFFFF
         OPTION(6) =  'G = NET2CURV' ! FFFFF
         OPTION(7) =  'C = CUT;    '
         OPTION(8) =  'X = DELCON; '
         OPTION(9) =  'S = SPLIT; '
         OPTION(10) = 'V = FIELDMOVE' ! fieldmove
         OPTION(11) = 'B = FLDROTATE' ! fieldrotate
         OPTION(12) = '1...4 = KN3;' ! kn(3,:) change 1/2/3/4
         OPTION(13) = 'L = TO LAND;' ! snap to land boundary
         OPTION(14) = 'k = KILL CELL;' ! delete cell and update administration
         OPTION(15) = 'K = DEREFINE; ' ! derefine by 'Casulli-type' killcell
         OPTION(16) = 'E = add layer; '   ! add layer of cells
         lastmenuheight = 2
         MAXOPT    = 16
      ELSE IF (NUMB .EQ. 11) THEN
         OPTION(1) =  '+   = INCREAS;'
         OPTION(2) =  '-   = DECREAS;'
         OPTION(3) =  'ESC = UNDO   ;'
         OPTION(4) =  'SPACE BAR =  ;'
         MAXOPT    =  4
      ELSE IF (NUMB .EQ. 12) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERT ;'
         OPTION(3) =  'R   = REPLACE;'
         OPTION(4) =  'D   = DELETE ;'
         OPTION(5) =  'C   = CHANGEV;'
         OPTION(6) =  'm   = SET MIN;'
         OPTION(7) =  'M   = SET MAX;'
         OPTION(8) =  'H = hide/show;'
         OPTION(9) =  'ESC = UNDO   ;'
         OPTION(10)=  'Z   = ZOOMIN ;'
         OPTION(11)=  'Q   = sampath;'
         OPTION(12)=  'F   = fldfill;'
         MAXOPT    =  12
      ELSE IF (NUMB .EQ. 13) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  'I   = INSERTD;'
         OPTION(3) =  'R   = REPLACD;'
         OPTION(4) =  'D   = DELETED;'
         OPTION(5) =  'ESC = UNDO   ;'
         OPTION(6) =  'Z   = ZOOMIN ;'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 14) THEN
!        colourchange
         OPTION(1) =  'LEFT MOUSE =  '
         OPTION(2) =  'INDICATE COLOR'
         OPTION(3) =  'RIGHT MOUSE = '
         OPTION(4) =  'CHANGE PALETTE'
         OPTION(5) =  'ESC = UNDO   ;'
         OPTION(6) =  'Z   = ZOOMIN ;'
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 15) THEN
         OPTION(1) =  'A   = ANCHOR ;'
         OPTION(2) =  '+   = +1 HOUR;'
         OPTION(3) =  '   SPACE BAR ='
         OPTION(4) =  'CONTINUE     ;'
         OPTION(5) =  'Yes SAVEIMAGES'
         OPTION(6) =  'No SAVEIMAGES '
         MAXOPT    =  6
      ELSE IF (NUMB .EQ. 16) THEN    ! editflow
         OPTION(1) =  'A = ANCHOR; '
         OPTION(2) =  'N = Node; '
         OPTION(3) =  'L = Link; '
         OPTION(4) =  'm = SET MIN;'
         OPTION(5) =  'M = SET MAX;'
         OPTION(6) =  'Z = ZOOMIN; '
         OPTION(7) =  'F = FIND link'
         OPTION(8) =  'H = FIND stru'
         MAXOPT    =  8
      ELSE IF (NUMB .EQ. 17) THEN    ! editgrid
         OPTION(1) =  'B = BELL; '
         OPTION(2) =  'D = DELETE; '
         OPTION(3) =  'I = INSERT; '
         OPTION(4) =  'R = REPLACE;'
         MAXOPT    =  4
      ENDIF

      IF (JA .EQ. 2) THEN
         CALL TIMLIN()
         IF (NOPSYS .EQ. 1) THEN
            CALL ITEXTCOLOUR('BBLUE','BWHITE')
         ELSE
            CALL ITEXTCOLOUR('BLACK','BWHITE')
         ENDIF
         CALL INHIGHLIGHT('BWHITE','RED')
         NWHAT  = IMenuHoriz(OPTION,MAXOPT,1,LI,IW,0,1)
         CALL TIMLIN()
      ENDIF
      IF (NOPSYS .EQ. 1) THEN
         CALL InHighlight('BWHITE','WHITE')
         CALL ITEXTCOLOUR('BWHITE','WHITE')
      ELSE
         CALL InHighlight('BLACK','WHITE')
         CALL ITEXTCOLOUR('BLACK','WHITE')
      ENDIF
      CALL IOUTMenuHoriz(OPTION,MAXOPT,1,LI,IW,0,1)
      IF (JA .NE. 2) RETURN


      KEY = InfoInput(55)
      IF (KEY .NE. 23) THEN
         NLEVEL = 3
         WRDKEY = OPTION(NWHAT)
      ENDIF

      IF (KEY .EQ. 21) THEN
!        ins, linker muis
         IF (NWHAT .GE. 1) THEN
            IF (OPTION(NWHAT) .EQ. 'F1 = help    ;') THEN
               KEY = 24
            ELSE IF (OPTION(NWHAT) .EQ. 'F2 = history ;') THEN
               KEY = 25
            ELSE IF (OPTION(NWHAT) .EQ. 'F3 = command ;') THEN
               KEY = 26
            ELSE IF (OPTION(NWHAT) .EQ. 'ESC = UNDO   ;') THEN
               KEY = 23
            ELSE IF (OPTION(NWHAT) .EQ. 'TAB = DCURSOR;') THEN
               KEY = 27
            ELSE IF (OPTION(NWHAT) .EQ. '+   = DEEPER ;') THEN
               KEY = 162
            ELSE IF (OPTION(NWHAT) .EQ. '-   = SHALLOW;') THEN
               KEY = 160
            ELSE IF (OPTION(NWHAT) .EQ. 'P/PRINTSCREEN;') THEN
               KEY = 80
            ELSE IF (OPTION(NWHAT) .EQ. 'DEL = CYCLE  ;') THEN
               KEY = 143
            ELSE IF (OPTION(NWHAT) .EQ. 'No SAVEIMAGES ') THEN
               KEY = 110
            ELSE IF (OPTION(NWHAT) .EQ. 'Yes SAVEIMAGES') THEN
               KEY = 121
            ELSE IF (OPTION(NWHAT) .EQ. 'Z   = ZOOMIN ;') THEN
               KEY  = 90
               NPUT = 2
               CALL ZOOM3(KEY,NPUT)
            ELSE IF (OPTION(NWHAT) .EQ. 'STOP         ;') THEN
               CALL STOPINT()
               ! CALL STOPLOGO()
            ELSE
               KEY    = ICHAR( OPTION(NWHAT)(1:1) )
            ENDIF
         ENDIF
         TEX    = ' ACTIONKEY    '
         WRITE(TEX(12:14),'(I3)') KEY
         CALL KTEXT(TEX,1,5,15)
         RETURN
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         RETURN
      ELSE IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
         GOTO 10
      ELSE
         KEY = 0
         RETURN
      ENDIF

      END
