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
module m_botlin
use m_zoom3, only: zoom3
use m_stopint, only: stopint

   implicit none
contains
   subroutine BOTLIN(JA, NUMB, KEY)
      use m_devices
      use unstruc_display_data, only: jafullbottomline
      use m_helpnow
      use m_ktext
      use m_timlin
      use m_fkeys

      integer :: imenuhoriz
      integer :: infoinput
      integer :: iw
      integer :: ja
      integer :: key
      integer :: li
      integer :: maxop
      integer :: maxopt
      integer :: nput
      integer :: numb
      integer :: nwhat
      parameter(MAXOP=64)
      character(len=14) :: OPTION(MAXOP), TEX
      integer, save :: lastmenuheight = 1

10    continue
      LI = IHS
      IW = IWS
      if (lastmenuheight == 2) then
         call ITEXTCOLOUR('BLACK', 'WHITE')
         call IClearLine(IHS - 1) ! Clear second-last line to erase old menus.
      end if
      lastmenuheight = 1 ! Default (only some are two lines)
      if (NUMB == 0) then
         OPTION(1) = 'CONTINUE     ;'
         OPTION(2) = 'F1 = help    ;'
         OPTION(3) = 'F2 = history ;'
         OPTION(4) = 'P/PRINTSCREEN;'
         OPTION(5) = 'STOP         ;'
         MAXOPT = 5
      else if (NUMB == 1) then
         OPTION(1) = ' = choose  ;'
         OPTION(2) = 'F1 = help    ;'
         OPTION(3) = 'F2 = history ;'
         OPTION(4) = 'Esc= exit    ;'
         MAXOPT = 4
      else if (NUMB == 2) then
         if (jafullbottomline == 1) then
            OPTION(1) = 'A   = ANCHOR ;'
            OPTION(2) = 'I   = INSERT ;'
            OPTION(3) = 'R   = REPLACE;'
            OPTION(4) = 'D   = DELETE ;'
            OPTION(5) = 'X   = SPLIT  ;'
            OPTION(6) = 'e   = ERASEPL;'
            OPTION(7) = 'E   = invERAS;'
            OPTION(8) = 'F   = REFINE ;'
            OPTION(9) = 'M   = MERGE  ;'
            OPTION(10) = 'L   = TO LAND;'
            OPTION(11) = 'N   = TO NET ;'
            OPTION(12) = 'w/W = dropwat;'
            OPTION(13) = 'b/B = droplnd;'
            OPTION(14) = 'TAB = DCURSOR;'
            OPTION(15) = 'ESC = UNDO   ;'
            OPTION(16) = 'Z   = ZOOMIN ;'
            maxopt = 16
            lastmenuheight = 2
         else
            OPTION(1) = 'I=INS '
            OPTION(2) = 'R=REPL '
            OPTION(3) = 'D=DEL '
            OPTION(4) = 'X=SPLIT '
            OPTION(5) = 'e=ERAS '
            OPTION(6) = 'E=inve '
            OPTION(7) = 'F=REF '
            OPTION(8) = 'M=MERG '
            OPTION(9) = 'L=TOLA '
            OPTION(10) = 'N=TONE '
            OPTION(11) = 'w/W=wat '
            OPTION(12) = 'b/B=lnd '
            maxopt = min(12, iws / 12)
         end if
      else if (NUMB == 3) then
         OPTION(1) = 'A   = ANCHOR ;'
         OPTION(2) = 'TAB = DCURSOR;'
         OPTION(3) = 'ESC = UNDO   ;'
         OPTION(4) = 'Z   = ZOOMIN ;'
         MAXOPT = 4
      else if (NUMB == 4) then
         OPTION(1) = '+   = DEEPER ;'
         OPTION(2) = '-   = SHALLOW;'
         OPTION(3) = 'ESC = UNDO   ;'
         OPTION(4) = 'Z   = ZOOMIN ;'
         MAXOPT = 4
      else if (NUMB == 5) then
         OPTION(1) = 'LMS = WINDOW ;'
         OPTION(2) = 'RMS = DEFAULT;'
         OPTION(3) = 'Z = ZOOM OUT ;'
         OPTION(4) = '+   = LARGER ;'
         OPTION(5) = '-   = SMALLER;'
         OPTION(6) = 'ESC = UNDO   ;'
         MAXOPT = 6
      else if (NUMB == 6) then
!        editgridlineBLOK
         OPTION(1) = 'F1 = help    ;'
         OPTION(2) = 'F2 = history ;'
         OPTION(3) = 'P/PRINTSCREEN;'
         OPTION(4) = 'ESC = UNDO   ;'
         OPTION(5) = 'CLICK GRIDLINE'
         OPTION(6) = 'AND INFLUENCE '
         OPTION(7) = 'READY=RIGHT MS'
         MAXOPT = 7
      else if (NUMB == 7) then
!        editgridshift
         OPTION(1) = 'F1 = help    ;'
         OPTION(2) = 'F2 = history ;'
         OPTION(3) = 'P/PRINTSCREEN;'
         OPTION(4) = 'ESC = UNDO   ;'
         OPTION(5) = 'SHIFT THE     '
         OPTION(6) = 'INDICATED LINE'
         OPTION(7) = 'READY=RIGHT MS'
         MAXOPT = 7
      else if (NUMB == 8) then
!        editgridBLOK
         OPTION(1) = 'F1 = help    ;'
         OPTION(2) = 'F2 = history ;'
         OPTION(3) = 'P/PRINTSCREEN;'
         OPTION(4) = 'ESC = UNDO   ;'
         OPTION(5) = 'CLICK A BLOCK;'
         OPTION(6) = 'READY=RIGHT MS'
         MAXOPT = 6
      else if (NUMB == 9) then
         OPTION(1) = 'A   = ANCHOR ;'
         OPTION(2) = 'I   = INSERT ;'
         OPTION(3) = 'R   = REPLACE;'
         OPTION(4) = 'D   = DELETE ;'
         OPTION(5) = 'ESC = UNDO   ;'
         OPTION(6) = 'Z   = ZOOMIN ;'
         OPTION(7) = 'NEW SPLINE RM;'
         OPTION(8) = 'C   = COPY   ;'
         OPTION(9) = 'M   = MOVE   ;'
         OPTION(10) = 'X   = DEL SPL;'
         OPTION(11) = 'L   = TO LAND;'
         MAXOPT = 11
      else if (NUMB == 10) then
         OPTION(1) = 'A = ANCHOR; '
         OPTION(2) = 'I = INSERT; '
         OPTION(3) = 'R = REPLACE;'
         OPTION(4) = 'D = DELETE; '
         OPTION(5) = 'M = MERGE;  ' ! FFFFF
         OPTION(6) = 'G = NET2CURV' ! FFFFF
         OPTION(7) = 'C = CUT;    '
         OPTION(8) = 'X = DELCON; '
         OPTION(9) = 'S = SPLIT; '
         OPTION(10) = 'V = FIELDMOVE' ! fieldmove
         OPTION(11) = 'B = FLDROTATE' ! fieldrotate
         OPTION(12) = '1...4 = KN3;' ! kn(3,:) change 1/2/3/4
         OPTION(13) = 'L = TO LAND;' ! snap to land boundary
         OPTION(14) = 'k = KILL CELL;' ! delete cell and update administration
         OPTION(15) = 'K = DEREFINE; ' ! derefine by 'Casulli-type' killcell
         OPTION(16) = 'E = add layer; ' ! add layer of cells
         lastmenuheight = 2
         MAXOPT = 16
      else if (NUMB == 11) then
         OPTION(1) = '+   = INCREAS;'
         OPTION(2) = '-   = DECREAS;'
         OPTION(3) = 'ESC = UNDO   ;'
         OPTION(4) = 'SPACE BAR =  ;'
         MAXOPT = 4
      else if (NUMB == 12) then
         OPTION(1) = 'A   = ANCHOR ;'
         OPTION(2) = 'I   = INSERT ;'
         OPTION(3) = 'R   = REPLACE;'
         OPTION(4) = 'D   = DELETE ;'
         OPTION(5) = 'C   = CHANGEV;'
         OPTION(6) = 'm   = SET MIN;'
         OPTION(7) = 'M   = SET MAX;'
         OPTION(8) = 'H = hide/show;'
         OPTION(9) = 'ESC = UNDO   ;'
         OPTION(10) = 'Z   = ZOOMIN ;'
         OPTION(11) = 'Q   = sampath;'
         OPTION(12) = 'F   = fldfill;'
         MAXOPT = 12
      else if (NUMB == 13) then
         OPTION(1) = 'A   = ANCHOR ;'
         OPTION(2) = 'I   = INSERTD;'
         OPTION(3) = 'R   = REPLACD;'
         OPTION(4) = 'D   = DELETED;'
         OPTION(5) = 'ESC = UNDO   ;'
         OPTION(6) = 'Z   = ZOOMIN ;'
         MAXOPT = 6
      else if (NUMB == 14) then
!        colourchange
         OPTION(1) = 'LEFT MOUSE =  '
         OPTION(2) = 'INDICATE COLOR'
         OPTION(3) = 'RIGHT MOUSE = '
         OPTION(4) = 'CHANGE PALETTE'
         OPTION(5) = 'ESC = UNDO   ;'
         OPTION(6) = 'Z   = ZOOMIN ;'
         MAXOPT = 6
      else if (NUMB == 15) then
         OPTION(1) = 'A   = ANCHOR ;'
         OPTION(2) = '+   = +1 HOUR;'
         OPTION(3) = '   SPACE BAR ='
         OPTION(4) = 'CONTINUE     ;'
         OPTION(5) = 'Yes SAVEIMAGES'
         OPTION(6) = 'No SAVEIMAGES '
         MAXOPT = 6
      else if (NUMB == 16) then ! editflow
         OPTION(1) = 'A = ANCHOR; '
         OPTION(2) = 'N = Node; '
         OPTION(3) = 'L = Link; '
         OPTION(4) = 'm = SET MIN;'
         OPTION(5) = 'M = SET MAX;'
         OPTION(6) = 'Z = ZOOMIN; '
         OPTION(7) = 'F = FIND link'
         OPTION(8) = 'H = FIND stru'
         MAXOPT = 8
      else if (NUMB == 17) then ! editgrid
         OPTION(1) = 'B = BELL; '
         OPTION(2) = 'D = DELETE; '
         OPTION(3) = 'I = INSERT; '
         OPTION(4) = 'R = REPLACE;'
         MAXOPT = 4
      end if

      if (JA == 2) then
         call TIMLIN()
         if (NOPSYS == 1) then
            call ITEXTCOLOUR('BBLUE', 'BWHITE')
         else
            call ITEXTCOLOUR('BLACK', 'BWHITE')
         end if
         call INHIGHLIGHT('BWHITE', 'RED')
         NWHAT = IMenuHoriz(OPTION, MAXOPT, 1, LI, IW, 0, 1)
         call TIMLIN()
      end if
      if (NOPSYS == 1) then
         call InHighlight('BWHITE', 'WHITE')
         call ITEXTCOLOUR('BWHITE', 'WHITE')
      else
         call InHighlight('BLACK', 'WHITE')
         call ITEXTCOLOUR('BLACK', 'WHITE')
      end if
      call IOUTMenuHoriz(OPTION, MAXOPT, 1, LI, IW, 0, 1)
      if (JA /= 2) return

      KEY = InfoInput(55)
      if (KEY /= 23) then
         NLEVEL = 3
         WRDKEY = OPTION(NWHAT)
      end if

      if (KEY == 21) then
!        ins, linker muis
         if (NWHAT >= 1) then
            if (OPTION(NWHAT) == 'F1 = help    ;') then
               KEY = 24
            else if (OPTION(NWHAT) == 'F2 = history ;') then
               KEY = 25
            else if (OPTION(NWHAT) == 'F3 = command ;') then
               KEY = 26
            else if (OPTION(NWHAT) == 'ESC = UNDO   ;') then
               KEY = 23
            else if (OPTION(NWHAT) == 'TAB = DCURSOR;') then
               KEY = 27
            else if (OPTION(NWHAT) == '+   = DEEPER ;') then
               KEY = 162
            else if (OPTION(NWHAT) == '-   = SHALLOW;') then
               KEY = 160
            else if (OPTION(NWHAT) == 'P/PRINTSCREEN;') then
               KEY = 80
            else if (OPTION(NWHAT) == 'DEL = CYCLE  ;') then
               KEY = 143
            else if (OPTION(NWHAT) == 'No SAVEIMAGES ') then
               KEY = 110
            else if (OPTION(NWHAT) == 'Yes SAVEIMAGES') then
               KEY = 121
            else if (OPTION(NWHAT) == 'Z   = ZOOMIN ;') then
               KEY = 90
               NPUT = 2
               call ZOOM3(KEY, NPUT)
            else if (OPTION(NWHAT) == 'STOP         ;') then
               call STOPINT()
               ! CALL STOPLOGO()
            else
               KEY = ichar(OPTION(NWHAT) (1:1))
            end if
         end if
         TEX = ' ACTIONKEY    '
         write (TEX(12:14), '(I3)') KEY
         call KTEXT(TEX, 1, 5, 15)
         return
      else if (KEY == 23) then
!        ESC
         return
      else if (KEY >= 24 .and. KEY <= 26) then
         call FKEYS(KEY)
         if (KEY == 3) return
         goto 10
      else
         KEY = 0
         return
      end if

   end
end module m_botlin
