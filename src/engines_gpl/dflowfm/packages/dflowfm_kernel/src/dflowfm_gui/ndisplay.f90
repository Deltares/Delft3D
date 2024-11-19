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

module m_ndisplay
use m_add_tracer, only: add_tracer
use m_textparameters
use m_setwy


implicit none

contains

   subroutine NDISPLAY(NWHAT, KEY)
      use m_menuv3
      use m_filemenu
      use m_changeisoparameters
      use m_changedisplayparameters
      use m_flow
      use m_flowgeom, only: lnx1d, xz
      use unstruc_display
      use unstruc_model, only: md_ident
      use unstruc_startup, only: initgui
      use m_physcoef, only: ifrctypuni
      use m_sediment
      use m_plotdots
      use m_transport
      use m_waves, only: waveparopt, numoptwav
      use gridoperations
      use m_drawthis
      use m_qnerror
      use m_paramtext
      use m_tek_num_netcells
      use m_set_branch_lc

      integer :: ium
      integer :: maxopt
      integer :: nputz
      integer :: nwhat2, MINP
      integer :: NWHAT, KEY
      logical :: jawel
      character(len=255) :: filnam
      integer :: mfil
      integer :: i
      integer :: ierror
      integer :: numopt
      integer, parameter :: MAXOP = 64
      character * 40 OPTION(MAXOP), exp(MAXOP)

1234  continue

      if (NWHAT == 1) then
         exp(1) = 'MENU 9                                  '
         exp(2) = 'DISPLAY PRESETS                         '
         OPTION(1) = 'Network topology (nrs)                  '
         OPTION(2) = 'Network orthogonality                   '
         OPTION(3) = 'Flow display                            '
         OPTION(4) = 'Load display settings                   '
         OPTION(5) = 'Save current display settings           '
         OPTION(6) = 'Load unstruc.cfg                        '
         OPTION(7) = 'Save unstruc.cfg                        '

         MAXOPT = 7
         NWHAT2 = 0
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 == 1) then ! Network topology
            NDRAW(2) = 1 !    Network solid lines
            NDRAW(16) = 0 ! NO previous network
            NDRAW(19) = 2 ! MODE nodevalues as numbers
            NDRAW(11) = 2 ! MODE linkvalues as numbers
            NDRAW(7) = 2 !    netlink values: numbers
            NDRAW(8) = 2 !    netnode values: numbers
            NDRAW(28) = 1 ! NO flownode values
            NDRAW(29) = 1 ! NO flowlink values
            NDRAW(33) = 2 !    netcell values: numbers
            KEY = 3
         elseif (NWHAT2 == 2) then ! Network orthogonality
            NDRAW(2) = 1 !    Network solid lines
            NDRAW(16) = 0 ! NO previous network
            NDRAW(19) = 5 ! MODE nodevalues as dots
            NDRAW(11) = 5 ! MODE linkvalues as dots
            NDRAW(7) = 4 !    netlink values: orthogonality
            NDRAW(8) = 1 ! NO netnode values
            NDRAW(28) = 1 ! NO flownode values
            NDRAW(29) = 1 ! NO flowlink values
            NDRAW(33) = 1 ! NO netcell values
            KEY = 3
         elseif (NWHAT2 == 3) then ! Flow display
            NDRAW(2) = 1 !    Network solid lines
            NDRAW(16) = 0 ! NO previous network
            NDRAW(19) = 3 ! MODE nodevalues as isofil smooth
            NDRAW(11) = 5 ! MODE linkvalues as dots
            NDRAW(7) = 4 !    netlink values: orthogonality
            NDRAW(8) = 1 ! NO netnode values
            NDRAW(28) = 2 !    flownode values: waterlevel
            NDRAW(29) = 1 ! NO flowlink values
            NDRAW(33) = 1 ! NO netcell values
            KEY = 3
         elseif (NWHAT2 == 4) then ! Load display preset
            FILNAM = '*.cfg'
            MFIL = 0
            call FILEMENU(MFIL, FILNAM, ierror)
            if (ierror == -2) then
               call qnerror('file', filnam, 'not found ')
            else if (mfil /= 0) then
               call initGUI(0) ! NO INTINI
               call doclose(mfil)
               call load_displaysettings(filnam)
               call MESSAGE('YOU LOADED ', filnam, ' ')
               key = 3
            end if
         else if (NWHAT2 == 5) then
            if (len_trim(md_ident) == 0) then
               FILNAM = '*.cfg'
            else
               FILNAM = trim(md_ident)//'.cfg'
            end if
            mfil = 1
            call FILEMENU(mfil, FILNAM, ierror)
            if (ierror == 0) then
               call doclose(mfil)
               call save_displaysettings(filnam)
               call MESSAGE('YOU SAVED ', filnam, ' ')
            end if
         else if (NWHAT2 == 6) then
            call load_displaysettings('unstruc.cfg'); key = 3
         else if (NWHAT2 == 7) then
            call save_displaysettings('unstruc.cfg')
         end if
      elseif (NWHAT == 2) then
         exp(1) = 'MENU 9                                  '
         exp(2) = 'HOW TO DISPLAY THE NETWORK              '
         OPTION(1) = 'NO NETWORK                              '
         OPTION(2) = 'NETWORK SOLID LINES                     '
         OPTION(3) = 'NETWORK SOLID LINES + OUTLINE           '
         OPTION(4) = 'NETWORK OUTLINE ONLY                    '
         OPTION(5) = 'NETWORK + XZ,YZ                         '
         OPTION(6) = 'NETWORK + crossings/quality checks      '
         OPTION(7) = 'NETWORK + on top                        '

         MAXOPT = 7
         NWHAT2 = NDRAW(2) + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 - 1 /= NDRAW(2)) KEY = 3
            NDRAW(2) = NWHAT2 - 1
            if (NDRAW(2) >= 2 .and. NDRAW(2) <= 4) then
               call findcells(0)
            else if (NDRAW(2) == 5) then
               call checknetwork()
               KEY = 3
            end if
         end if
      else if (NWHAT == 3) then
         exp(1) = 'MENU 9                                  '
         exp(2) = 'HOW TO DISPLAY THE PREVIOUS NETWOK      '
         OPTION(1) = 'NO NETWORK                              '
         OPTION(2) = 'NETWORK SOLID LINES                     '
         OPTION(3) = 'OTHER                                   '
         OPTION(4) = 'GRID SPLINE SHAPE SOLID LINES           '
         OPTION(5) = 'GRID SPLINE SHAPE DOTTED LINES          '
         OPTION(6) = 'GRID SOLID LINES PLUS M,N COORDINATES   '
         MAXOPT = 6
         NWHAT2 = NDRAW(16) + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 - 1 /= NDRAW(16)) KEY = 3
            NDRAW(16) = NWHAT2 - 1
         end if
      else if (NWHAT == 4) then
         exp(1) = 'MENU 9                                  '
         exp(2) = 'HOW TO DISPLAY THE SPLINES              '
         OPTION(1) = 'No Splines                              '
         OPTION(2) = 'Splines with Dots                       '
         OPTION(3) = 'Splines                                 '
         MAXOPT = 3
         NWHAT2 = NDRAW(15) + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 - 1 /= NDRAW(15)) KEY = 3
            NDRAW(15) = NWHAT2 - 1
         end if
      else if (NWHAT == 5) then
         exp(1) = 'MENU 10                                 '
         exp(2) = 'HOW TO DISPLAY THE land boundary        '
         OPTION(1) = 'NO land boundary                        '
         OPTION(2) = 'LINES                                   '
         OPTION(3) = 'LINES + DOTS                            '
         OPTION(4) = 'LINES + NRS                             '
         OPTION(5) = 'THICK LINES                             '
         OPTION(6) = 'LINES,        first drawing object      '
         OPTION(7) = 'LINES + DOTS, first drawing object      '
         OPTION(8) = 'LINES + NRS , first drawing object      '
         OPTION(9) = 'THICK LINES , first drawing object      '

         MAXOPT = 9
         NWHAT2 = NDRAW(3) + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 - 1 /= NDRAW(3)) KEY = 3
            NDRAW(3) = NWHAT2 - 1
         end if
      else if (NWHAT == 6) then
         exp(1) = 'MENU 8                                  '
         exp(2) = 'HOW TO DISPLAY NODE VALUES              '
         OPTION(1) = 'NO                                      '
         OPTION(2) = 'NUMBERS                                 '
         OPTION(3) = 'ISOFIL SMOOTH                           '
         OPTION(4) = 'ISOFIL                                  '
         OPTION(5) = 'DOTS                                    '
         OPTION(6) = 'ISOFIL SMOOTH + NUMBERS                 '
         OPTION(7) = 'ISOFIL        + NUMBERS                 '
         OPTION(8) = 'DOTS          + NUMBERS                 '
         OPTION(9) = 'highlight dots smallest 5 %             '
         OPTION(10) = 'highlight dots largest  5 %             '
         MAXOPT = 10
         NWHAT2 = NDRAW(19)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(19)) KEY = 3
         NDRAW(19) = NWHAT2
      else if (NWHAT == 7) then
         exp(1) = 'MENU 8                                  '
         exp(2) = 'HOW TO DISPLAY LINK VALUES              '
         OPTION(1) = 'NO                                      '
         OPTION(2) = 'NUMBERS                                 '
         OPTION(3) = 'ISOfil SMOOTH                           '
         OPTION(4) = 'ISOFIL                                  '
         OPTION(5) = 'DOTS                                    '
         OPTION(6) = 'ISOLINE + NUMBERS                       '
         OPTION(7) = 'ISOFIL  + NUMBERS                       '
         OPTION(8) = 'DOTS    + NUMBERS                       '
         OPTION(9) = 'highlight dots smallest 5 %             '
         OPTION(10) = 'highlight dots largest  5 %             '
         MAXOPT = 10
         NWHAT2 = NDRAW(11)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(11)) KEY = 3
         NDRAW(11) = NWHAT2
      else if (NWHAT == 8) then
         exp(1) = 'MENU 11                                 '
         exp(2) = 'SHOW NODE ADMINISTRATION                '
         OPTION(1) = 'NO NODE VALUES                          '
         OPTION(2) = 'NODE NUMBERS                            '
         OPTION(3) = 'NUMBER OF LINKS ATTACHED TO NODE        '
         OPTION(4) = 'LINK NUMBERS BASED ON NODES             '
         OPTION(5) = 'NODE CODES                              '
         OPTION(6) = 'Vertical level ZK                    (m)'
         OPTION(7) = 'Distance to land boundary               '
         OPTION(8) = 'Erodable Lay. Thickn.                (m)'
         OPTION(9) = 'Vorticity at netnodes              (1/s)'
         OPTION(10) = 'Netnode area BAN                    (m2)'
         OPTION(11) = 'Ship hull vertical level            (m )'
         MAXOPT = 11
         NWHAT2 = NDRAW(8)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(8)) KEY = 3
         NDRAW(8) = NWHAT2
         ! Set default display mode to numbers for nodenums/codes, etc.
         if (nwhat2 == 2 .or. nwhat2 == 3 .or. nwhat2 == 4 .or. nwhat2 == 5 .or. nwhat2 == 7) then
            ndraw(19) = 2
         elseif (ndraw(19) == 2) then ! Set back to default if current is 'numbers'.
            ndraw(19) = 4
         end if
         if (NWHAT2 > 0) then
            call PARAMTEXT(OPTION(NWHAT2), 1)
         end if
      else if (NWHAT == 9) then
         exp(1) = 'MENU 9                                  '
         exp(2) = 'HOW TO DISPLAY THE ELEMENT ADMIN        '
         OPTION(1) = 'NO LINK VALUES                    ( )   '
         OPTION(2) = 'LINK NUMBERS                      ( )   '
         OPTION(3) = 'NODE NUMBERS BASED ON LINKS       ( )   '
         OPTION(4) = 'LINK ORTHOGONALITY COSPHI         ( )   '
         OPTION(5) = '                                  ( )   '
         OPTION(6) = 'LINK CODE LC, branch nr           ( )   '
         OPTION(7) = 'nr of links on branch             ( )   '
         OPTION(8) = '                                  ( )   '
         OPTION(9) = '                                  ( )   '
         OPTION(10) = 'LINK LENGHT                     (  m)   '
         OPTION(11) = 'LINK CODE KN(3,L)               (   )   '
         OPTION(12) = 'LINK, NR OF CONNECTED CELLS LNN (   )   '
         OPTION(13) = 'LINK, CONNECTED CELL NR 1   LNE1(   )   '
         OPTION(14) = 'LINK, CONNECTED CELL NR 2   LNE2(   )   '
         OPTION(15) = 'decrease in topology functional (   )   '
         OPTION(16) = 'smoothness indicator            (   )   '
         OPTION(17) = 'small flow link criterion, dx/(ba12) ( )'
         OPTION(18) = 'dzk / dx                             ( )'

         MAXOPT = 18
         if (jatrt == 1) then
            MAXOPT = 18
            if (ifrctypuni == 0) then
               OPTION(18) = 'Roughness from trachytopes     (Chezy) '
            elseif (ifrctypuni == 1) then
               OPTION(18) = 'Roughness from trachytopes   (Manning) '
            elseif ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then
               OPTION(18) = 'Roughness from trachytopes   (WhitCol) '
            else
               OPTION(18) = 'Roughness from trachytopes      (    ) '
            end if
         end if
         NWHAT2 = NDRAW(7)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(7)) KEY = 3
         NDRAW(7) = NWHAT2
         ! Prepare data
         if (nwhat2 == 4 .or. nwhat2 == 15) then
            if (.not. allocated(xz)) then
               call findcells(0)
            end if
         end if
         ! Set default display mode to numbers for linknums/codes, etc.
         if (nwhat2 == 2 .or. nwhat2 == 3 .or. nwhat2 == 11 .or. nwhat2 == 12 .or. nwhat2 == 13 .or. nwhat2 == 14 .or. nwhat2 == 15) then
            ndraw(11) = 2
         elseif (ndraw(11) == 2 .and. (nwhat2 == 4 .or. nwhat2 == 10)) then ! Set to dots for real values if current is numbers.
            ndraw(11) = 4
         end if
         if (NWHAT2 /= 0) then
            call PARAMTEXT(OPTION(NWHAT2), 2)
         end if
         if (NWHAT2 == 6 .or. NWHAT2 == 7) call SETBRANCH_LC(ium)
      else if (NWHAT == 10) then ! flow nodes
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW flow nodes                         '
         option = ' '
         OPTION(1) = 'NO                                      '
         OPTION(2) = 'Waterlevel                          (m )' ! options for nodes , znod, ndraw(28)
         OPTION(3) = 'Bedlevel                            (m )'
         OPTION(4) = 'Cell area                           (m2)'
         OPTION(5) = 'Free surface area                   (m2)'
         OPTION(6) = 'Volume                              (m3)'
         OPTION(7) = 'Waterdepth                          (m )'
         OPTION(8) = 'Node velocity magnitude            (m/s)'
         OPTION(9) = 'Node x-velocity component          (m/s)'
         OPTION(10) = 'Node y-velocity component          (m/s)'
         OPTION(11) = 'Salinity                           (ppt)'
         OPTION(12) = 'Temperature                       (degC)'
         if (jased > 0) then
            OPTION(13) = 'Sediment concentration           (kg/m3)'
         else if (jagrw > 0) then
            OPTION(13) = 'Ground water level                   (m)'
         else
            OPTION(13) = '                                        '
         end if
         OPTION(14) = 'Froude nr                          (   )'
         OPTION(15) = 'Node nr                            (   )'
         OPTION(16) = 'Nr of links attached to this node  (   )'
         OPTION(17) = 'Kcs                                (   )'
         OPTION(18) = 'Squ sum of q out of cell          (m3/s)'
         OPTION(19) = 'Sqi sum of q in to  cell          (m3/s)'
         OPTION(20) = 'Sqi-squ                           (m3/s)'
         OPTION(21) = 'QW vertical interface velocity    (m /s)'
         OPTION(22) = 'Equilibrium Transport conc.      (kg/m3)'
         OPTION(23) = 'Qin                               (m3/s)'
         OPTION(24) = 'Erodable Lay. Thickn.                (m)'
         OPTION(25) = 'nr of layers                      (    )'
         OPTION(26) = 'vol1/squ                          (s)   '
         OPTION(27) = 'vicwws                            (m2/s)'
         OPTION(28) = 'cg=red, substi=white                    '
         OPTION(29) = 'Tidal potential                  (m2/s2)'
         OPTION(30) = 'Timestep for jaautotimestep >= 1 (   s )'
         OPTION(31) = 'Patm                             (N/m2 )'
         OPTION(32) = 'Numlimdt                                ' ! Velocity head                      (m  )'
         OPTION(33) = 'Total head                         (m  )'
         OPTION(34) = 'Volume error                       (m3 )'

         OPTION(35) = 'Rho                              (kg/m3)'
         OPTION(36) = 'cflmx*vol1(k)/squ(k)               (   )'

         if (soiltempthick == 0d0) then
            OPTION(37) = 'salmase                            (   )'
         else
            OPTION(37) = 'soiltemp                           ( C )'
         end if

         OPTION(38) = 'Layer thickness                    (m  )'

         OPTION(39) = 'Taus 2D                           (N/m2)'
         OPTION(40) = 'Rainfall                        (mm/day)'

         OPTION(41) = 'Humidity                             (%)'
         OPTION(42) = 'Air temperature                      (C)'
         OPTION(43) = 'Cloudiness                           (%)'
         OPTION(44) = 'Solar radiation                   (W/m2)'

         OPTION(45) = 'Constituents                            '

         if (allocated(FrcInternalTides2D)) then
            OPTION(46) = 'FrcInternalTides2D                      '
         else
            OPTION(46) = 'turkinws                                '
         end if

         if (jagrw > 0 .or. jadhyd > 0) then
            OPTION(47) = 'Hydrology & groundwater parameters      '
         end if

         if (nonlin >= 2) then
            OPTION(48) = 'a1m                                 (m2)'
         else if (lnx1D > 0d0) then
            OPTION(48) = 'uc1d                               (m/s)'
         else if (kmx > 0) then
            OPTION(48) = 'max nr of layers                   (   )'
         end if

         if (nshiptxy > 0) then
            OPTION(49) = 'zsp                                  (m)'
         end if
         if (janudge > 0) then
            OPTION(50) = 'Nudge time                           (s)'
         else if (nshiptxy > 0) then
            OPTION(50) = 's1+zsp                               (m)'
         end if
         numopt = 50
         numoptwav = -999
         numoptsf = -999
         numoptsed = -999
         if (jawave > 0) then
            numopt = numopt + 1
            numoptwav = numopt
            OPTION(numoptwav) = 'Wave parameters                         '
         end if

         if (jasecflow > 0) then
            numopt = numopt + 1
            numoptsf = numopt
            OPTION(numoptsf) = 'Spiral flow parameters                  '
         end if

         if (stm_included) then
            numopt = numopt + 1
            numoptsed = numopt
            OPTION(numoptsed) = 'Sediment transport parameters           '
         end if

         MAXOPT = numopt !53

         NWHAT2 = NDRAW(28)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         ! Set default display mode to numbers for nodenums/codes, etc.
         if (nwhat2 == 11 .and. isalt > 0) iconst_cur = isalt
         if (nwhat2 == 12 .and. itemp > 0) iconst_cur = itemp
         if (nwhat2 == 13 .and. ised1 > 0) iconst_cur = ised1

         if (ndraw(19) == 1) then
            if (nwhat2 == 15 .or. nwhat2 == 16) then
               ndraw(19) = 2
            else ! Set back to default if current is 'no'
               ndraw(19) = 4
            end if
         end if
         if (NWHAT2 > 1) then
            call PARAMTEXT(OPTION(NWHAT2), 1)
         end if

         if (NWHAT2 /= NDRAW(28)) KEY = 3
         NDRAW(28) = NWHAT2

         if (ndraw(28) == 24 .and. jaceneqtr == 2) then ! transfer to net node drawing
            ndraw(28) = 0
            ndraw(8) = 8

         else if (ndraw(28) == 45 .and. NUMCONST > 0) then
            if (NUMCONST > 0) then
               ndraw(28) = 1
               nwhat = 37
               goto 1234
            else
               ndraw(28) = 0
            end if

         else if (ndraw(28) == 47) then
            if (jagrw > 0 .or. jadhyd > 0) then
               ndraw(28) = 1
               nwhat = 46 ! Grw & Hydrology submenu
               goto 1234
            else
               ndraw(28) = 0
            end if

         else if (ndraw(28) == numoptwav) then
            if (jawave > 0) then
               ndraw(28) = 1
               nwhat = 42 ! WAVE submenu
               goto 1234
            else
               ndraw(28) = 0
            end if
         else if (ndraw(28) == numoptsf) then
            if (jasecflow > 0) then
               ndraw(28) = 1
               nwhat = 43 ! SECF submenu
               goto 1234
            else
               ndraw(28) = 0
            end if
         else if (ndraw(28) == numoptsed) then
            if (stm_included) then
               ndraw(28) = 1
               nwhat = 45 ! STM flow nodes submenu
               goto 1234
            else
               ndraw(28) = 0
            end if
         end if

      else if (NWHAT == 11) then ! flow links
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW flow links                         '
         OPTION(1) = 'NO                                      '
         OPTION(2) = 'abs(u1)                            (m/s)' ! options for links, zlin, ndraw(29)
         OPTION(3) = 'q1-specific                       (m2/s)'
         OPTION(4) = 'q1                                (m3/s)'
         OPTION(5) = 'au                                  (m2)'
         OPTION(6) = 'hu                                   (m)'
         OPTION(7) = 'user defined friction coefficient frcu  '
         OPTION(8) = 'dx                                      '
         OPTION(9) = 'wu                                      '
         OPTION(10) = 'bob         nd1                         '
         OPTION(11) = 'bob         nd2                         '
         OPTION(12) = 'kcu                                     '
         OPTION(13) = 'horizontal eddy viscosity coeff.  (m2/s)'
         OPTION(14) = 'teta (L)                             ( )'
         OPTION(15) = '                                        '
         OPTION(16) = 'u1                                 (m/s)'
         OPTION(17) = 'adve                              (m/s2)'
         OPTION(18) = 'advi                               (1/s)'
         OPTION(19) = 'FU                                      '
         OPTION(20) = 'RU                                      '
         OPTION(21) = 'suu                               (m/s2)'
         if (javeg == 0) then
            OPTION(22) = 'aifu ()                                 '
            OPTION(23) = 'Local waterlevel slope               ( )'
            OPTION(24) = 'cfuhi=g/(HC2)                      (   )'
         else
            OPTION(22) = 'Plant diameter                       (m)'
            OPTION(23) = 'Plant density                     (1/m2)'
            OPTION(24) = 'Stem  height                       ( m )'
         end if
         OPTION(25) = 'wx      windspeed                  (m/s)'
         OPTION(26) = 'wy      windspeed                  (m/s)'
         OPTION(27) = 'wdsu_x  windstress                (N/m2)'
         OPTION(28) = 'cosphiu , link orthogonality          ()'
         OPTION(29) = 'link nr                                 '
         OPTION(30) = 'tangential velocity                (m/s)'
         OPTION(31) = 'Fu                                 (1/s)'
         OPTION(32) = 'Ru                                 (m/s)'
         OPTION(33) = 'iadv                               (m/s)'
         OPTION(34) = 'plotlin                          (     )'
         OPTION(35) = 'node nr 1, ln(1,L)               (     )'
         OPTION(36) = 'node nr 2, ln(2,L)               (     )'
         OPTION(37) = 'Vorticity                        ( 1/s )'
         OPTION(38) = 'Timestep if jaautotimestep == 2  ( s   )'
         OPTION(39) = 'bottom slope (bl2-bl1)/dx        (     )'
         OPTION(40) = 'IFRCUTP friction type            (     )'
         OPTION(41) = 'turkin0                          (m2/s2)'
         OPTION(42) = 'tureps0                          (1/s  )'
         OPTION(43) = 'vicwwu                           (m2/s )'
         OPTION(44) = 'ustb                             (     )'
         if (jawind > 0) then
            OPTION(45) = 'ustw                             (m/s  )'
         else
            OPTION(45) = 'womegu                           (m/s  )'
         end if
         OPTION(46) = 'Layer Thickness at u             (m    )'
         if (jafrculin > 0) then
            OPTION(47) = 'Linear friction coefficient      (m/s  )'
         else
            OPTION(47) = 'Coriolis parameter fcorio        (1/s  )'
         end if
         if (jawave > 2 .and. jawave < 5) then
            OPTION(48) = 'Wave forcing term at u            (m/s2)'
         else
            OPTION(48) = '                                        '
         end if
         OPTION(49) = 'Number of active layers          (     )'
         OPTION(50) = 'Maximum nr of layers             (     )'
         OPTION(51) = 'Lbot                             (     )'
         OPTION(52) = 'Ltop                             (     )'
         OPTION(53) = 'Smoothness min(AL,AR)/max(AL,AR) (     )'
         numopt = 53
         if (stm_included) then
            numopt = numopt + 1
            numoptsed = numopt
            OPTION(numopt) = 'Sediment transport parameters           '
         end if

         MAXOPT = numopt
         NWHAT2 = NDRAW(29)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(29)) KEY = 3
         ! Set default display mode to numbers for linknums, etc.
         if (nwhat2 == 29) then
            ndraw(11) = 2
         end if
         if (NWHAT2 > 1) then
            call PARAMTEXT(OPTION(NWHAT2), 2)
         end if
         NDRAW(29) = NWHAT2
         if (ndraw(29) == numoptsed) then
            if (stm_included) then
               ndraw(29) = 1
               nwhat = 44 ! STM flow links submenu
               goto 1234
            else
               ndraw(28) = 0
            end if
         end if
      else if (NWHAT == 12) then ! show values at net cell
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW net cells                          '
         OPTION(1) = 'Do not show values at net cells         '
         OPTION(2) = 'Cell numbers                            '
         OPTION(3) = 'Show aspect ratio, <=1 by definition    '
         OPTION(4) = 'Show orientation vectors                '
         OPTION(5) = 'Show aspect ratio plus vectors          '
         OPTION(6) = 'Show cell size                      (m2)'
         OPTION(7) = 'Show cell coarsening information        '
         OPTION(8) = 'Show cell type: tri, quad, penta, hexa  '
         OPTION(9) = 'Normalised circumcentre - gravitycentre '
         OPTION(10) = 'Cell slope                           ( )'
         OPTION(11) = '                                        '
         OPTION(12) = 'Smallest angle                     (deg)'
         OPTION(13) = 'Largest  angle                     (deg)'

         OPTION(14) = '                                        '
         OPTION(15) = 'Partitioning info, domain number        '
         OPTION(16) = 'Partitioning info, number of cells      '
         OPTION(17) = 'Partitioning info, ghostlevels          '
         OPTION(18) = 'Partitioning info, cell-based ghostlevls'
         OPTION(19) = 'Partitioning info, node-based ghostlevls'
         OPTION(20) = 'Partitioning info, global cell number   '

         MAXOPT = 20
         NWHAT2 = NDRAW(33)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(33)) KEY = 3
         NDRAW(33) = NWHAT2
         if (NWHAT2 > 1) then
            call PARAMTEXT(OPTION(NWHAT2), 1)
         end if

         ! Set default display mode
         if (nwhat2 == 3 .or. nwhat2 == 5) then
            ndraw(19) = 3
         elseif (nwhat2 == 2 .or. nwhat2 == 6 .or. nwhat2 == 7) then
            ndraw(19) = 2
         elseif (nwhat2 == 15 .or. nwhat == 16) then
            ndraw(19) = 5
         end if
      else if (NWHAT == 13) then ! show values at cell corners
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW flow links                         '
         OPTION(1) = 'Do NOt show values at flow cell corners '
         OPTION(2) = 'Show x velocity comp                    '
         OPTION(3) = 'Show y velocity comp                    '
         OPTION(4) = 'Show velocity magnitude                 '
         OPTION(5) = 'Show corner velocity vectors            '
         MAXOPT = 5
         NWHAT2 = NDRAW(31)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(31)) KEY = 3
         NDRAW(31) = NWHAT2
      else if (NWHAT == 14) then ! show all flow white line
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW flow links                         '
         OPTION(1) = 'Do NOt show flow links                  '
         OPTION(2) = 'Show All flow links                     '
         OPTION(3) = 'Show All flow link directions           '
         OPTION(4) = 'Show All dry flow links                 '
         OPTION(5) = 'Show All 3D flow links sideview         '
         MAXOPT = 5
         NWHAT2 = NDRAW(30)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 /= NDRAW(30)) KEY = 3
         NDRAW(30) = NWHAT2
      else if (NWHAT == 15) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW vectors YES/NO                     '
         OPTION(1) = 'NO                                      '
         OPTION(2) = 'Velocity                                '
         OPTION(3) = 'Discharge                               '
         OPTION(4) = 'Momentum transport                      '
         OPTION(5) = 'Wind                                    '
         OPTION(6) = 'Wind arcuv                              '
         OPTION(7) = 'Atmospheric pressure arc                '
         OPTION(8) = 'Wind spiderweb                          '
         OPTION(9) = 'Primitive velocity u1                   '
         MAXOPT = 9
         NWHAT2 = NDRAW(13)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            NDRAW(13) = NWHAT2
         end if
         key = 3
      else if (NWHAT == 16) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW observation stations               '
         OPTION(1) = 'NO observation stations                 '
         OPTION(2) = 'Cross                                   '
         OPTION(3) = 'Cross + name                            '
         OPTION(4) = 'Polyfil                                 '
         OPTION(5) = 'Polyfil + name                          '
         OPTION(6) = 'Cross   + waterlevel (m)                '
         OPTION(7) = 'Cross   + waterdepth (m)                '
         OPTION(8) = 'Cross   + velocity magnitudes (m/s)     '
         OPTION(9) = 'Cross   + znod                          '
         OPTION(10) = 'Cross   + temperatures surface + bed   '
         OPTION(11) = 'Cross   + kobs index number             '

         MAXOPT = 10
         NWHAT2 = NDRAWobs
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 /= NDRAWobs) KEY = 3
            NDRAWobs = NWHAT2
         end if
      else if (NWHAT == 17) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW CROSS SECTIONS                     '
         OPTION(1) = 'NO cross sections                       '
         OPTION(2) = 'Line only                               '
         OPTION(3) = 'Line dir                                '
         OPTION(4) = 'Line dir name                           '
         OPTION(5) = 'Line dir discharge             (m3/s)   '
         OPTION(6) = 'Line dir flow area             (m2)     '
         OPTION(7) = 'Line dir ave. velocity         (m/s     '
         OPTION(8) = 'Line dir ave. waterlevel       (m)      '
         OPTION(9) = 'Line dir ave. waterdepth       (m)      '
         OPTION(10) = 'Line dir integrated transport 1(c*m3/sm)'
         OPTION(11) = 'Line dir integrated transport 2(c*m3/sm)'

         MAXOPT = 9
         if (numconst >= 1) MAXOPT = 10
         if (numconst >= 2) MAXOPT = 11
         NWHAT2 = ndrawcrosssections
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 /= NDRAWcrosssections) KEY = 3
            NDRAWcrosssections = NWHAT2
         end if
      else if (NWHAT == 18) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW THIN DAMS YES/NO                   '
         OPTION(1) = 'NO thin dams                            '
         OPTION(2) = 'Thin dam polylines                      '
         OPTION(3) = 'Thin dam net links                      '
         MAXOPT = 3
         NWHAT2 = ndrawThinDams + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         ndrawThinDams = NWHAT2 - 1
         KEY = 3
      else if (NWHAT == 19) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW FIXED WEIRS YES/NO                 '
         OPTION(1) = 'NO fixed weirs                           '
         OPTION(2) = 'Fixed weir flow links                    '
         OPTION(3) = 'Fixed weir flow links + heights          '
         OPTION(4) = 'Fixed weir flow links isocol             '
         OPTION(5) = 'Fixed weir flow links + heights isocol   '
         OPTION(6) = 'Fixed weir only if above water surface   '
         MAXOPT = 6
         NWHAT2 = ndrawFixedWeirs + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         ndrawFixedWeirs = NWHAT2 - 1
         KEY = 3
      else if (NWHAT == 20) then
!     Lege regel
      else if (NWHAT == 21) then
         exp(1) = 'MENU 12                                 '
         exp(2) = 'ISOSCALE YES OR NO                      '
         OPTION(1) = 'ISOSCALE  NODES ON                      '
         OPTION(2) = 'ISOSCALE  LINKS ON                      '
         OPTION(3) = 'ISOSCALES NODES AND LINKS ON            '
         OPTION(4) = 'ISOSCALES OFF                           '
         MAXOPT = 4
         NWHAT2 = NDRAW(12)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 /= NDRAW(12)) KEY = 3
            NDRAW(12) = NWHAT2
         end if
      else if (NWHAT == 22) then
         call SETCOLTABFILE('*.hls               ', 0)
         KEY = 3
      else if (NWHAT == 23) then
         call CHANGEISOPARAMETERS()
         KEY = 3
      else if (NWHAT == 24) then
         call CHANGEDISPLAYPARAMETERS()
         KEY = 3
      else if (NWHAT == 25) then
         call TEXTPARAMETERS()
         KEY = 3
      else if (NWHAT == 26) then
!     Lege regel
      else if (NWHAT == 27) then
         KEY = 90

         inquire (file=trim(md_ident)//'.x1y1x2', exist=jawel)
         if (jawel) then
            call oldfil(minp, trim(md_ident)//'.x1y1x2')
            read (minp, *) x1, y1, x2
            call doclose(minp)
            call setwy(x1, y1, x2)
            key = 3
         else
            NPUTZ = 2
            call ZOOM3(KEY, NPUTZ)
         end if

      else if (NWHAT == 28) then
         KEY = 3
      else if (NWHAT == 29) then
         NDRAW(10) = 1
         KEY = 3
      else if (NWHAT == 30) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW ORTHO YES/NO                       '
         OPTION(1) = 'NO rai                                  '
         OPTION(2) = 'small rai                               '
         OPTION(3) = 'somewhat larger rai                     '
         OPTION(4) = 'larger rai                              '
         OPTION(5) = 'velocity prof                           '
         MAXOPT = 5
         NWHAT2 = NDRAW(18)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(18) = NWHAT2
         KEY = 3
      else if (NWHAT == 31) then
         NDRAW(9) = 2
         KEY = 3
      else if (NWHAT == 32) then
         exp(1) = 'MENU 9                                  '
         exp(2) = 'HOW TO DISPLAY SAMPLE POINTS            '
         OPTION(1) = 'NO SAMPLE POINTS                        '
         OPTION(2) = 'COLOURED DOTS                           '
         OPTION(3) = 'COLOURED DOTS AND CIRCLES               '
         OPTION(4) = 'SMALL POINTS                            '
         OPTION(5) = 'NUMBERS ISOCOLOUR                       '
         OPTION(6) = 'NUMBERS MONOCOLOUR                      '
         OPTION(7) = 'NUMBERS ISOCOLOUR + COLOURED DOTS       '
         OPTION(8) = 'COLOURED SQUARES                        '
         OPTION(9) = 'INDEX NUMBERS                           '
         MAXOPT = 9
         NWHAT2 = max(0, NDRAW(32)) + 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 >= 1) then
            if (NWHAT2 - 1 /= NDRAW(32)) KEY = 3
            NDRAW(32) = NWHAT2 - 1
         end if
      else if (NWHAT == 33) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW ORTHO YES/NO                       '
         OPTION(1) = 'YO BITMAP                               '
         OPTION(2) = 'NO BITMAP                               '
         MAXOPT = 2
         NWHAT2 = NDRAW(26)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(26) = NWHAT2
         KEY = 3
      else if (NWHAT == 34) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW ORTHO YES/NO                       '
         OPTION(1) = 'No Banf                                 '
         OPTION(2) = 'Equilibrium concentration               '
         OPTION(3) = 'Banf flux (ceq - c)                     '
         OPTION(4) = 'Netnode  nr                             '
         OPTION(5) = 'Flownode nr                             '
         OPTION(6) = 'Ban      nr                             '

         MAXOPT = 6
         NWHAT2 = NDRAW(34)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(34) = NWHAT2
         KEY = 3
      else if (NWHAT == 35) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW ORTHO YES/NO                       '
         OPTION(1) = 'No Polygon                              '
         OPTION(2) = 'Polygon red + white dots                '
         OPTION(3) = 'Polygon no dots                         '
         OPTION(4) = 'Polygon isocolour ZPL   Crest levels    '
         OPTION(5) = 'Polygon + numbers ZPL   Crest levels    '
         OPTION(6) = 'Polygon + numbers ZPL   Crest widths    '
         OPTION(7) = 'Polygon + numbers Left  Toe heights     '
         OPTION(8) = 'Polygon + numbers Right Toe heights     '
         OPTION(9) = 'Polygon + numbers Left  Slopes          '
         OPTION(10) = 'Polygon + numbers Right Slopes          '
         OPTION(11) = 'Polygon isocolour Left/Right levels     '
         OPTION(12) = 'Polygon index nrs                       '

         MAXOPT = 12
         NWHAT2 = NDRAWPOL
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAWPOL = NWHAT2
         KEY = 3
      else if (NWHAT == 36) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW ORTHO YES/NO                       '
         OPTION(1) = 'No curvilinear grid                     '
         OPTION(2) = 'Lines                                   '
         OPTION(3) = '                                        '
         OPTION(4) = '                                        '
         OPTION(5) = 'Nr of netcells in gridcells (partition).'
         MAXOPT = 5
         NWHAT2 = NDRAW(38)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(38) = NWHAT2
         if (ndraw(38) == 5) then
            call teknumnetcells(1)
         end if
         KEY = 3

      else if (NWHAT == 37) then ! constituents
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW CONSTITUENTS YES/NO                '
         OPTION(1) = 'NEW TRACER                              '

         do i = 1, NUMCONST
            OPTION(i + 1) = const_names(i)
         end do
         MAXOPT = 1 + NUMCONST
         NWHAT2 = NDRAW(28)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(28) = NWHAT2
         KEY = 3
         iconst_cur = max(nwhat2 - 1, 0)

         if (NWHAT2 == 1) then ! new tracer
            call add_tracer('', iconst_cur)
            nwhat2 = iconst_cur + 1
            option(nwhat2) = const_names(iconst_cur)
         end if
         NDRAW(28) = 45
         if (nwhat2 > 0) then
            call PARAMTEXT(option(nwhat2), 1)
         end if

      else if (NWHAT == 38) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW sorsin                             '
         OPTION(1) = 'No Sources & Sinks                      '
         OPTION(2) = 'black (sin) + white (sor) dots          '
         OPTION(3) = 'idem + Names                            '
         OPTION(4) = 'idem + discharges  (m3/s)               '
         OPTION(5) = 'idem + salinity    (ppt)                '
         OPTION(6) = 'idem + temperature (degC)               '

         MAXOPT = 6
         call MENUV3(Ndraw(41), OPTION, MAXOPT)
         KEY = 3

      else if (NWHAT == 39) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW DOTS YES/NO                        '
         OPTION(1) = 'DO NOT SHOW DOTS                        '
         OPTION(2) = 'SHOW DOTS                               '
         OPTION(3) = 'CLEAR DOTS                              '
         MAXOPT = 3
         NWHAT2 = NDRAWDOTS
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         if (NWHAT2 == 3) then
            call deldots()
         else
            NDRAWDOTS = NWHAT2
         end if
         KEY = 3
      else if (NWHAT == 40) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW STRUCTURES YES/NO                '
         OPTION(1) = 'DO NOT SHOW STRUCTURES                '
         OPTION(2) = 'SHOW STRUCTURES SYMBOLS ONLY          '
         OPTION(3) = 'SHOW STRUCTURES SYMBOLS AND IDS       '
         MAXOPT = 3
         NWHAT2 = 1
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         ndrawStructures = NWHAT2
         KEY = 3

      else if (NWHAT == 41) then
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW PARTICLES YES/NO                   '
         OPTION(1) = 'DO NOT SHOW PARTICLES                   '
         OPTION(2) = 'SHOW PARTICLES                          '
         MAXOPT = 2
         NWHAT2 = NDRAWPART
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAWPART = NWHAT2
         KEY = 3
      else if (NWHAT == 42) then ! wave stuff
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW WAVEPARS YES/NO                    '

         if (jawave == 1 .or. jawave == 2) then
            OPTION(1) = 'RMS wave height  (~ 0.7*Hsig)        (m)'
            OPTION(2) = 'Wave length                          (m)'
            OPTION(3) = 'Peak wave period                     (s)'
            OPTION(4) = 'Orbital velocity at bed            (m/s)'
            OPTION(5) = 'Ustar(w)                           (m/s)'
            OPTION(6) = 'Ustar(w+c)                         (m/s)'
            OPTION(7) = 'Taus(w+c)                         (N/m2)'
            OPTION(8) = 'Ustokes                            (m/s)'
            OPTION(9) = 'Fetchlength                          (m)'
            OPTION(10) = 'Fetchdepth                           (m)'
            MAXOPT = 10
         else
            OPTION(1) = 'RMS wave height                      (m)'
            OPTION(2) = 'Peak waveperiod                      (s)'
            OPTION(3) = 'Total shear stress (c+w)          (N/m2)'
            OPTION(4) = 'Wave force, magnitude             (N/m2)'
            OPTION(5) = 'Ustokes, magnitude                 (m/s)'
            OPTION(6) = 'Wave number                      (rad/m)'
            OPTION(7) = 'sinh(kh)                             (-)'
            OPTION(8) = 'Surface force term                (N/m2)'
            OPTION(9) = 'Body force term                   (N/m2)'
            OPTION(10) = 'Stokes drift, X component          (m/s)'
            OPTION(11) = 'Stokes drift, Y component          (m/s)'
            OPTION(12) = 'Wave energy                          (J)'
            OPTION(13) = 'Roller energy                        (J)'
            OPTION(14) = 'RMS orbital velocity               (m/s)'
            OPTION(15) = 'Wave dissipation                  (W/m2)'
            OPTION(16) = 'Roller dissipation                (W/m2)'
            OPTION(17) = 'Bulk roller energy                   (J)'
            OPTION(18) = 'Radiation stress, X component     (N/m2)'
            OPTION(19) = 'Radiation stress, Y component     (N/m2)'
            OPTION(20) = 'Radiation stress, XY component    (N/m2)'
            OPTION(21) = 'Wave number                      (rad/m)'
            OPTION(22) = 'Wave direction              (deg from N)'
            OPTION(23) = 'Depth gradient, X component          (-)'
            OPTION(24) = 'Depth gradient, Y component          (-)'
            OPTION(25) = 'Wind source term             (J/rad/m/s)'
            OPTION(26) = 'Wave frequency                   (rad/s)'
            OPTION(27) = 'Wave group speed            (m/s in bin)'
            OPTION(28) = ''
            OPTION(29) = 'egradcg                       (J/m/s) '
            OPTION(30) = 'SwT                             (s/s) '
            OPTION(31) = 'SwE                          (J/m2/s) '
            OPTION(32) = 'horadvec                              '
            OPTION(33) = 'horadvec2                             '
            OPTION(34) = 'ma                                    '
            MAXOPT = 34
         end if

         NWHAT2 = NDRAW(28)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(28) = NWHAT2
         KEY = 3
         waveparopt = nwhat2
         if (nwhat2 > 0) then
            call PARAMTEXT(option(nwhat2), 1)
         end if
         NDRAW(28) = numoptwav

      else if (NWHAT == 43) then ! Spiral flow parameters
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW Spiral Flow Parameters YES/NO      '
         OPTION(1) = 'Streamlines curvature              (1/m)'
         OPTION(2) = 'Spiral flow intensity              (m/s)'
         OPTION(3) = 'Dispersion stress by spiral flow  (m/s2)'

         MAXOPT = 3
         NWHAT2 = NDRAW(28)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(28) = NWHAT2
         KEY = 3
         ispirparopt = nwhat2

         NDRAW(28) = numoptsf
         if (nwhat2 > 0) then
            call PARAMTEXT(option(nwhat2), 1)
         end if

      else if (NWHAT == 44) then ! Sed trsp on flow links
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW Mophology Parameters YES/NO        '
         OPTION(1) = 'Curr. rel. bedload transport    (kg/s/m)'
         OPTION(2) = 'Curr. rel. suspended transport  (kg/s/m)'
         OPTION(3) = 'Wave  rel. bedload transport    (kg/s/m)'
         OPTION(4) = 'Wave  rel. susp. transport      (kg/s/m)'
         OPTION(5) = 'Total transport                 (kg/s/m)'
         OPTION(6) = 'Bermslope correction            (kg/s/m)'
         OPTION(7) = 'Bermslope index                      (-)'

         MAXOPT = 7
         NWHAT2 = NDRAW(29)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(29) = NWHAT2
         KEY = 3
         sedparopt = nwhat2

         NDRAW(29) = numoptsed
         if (nwhat2 > 0) then
            call PARAMTEXT(option(nwhat2), 2)
         end if

      else if (NWHAT == 45) then ! Sed trsp on flow nodes
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW Mophology Parameters YES/NO        '
         OPTION(1) = 'Bottom level change in last timestep (m)'
         OPTION(2) = 'Sediment source adv. eq.       (kg/m3/s)'
         OPTION(3) = 'Sediment sink   adv. eq.           (1/s)'
         OPTION(4) = 'Total transport magnitude       (kg/s/m)'

         MAXOPT = 4
         NWHAT2 = NDRAW(28)
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         NDRAW(28) = NWHAT2
         KEY = 3
         sedparopt = nwhat2

         NDRAW(28) = numoptsed
         if (nwhat2 > 0) then
            call PARAMTEXT(option(nwhat2), 1)
         end if

      else if (NWHAT == 46) then ! Groundwater & Hydrology submenu
         exp(1) = 'MENU                                    '
         exp(2) = 'SHOW Groundwater & Hydrology Parameters '
         OPTION(1) = 'Groundwater pressure                 (m)'
         OPTION(2) = ' ' ! Reserved
         OPTION(3) = ' ' ! Reserved
         OPTION(4) = 'Infiltration capacity            (mm/hr)'
         OPTION(5) = ' ' ! Reserved for 'Actual infiltration              (mm/hr)'
         OPTION(6) = 'Interception layer thickness         (m)'
         OPTION(7) = 'Interception layer water depth       (m)'
         OPTION(8) = 'Potential evaporation            (mm/hr)'
         OPTION(9) = 'Actual evaporation open water    (mm/hr)'

         MAXOPT = 9
         NWHAT2 = grwhydopt
         call MENUV3(NWHAT2, OPTION, MAXOPT)
         KEY = 3
         grwhydopt = nwhat2

         NDRAW(28) = 47 ! set back to grw/hyd menu name
         if (nwhat2 > 0) then
            call PARAMTEXT(option(nwhat2), 1)
         end if
      end if
      return
   end subroutine NDISPLAY

end module m_ndisplay
