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

   subroutine MENUV1(NUM, NWHAT)
      use m_netw
      use m_modenow
      use m_menuv2
      use m_fieldopt

      implicit none
      integer :: NUM, NWHAT
      integer :: maxopt
      integer, parameter :: MAXOP = 64
      character(len=40) OPTION(MAXOP), exp(MAXOP)
      integer :: NFO

      if (NUM == 1) then
         exp(1) = 'MENU 1                                  '
         exp(2) = 'FILES                                   '
         OPTION(1) = 'Load MDU-file                    (*.mdu)'
         OPTION(2) = 'Load network      (*.unt/*.net/*_net.nc)'
         OPTION(3) = ' Add network      (*.unt/*.net/*_net.nc)'
         OPTION(4) = 'Load curvilinear grid            (*.grd)'
         OPTION(5) = 'Load arc-info grid               (*.aht)'
!!    OPTION(20)= 'LOAD Untrim grd file,            (*.unt)'
         OPTION(6) = 'Load polygon                     (*.pol)'
         OPTION(7) = 'Load splines                     (*.spl)'
         OPTION(8) = 'Load land boundary               (*.ldb)'
         OPTION(9) = 'Load observation points      (*_obs.xyn)'
         OPTION(10) = ' Add observation points      (*_obs.xyn)'
         OPTION(11) = 'Load cross sections          (*_crs.pli)'
         OPTION(12) = ' add cross sections          (*_crs.pli)'
         OPTION(13) = 'Load thin dams               (*_thd.pli)'
         OPTION(14) = ' add thin dams               (*_thd.pli)'
         OPTION(15) = 'Load samples  (*.xyz/*.dem/*.asc/*.tif*)'
         OPTION(16) = 'Load flow bathymetry (*.xybl or *.xyblu)'
         OPTION(17) = 'Load flow restart             (*_rst.nc)'
         OPTION(18) = 'Load bitmap                      (*.bmp)'
         OPTION(19) = '-                                       '
         OPTION(20) = 'Save MDU-file                    (*.mdu)'
         OPTION(21) = 'Save network                  (*_net.nc)'
         OPTION(22) = 'Save network with cell info   (*_net.nc)'
         OPTION(23) = 'Save network for Google Earth    (*.kml)'
#ifdef HAVE_TECPLOT
         OPTION(24) = 'Save network for Tecplot         (*.plt)'
#else
         OPTION(24) = 'Not available                           '
#endif
         OPTION(25) = 'Save curvilinear grid            (*.grd)'
         OPTION(26) = 'Save polygon                     (*.pol)'
         OPTION(27) = 'Save splines                     (*.spl)'
         OPTION(28) = 'Save land boundary               (*.ldb)'
         OPTION(29) = 'Save observation points      (*_obs.xyn)'
         OPTION(30) = 'Save cross sections          (*_crs.pli)'
         OPTION(31) = 'save samples                     (*.xyz)'
         OPTION(32) = 'save flow bathymetry (*.xybl or *.xyblu)'
         OPTION(33) = 'Save snapshot for restart     (*_rst.nc)'
         OPTION(34) = 'Save snapshot net+s1+u1       (*_map.nc)'
         OPTION(35) = 'TMP read manually preprocessed SVG      '
         OPTION(36) = 'Save SWAN files       (*.node and *.ele)'
         OPTION(37) = 'Save partition files     (*_NNNN_net.nc)'
         OPTION(38) = 'Stop program                            '
         MAXOPT = 38
      else if (NUM == 2) then
         exp(1) = 'MENU 2                                  '
         exp(2) = 'OPERATIONS                              '
         OPTION(1) = 'Undo net                                '
         OPTION(2) = 'Create uniform curvilinear grid         '
         OPTION(3) = 'Create curvilinear grid from splines    '
         OPTION(4) = 'Create curvilinear grid in   polygon    '
         OPTION(5) = 'Create samples in polygon               '
         OPTION(6) = 'Triangulate samples to net in polygon   '
         OPTION(7) = 'Convert grid to net                     '
         OPTION(8) = 'Orthogonalise / Smooth net              '
         OPTION(9) = '-                                       '
         OPTION(10) = 'Refine Polygon                          '
         OPTION(11) = 'Refine quads factor 2 (triangle border) '
         OPTION(12) = 'Refine cells factor 2 (Casulli-type)    '
         OPTION(13) = 'Refine cells and faces factor 2         '
         OPTION(14) = 'Derefine quads factor 2 (Casulli-type)  '
         OPTION(15) = 'Connect curvilinear quads dd type       '
         OPTION(16) = 'Tie Landboundary in network             '
         !OPTION(16)= 'Connect hanging nodes                   '
         OPTION(17) = 'Copy and translate/rotate net           '
         OPTION(18) = 'External triangles to outer quads       '
         OPTION(19) = '-                                       '
         OPTION(20) = '(Re) initialise flow model geometry     '
         OPTION(21) = 'Refresh net adm. (setnodadm + findcells)'
         OPTION(22) = 'Interpolate Network ZK-values in samples'
         OPTION(23) = 'Interpolate Other, see Various/Int. Par.'
         OPTION(24) = 'Make1D2Dinternalnetlinks                '
         OPTION(25) = '                                        '
         ! OPTION(25)= 'Flood fill waterlevels S1 from samples  '
         OPTION(26) = '                                        '
         OPTION(27) = 'Do 1 FLOW step                          '
         OPTION(28) = 'MAKECOARSE2FINETRIANGLECONNECTIONCELLS  '
         OPTION(29) = '                                        '
         OPTION(30) = 'Flip links                              '
         OPTION(31) = 'Coarsen mesh                            '
         OPTION(32) = 'Grow curvilinear grid from splines      '
         OPTION(33) = 'Make triangles from quads, pentas, hexas'
         OPTION(34) = 'Detect ridges in structured sample set  '
         OPTION(35) = '                                        '
         OPTION(36) = '                                        '
         OPTION(37) = 'Gen. domain numbers (polygons or METIS) '
         OPTION(38) = 'Generate dual mesh                      '
         OPTION(39) = 'Diff. samples w. 2nd samples (<tooclose)'
         OPTION(40) = 'Smooth. samples                         '
         OPTION(41) = 'curv. grid to structured triangular grid'

         MAXOPT = 41
      else if (NUM == 3) then
         exp(1) = 'MENU 3                                  '
         exp(2) = 'DISPLAY                                 '
         OPTION(1) = 'Display presets...                      '
         OPTION(2) = 'Display network                         '
         OPTION(3) = 'Display previous state network          '
         OPTION(4) = 'Display splines                         '
         OPTION(5) = 'Display land boundary                   '
         OPTION(6) = 'Display mode net/flow nodes             '
         OPTION(7) = 'Display mode net/flow links             '
         OPTION(8) = 'Values at net  nodes                    '
         OPTION(9) = 'Values at net  links                    '
         OPTION(10) = 'Values at flow nodes                    '
         OPTION(11) = 'Values at flow links                    '
         OPTION(12) = 'Values at net cells                     '
         OPTION(13) = 'Values at cell corners                  '
         OPTION(14) = 'Show all flow links in white            '
         OPTION(15) = 'Display Velocity vectors                '
         OPTION(16) = 'Display Observation points              '
         OPTION(17) = 'Display Cross sections                  '
         OPTION(18) = 'Display Thin Dams                       '
         OPTION(19) = 'Display Fixed Weirs                     '
         OPTION(20) = '-                                       '
         OPTION(21) = 'Isoscale on or off                      '
         OPTION(22) = 'Load colourtable         (in file *.hls)'
         OPTION(23) = 'Change isocolour parameters             '
         OPTION(24) = 'Change display   parameters             '
         OPTION(25) = 'Change text      parameters             '
         OPTION(26) = '-                                       '
         OPTION(27) = 'Zoom in or set zoomwindow of x1y1x2     '
         OPTION(28) = 'Redraw                                  '
         OPTION(29) = 'Hardcopy                                '
         OPTION(30) = 'Show sideview                           '
         OPTION(31) = 'Perspective view                        '
         OPTION(32) = 'Display Samples                         '
         OPTION(33) = 'Show bitmap yes or no                   '
         OPTION(34) = 'Display banf                            '
         OPTION(35) = 'Display Polygon                         '
         OPTION(36) = 'Display Curvilinear grid                '
         OPTION(37) = 'tracers                                 '
         OPTION(38) = 'Display Sources & Sinks                 '
         OPTION(39) = 'Display dots                            '
         OPTION(40) = 'Display structures                      '

         MAXOPT = 40

      else if (NUM == 4) then
         exp(1) = 'MENU 4                                  '
         exp(2) = 'Edit data                               '
         OPTION(1) = 'Edit polygon                            '
         OPTION(2) = 'Edit network                            '
         OPTION(3) = 'Edit splines                            '
         OPTION(4) = 'Edit curvilinear grid                   '
         OPTION(5) = 'Edit samples                            '
         OPTION(6) = 'Show flow nodes                         '
         OPTION(7) = 'Show flow links                         '
         MAXOPT = 7
      else if (NUM == 5) then
         exp(1) = 'MENU 5                                  '
         exp(2) = 'ADDSUBDEL                               '
         OPTION(1) = 'Delete polygon                          '
         OPTION(2) = 'Delete network                          '
         OPTION(3) = 'Delete network based on cell centers.   '
         OPTION(4) = 'Delete splines                          '
         OPTION(5) = 'Delete samples                          '
         OPTION(6) = 'Delete land boundary                 '
         OPTION(7) = 'Delete curvilinear grid                 '
         OPTION(8) = 'Delete observation points               '
         OPTION(9) = 'Remove small flow links from network    '
         OPTION(10) = 'Merge nodes on top of each other        '
         OPTION(11) = '-                                       '
         OPTION(12) = 'Set zero waterdepth                     '
         OPTION(13) = 'Initial waterlevel     +-* uniform value'
         OPTION(14) = 'Initial salinity       +-* uniform value'
         OPTION(15) = 'Curvilinear grid ZC    +-* uniform value'
         OPTION(16) = 'Netw xk coordinates    +-* uniform value'
         OPTION(17) = 'Netw yk coordinates    +-* uniform value'
         OPTION(18) = 'Netw zk coordinates    +-* uniform value'
         OPTION(19) = 'Samples Z value        +-* uniform value'
         OPTION(20) = 'Polygon Z value        +-* uniform value'
         OPTION(21) = 'Netw link codes kn3    +-* uniform value'
         OPTION(22) = '-                                       '
         OPTION(23) = 'Copy ... to polygon                     '
         OPTION(24) = 'Copy polygon to ...                     '
         OPTION(25) = 'Copy ... to samples                     '
         OPTION(26) = 'Copy Land Boundary to 1D network        '
         OPTION(27) = 'Copy network to network (in polgon)     '
         OPTION(28) = 'Shape/Cut network to *.pol files        '
         OPTION(29) = 'Delete network in *.cut files (cutcell) '
         OPTION(30) = '                                        '
         OPTION(31) = 'Merge polylines                         '
         OPTION(32) = 'Delete netnodes with ZK > ZKuni         '
         OPTION(33) = 'Delete netlinks to improve orthogonality'
         OPTION(34) = 'Shift 1D netnodes to duikers.pliz (5col)'
         OPTION(35) = 'Convert crsdef/loc to profdef/loc files '
         OPTION(36) = 'Connecthangingnodes                     '
         OPTION(37) = 'Removelinksofhangingnodes               '
         OPTION(38) = 'MakeZKbedlevels                         '

         MAXOPT = 38
      else if (NUM == 6) then
         exp(1) = 'MENU 6                                  '
         exp(2) = 'VARIOUS                                 '
         OPTION(1) = 'Shortstop                               '
         OPTION(2) = 'Actual and maximum data dimensions      '
         OPTION(3) = 'Change network           parameters     '
         OPTION(4) = 'Change orthogonalisation parameters     '
         OPTION(5) = 'Change curvilinear grid  parameters     '
         OPTION(6) = 'Change interpolation parameters         '
         OPTION(7) = 'Coordinate transformation               '
         OPTION(8) = 'Change flow time         parameters     '
         OPTION(9) = 'Change flow geometry     parameters     '
         OPTION(10) = 'Change flow physical     parameters     '
         OPTION(11) = 'Change flow numerical    parameters     '
         OPTION(12) = 'Change flow numerical    parameters 2   '
         OPTION(13) = 'Change flow numerical    parameters 3   '
         OPTION(14) = 'Change flow numerical    parameters 4   '
         OPTION(15) = 'Change plot colour numbers              '
         MAXOPT = 15
      end if

      if (NUM == 4 .and. MODE == 4) then ! Edit grid submenu
         NFO = NFLD
         call FIELDOPT(NFLD)
         NWHAT = 4
         if (NFLD == 22) then
            call MENUV2(NWHAT, OPTION, MAXOPT)
            NFLD = NFO
         end if
      else
         call MENUV2(NWHAT, OPTION, MAXOPT)
      end if

      return
   end subroutine MENUV1
