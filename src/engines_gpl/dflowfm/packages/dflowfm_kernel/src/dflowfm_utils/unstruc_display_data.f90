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
module unstruc_display_data
   implicit none

   integer :: ntek = 0
   integer :: plottofile = 0
   integer :: jadatetime = 0
   integer :: jareinitialize = 0

   ! Highlight certain net/flow node/link numbers
   integer :: nhlNetNode = 0 !< Number of netnode  to be highlighted
   integer :: nhlNetLink = 0 !< Number of netlink  to be highlighted
   integer :: nhlFlowNode = 0 !< Number of flownode to be highlighted
   integer :: nhlFlowLink = 0 !< Number of flowlink to be highlighted
   integer :: NPOS(4) !< Size + position of HELP text screen
   integer :: jaHighlight = 0 !< Completely enable/disable highlighting.

   integer :: ndrawPol = 2 !< Polygon, 1=No, 2=Regular, 3=plus numbers ZPL, 4=plus isocolour ZPL
   integer :: ndrawObs = 2 !< Observationstation : 1='NO, 2=Cross, 3=Cross + name4=Polyfil,5='Polyfil + name,6=Cross+waterlevel,7=Cross+velocity magnitudes
   integer :: ndrawCrossSections = 5 !< how draw cross sections
   integer :: ndrawThinDams = 2 !< show thin dams  0=no, 1=polylines, 2=net links
   integer :: ndrawFixedWeirs = 1 !< show fixed weirs 0=no, 1=polylines, 2=flow links
   integer :: ndrawPart = 2 !< Particles, 1=No, 2=Yes
   integer :: ndrawDots = 2 !< dots, 1=No, 2=Yes
   integer :: ndrawStructures = 1 !< structures, 1=No, 2=Yes (only symbols), 3=Yes (symbols and IDs)
   integer :: idisLink = 0 !< Index of flowlink which is to be displayed with more information

   integer :: grwhydopt = 1 !< Groundwater & Hydrology display menu item

   integer :: numzoomshift = 250 !< nr of steps in zoomshift
   double precision :: wetplot = 0.001 !< only show wet waterlevel points if (hs>wetplot)
   double precision :: yfac = 0.0 !< cheap perspective
   integer :: jafullbottomline = 0 !<larger bottomline with more complete description in screen
   double precision :: profmax(20) = -999d0 !< minmax axes of tekprofiles
   double precision :: profmin(20) = -999d0
   double precision :: ymn, zmn ! for tekrailines

end module unstruc_display_data

