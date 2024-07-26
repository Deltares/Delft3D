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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

!> Determines the model extent based on 2D cell vertices and 1D nodes.
!! If applicable, also the geospatial bounds as lat/lon values is set.
!! The min/max variables are set in module m_modelbounds.
function set_model_boundingbox() result(ierr)
   use m_flowgeom
   use m_modelbounds
   use m_sferic, only: jsferic
   use network_data, only: xk, yk
   use dfm_error
   use m_alloc
   use m_missing, only: dmiss
   use unstruc_netcdf, only: crs
   use unstruc_messages
#ifdef HAVE_PROJ
   use coordinate_reference_system, only: transform_coordinates, WGS84_PROJ_STRING
   use proj6
#endif
   implicit none

   integer :: ierr !< Result status (DFM_NOERR if successful)
   integer :: n, k, kk, nv

   double precision, pointer :: lonn(:), latn(:)
   integer :: make_latlon

   ierr = DFM_NOERR

   make_latlon = 0
#ifdef HAVE_PROJ
   if (jsferic == 0) then
      make_latlon = 1
   end if
#else
   if (jsferic == 0) then
      call mess(LEVEL_WARN, 'set_model_boundingbox: cannot set lat/lon model bounds, because PROJ is unavailable.')
      ierr = DFM_GENERICERROR
      goto 999
   end if
#endif

   ! Lat/lon: either take directly from xk/yk for spherical models, or transform using PROJ.
   if (jsferic == 1) then
      lonn => XK
      latn => YK
   else
      call reallocP(lonn, size(xk), fill=dmiss, keepExisting=.false.)
      call reallocP(latn, size(yk), fill=dmiss, keepExisting=.false.)
      if (make_latlon == 1) then
#ifdef HAVE_PROJ
         call transform_coordinates(crs%proj_string, WGS84_PROJ_STRING, XK, YK, lonn, latn)
#endif
      end if
   end if

   mb_xmin = huge(1d0)
   mb_ymin = huge(1d0)
   mb_xmax = -huge(1d0)
   mb_ymax = -huge(1d0)

   mb_lonmin = huge(1d0)
   mb_latmin = huge(1d0)
   mb_lonmax = -huge(1d0)
   mb_latmax = -huge(1d0)

   ! Compute model bounds based on *flowgeom*, intentionally not on network_data (which may include inactive points).
   do n = 1, ndxi
      nv = size(nd(n)%nod)
      do kk = 1, nv ! Loop over cell vertices
         k = nd(n)%nod(kk) ! net node

         mb_xmin = min(mb_xmin, XK(k))
         mb_xmax = max(mb_xmax, XK(k))
         mb_ymin = min(mb_ymin, YK(k))
         mb_ymax = max(mb_ymax, YK(k))

         if (make_latlon == 1) then
            mb_lonmin = min(mb_lonmin, lonn(k))
            mb_lonmax = max(mb_lonmax, lonn(k))
            mb_latmin = min(mb_latmin, latn(k))
            mb_latmax = max(mb_latmax, latn(k))
         end if
      end do
   end do
   if (jsferic == 1) then
      ! Original "x" and "y" were already lonlat, so directly copy min/max
      mb_lonmin = mb_xmin
      mb_lonmax = mb_xmax
      mb_latmin = mb_ymin
      mb_latmax = mb_ymax
   end if

   ! Reset to dmiss if no bounds were set at all.
   if (mb_xmin == huge(1d0)) then
      mb_xmin = dmiss
   end if
   if (mb_ymin == huge(1d0)) then
      mb_ymin = dmiss
   end if
   if (mb_xmax == -huge(1d0)) then
      mb_xmax = dmiss
   end if
   if (mb_ymax == -huge(1d0)) then
      mb_ymax = dmiss
   end if

   if (mb_lonmin == huge(1d0)) then
      mb_lonmin = dmiss
   end if
   if (mb_latmin == huge(1d0)) then
      mb_latmin = dmiss
   end if
   if (mb_lonmax == -huge(1d0)) then
      mb_lonmax = dmiss
   end if
   if (mb_latmax == -huge(1d0)) then
      mb_latmax = dmiss
   end if

888 continue
   ! Successful exit
   return

999 continue
   ! Some error occurred
   return

end function set_model_boundingbox

