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

!> Store the model bounding box coordinates to quickly present this model's extent.
!! If applicable, also geospatial bounds with lat+_lon values can be set.
!!
!! Note: x/y or lat/lon pairs may be set to dmiss if they are not applicable.
module m_modelbounds
   double precision :: mb_xmin !< Smallest x-value (of 2D cell vertices/1D nodes).
   double precision :: mb_xmax !< Largest  x-value (of 2D cell vertices/1D nodes).
   double precision :: mb_ymin !< Smallest y-value (of 2D cell vertices/1D nodes).
   double precision :: mb_ymax !< Largest  y-value (of 2D cell vertices/1D nodes).

   double precision :: mb_lonmin !< Smallest longitude-value (of 2D cell vertices/1D nodes).
   double precision :: mb_lonmax !< Largest  longitude-value (of 2D cell vertices/1D nodes).
   double precision :: mb_latmin !< Smallest latitude-value (of 2D cell vertices/1D nodes).
   double precision :: mb_latmax !< Largest  latitude-value (of 2D cell vertices/1D nodes).

contains

   !> Sets ALL (scalar) variables in this module to their default values.
   !! For a reinit prior to flow computation, call reset_modelbounds() instead.
   subroutine default_modelbounds()
      ! Remaining of variables is handled in reset_modelbounds()
      call reset_modelbounds()
   end subroutine default_modelbounds

   !> Resets only modelbounds variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, use default_modelbounds() instead.
   subroutine reset_modelbounds()
      mb_xmin = huge(1d0) !< Smallest x-value (of 2D cell vertices/1D nodes).
      mb_xmax = -huge(1d0) !< Largest  x-value (of 2D cell vertices/1D nodes).
      mb_ymin = huge(1d0) !< Smallest y-value (of 2D cell vertices/1D nodes).
      mb_ymax = -huge(1d0) !< Largest  y-value (of 2D cell vertices/1D nodes).

      mb_lonmin = huge(1d0) !< Smallest longitude-value (of 2D cell vertices/1D nodes).
      mb_lonmax = -huge(1d0) !< Largest  longitude-value (of 2D cell vertices/1D nodes).
      mb_latmin = huge(1d0) !< Smallest latitude-value (of 2D cell vertices/1D nodes).
      mb_latmax = -huge(1d0) !< Largest  latitude-value (of 2D cell vertices/1D nodes).
   end subroutine reset_modelbounds

end module m_modelbounds
