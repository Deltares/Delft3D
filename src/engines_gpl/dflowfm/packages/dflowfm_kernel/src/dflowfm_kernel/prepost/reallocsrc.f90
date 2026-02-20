!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_reallocsrc

   implicit none

   private

   public :: reallocsrc

contains

   !> Reallocates all source-sink-related arrays to a desired minimum size.
   !! If arrays are already large enough, nothing is done (specifically, no shrinking is done).
   subroutine reallocsrc(new_size_src, new_num_points)
      use m_transport, only: NUMCONST
      use fm_external_forcings_data, only: source_sink_indices, num_source_sink_max_polyline_points, source_sink_x, source_sink_y, source_sink_water_discharge, dp, source_sink_constituents, source_sink_area, source_sink_discharge_cosine, source_sink_discharge_sine, source_sink_z_bot, source_sink_z_top, srsn, source_sink_extraction_warning, source_sink_discharge, source_sink_name, source_sink_max_num_xy_points, qsrcavg, vsrccum, vsrccum_pre
      use m_alloc, only: realloc
      use m_missing, only: dmiss

      integer, intent(in) :: new_size_src !< Desired *minimum* size of sourcesink arrays.
      integer, intent(in) :: new_num_points !< Desired *minimum* size of x/y arrays.

      integer :: current_size_src

      if (allocated(source_sink_indices)) then
         current_size_src = size(source_sink_indices, 2)
      else
         current_size_src = 0
      end if

      ! Always make sure that the "points arrays" are large enough.
      if (new_size_src > current_size_src .or. new_num_points > num_source_sink_max_polyline_points) then
         num_source_sink_max_polyline_points = max(num_source_sink_max_polyline_points, new_num_points)
         call realloc(source_sink_x, [max(current_size_src, new_size_src), num_source_sink_max_polyline_points], keepExisting=.true., fill=dmiss)
         call realloc(source_sink_y, [max(current_size_src, new_size_src), num_source_sink_max_polyline_points], keepExisting=.true., fill=dmiss)
      end if

      ! Next, make sure that all other arrays are large enough
      if (new_size_src > current_size_src) then
         call realloc(source_sink_indices, [6, new_size_src], keepexisting=.true., fill=0)
         call realloc(source_sink_water_discharge, new_size_src, keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_constituents, [numconst, new_size_src], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_area, new_size_src, keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_discharge_cosine, [2, new_size_src], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_discharge_sine, [2, new_size_src], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_z_bot, [2, new_size_src], keepExisting=.true., fill=dmiss)
         call realloc(source_sink_z_top, [2, new_size_src], keepExisting=.true., fill=dmiss)
         call realloc(srsn, [2 * (numconst + 1), new_size_src], keepExisting=.true.)
         call realloc(source_sink_extraction_warning, new_size_src, keepExisting=.true.)
         call realloc(source_sink_discharge, [(numconst + 1), new_size_src], keepExisting=.true., fill=0.0_dp)
         call realloc(source_sink_name, new_size_src, keepExisting=.true., fill=' ')
         call realloc(source_sink_max_num_xy_points, new_size_src, keepExisting=.true., fill=0)
         call realloc(qsrcavg, new_size_src, keepExisting=.true., fill=0.0_dp)
         call realloc(vsrccum, new_size_src, keepExisting=.true., fill=0.0_dp)
         call realloc(vsrccum_pre, new_size_src, keepExisting=.true., fill=0.0_dp)
      end if

   end subroutine reallocsrc

end module m_reallocsrc
