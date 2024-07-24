!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

module dll_api
   use iso_c_binding
   use unstruc_api
   use unstruc_display, only: jaGUI ! this should be removed when jaGUI = 0 by default

   implicit none

contains

!!> generate the volume table with the given increment
   subroutine dfm_generate_volume_tables(increment) bind(C, name="dfm_generate_volume_tables")
      !DEC$ ATTRIBUTES DLLEXPORT :: dfm_generate_volume_tables

      use messageHandling
      use unstruc_files
      use unstruc_channel_flow
      use m_VolumeTables
      use m_flowgeom
      use m_flowparameters

      real(c_double), value, intent(in) :: increment !< Desired increment for the volume tables
      character(len=Idlen) :: filename
      filename = defaultFilename('volumeTables')
      tableIncrement = increment
      useVolumeTableFile = .false.
      nonlin1d = 1
      call makeVolumeTables(filename, .true.)

   end subroutine dfm_generate_volume_tables

!!> DLL handle to unc_write_1D_flowgeom_ugrid, used by volume tool to write 1D flowgeom
   subroutine write_1D_flowgeom_ugrid(ncid) bind(C, name="write_1D_flowgeom_ugrid")
      !DEC$ ATTRIBUTES DLLEXPORT :: write_1D_flowgeom_ugrid

      use unstruc_netcdf, only: unc_write_1D_flowgeom_ugrid, t_unc_mapids
      use messageHandling, only: Idlen
      use iso_c_utils

      integer, intent(in) :: ncid !< Handle to open Netcdf file to write the geometry to.
      type(t_unc_mapids) :: mapids
      call unc_write_1D_flowgeom_ugrid(mapids%id_tsp, ncid)

   end subroutine write_1D_flowgeom_ugrid

end module dll_api
