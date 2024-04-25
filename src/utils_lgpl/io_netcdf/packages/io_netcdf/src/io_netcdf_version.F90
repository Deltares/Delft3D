module io_netcdf_version_module
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
#include "version_definition.h"
#include "io_netcdf_version.h"

implicit none

    character(*),  public, parameter :: io_netcdf_major        = MAJOR_STR
    character(*),  public, parameter :: io_netcdf_minor        = MINOR_STR
    character(*),  public, parameter :: io_netcdf_revision     = REVISION_STR
    character(*),  public, parameter :: io_netcdf_build_number = BUILD_NR

    character(*),  public, parameter :: io_netcdf_company      = COMPANY_NAME
    character(*),  public, parameter :: io_netcdf_company_url  = COMPANY_URL
    character(*),  public, parameter :: io_netcdf_program      = PROGRAM

#if defined(WIN32)
    character(*),  public, parameter :: io_netcdf_architecture = 'Win32' ! used in about box
#elif defined(WIN64)
    character(*),  public, parameter :: io_netcdf_architecture = 'Win64' ! used in about box
#elif defined(LINUX64)
    character(*),  public, parameter :: io_netcdf_architecture = 'Linux64' ! used in about box
#else
    character(*),  public, parameter :: io_netcdf_architecture = 'unknown' ! used in about box
#endif

    character(*),  public, parameter :: io_netcdf_version      = io_netcdf_major//'.'//io_netcdf_minor//'.'//io_netcdf_revision//'.'//io_netcdf_build_number
    character(*),  public, parameter :: io_netcdf_version_full = io_netcdf_company//', '//io_netcdf_program//' Version '//io_netcdf_version//' ('//io_netcdf_architecture// '), '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: io_netcdf_version_id   = '@(#)'//io_netcdf_version_full

contains

    subroutine getfullversionstring_io_netcdf(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(io_netcdf_version_full),len(stringout))
        stringout = io_netcdf_version_id(5:5+length-1)
    end subroutine getfullversionstring_io_netcdf

end module io_netcdf_version_module
