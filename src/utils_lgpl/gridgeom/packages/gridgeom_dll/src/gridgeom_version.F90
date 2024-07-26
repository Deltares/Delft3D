module gridgeom_version_module
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
#include "version_definition.h"
#include "gridgeom_version.h"

implicit none

    character(*),  public, parameter :: gridgeom_major        = MAJOR_STR
    character(*),  public, parameter :: gridgeom_minor        = MINOR_STR
    character(*),  public, parameter :: gridgeom_revision     = REVISION_STR
    character(*),  public, parameter :: gridgeom_build_number = BUILD_NR
	                                                            
    character(*),  public, parameter :: gridgeom_company      = COMPANY_NAME
    character(*),  public, parameter :: gridgeom_company_url  = COMPANY_URL
    character(*),  public, parameter :: gridgeom_program      = PROGRAM_NAME


#if defined(WIN32)
    character(*),  public, parameter :: gridgeom_architecture = 'Win32' ! used in about box
#elif defined(WIN64)
    character(*),  public, parameter :: gridgeom_architecture = 'Win64' ! used in about box
#else
    character(*),  public, parameter :: gridgeom_architecture = 'unknown' ! used in about box
#endif

    character(*),  public, parameter :: gridgeom_version      = gridgeom_major//'.'//gridgeom_minor//'.'//gridgeom_revision//'.'//gridgeom_build_number
    character(*),  public, parameter :: gridgeom_version_full = gridgeom_company//', '//gridgeom_program//' Version '//gridgeom_version//' ('//gridgeom_architecture// '), '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: gridgeom_version_id   = '@(#)'//gridgeom_version_full

contains

    subroutine getfullversionstring_gridgeom(stringout)
        character(*), intent(out) :: stringout
        integer                   :: length

        length = min(len_trim(gridgeom_version_full),len(stringout))
        stringout = gridgeom_version_id(5:5+length-1)
    end subroutine getfullversionstring_gridgeom

end module gridgeom_version_module
