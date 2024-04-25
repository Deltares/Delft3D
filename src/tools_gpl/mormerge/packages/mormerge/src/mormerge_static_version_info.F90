!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

#include "mormerge_version.h"

module mormerge_static_version_info
    implicit none

    character(*),  public, parameter :: company      = COMPANY_NAME
    character(*),  public, parameter :: company_url  = COMPANY_URL

    character(*),  public, parameter :: product_name = PRODUCT_NAME

    character(*),  public, parameter :: build_nr     = BUILD_NR
    character(*),  public, parameter :: major        = MAJOR_STR
    character(*),  public, parameter :: minor        = MINOR_STR
    character(*),  public, parameter :: revision     = REVISION_STR
    character(*),  public, parameter :: branch       = 'HeadURL: '//BRANCH

    character(*),  public, parameter :: version_full = trim(major)//'.'//trim(minor)//'.'//trim(revision)//'.'//trim(build_nr)
    character(*),  public, parameter :: version_suffix      = version_full
    character(*),  public, parameter :: version_suffix_full = ' Version '//version_suffix//', '//__DATE__//', '//__TIME__
    character(*),  public, parameter :: version_prefix      = company
    character(*),  public, parameter :: version_prefix_id   = '@(#) '// company

    ! While in principle we can use a literal constant, this allows the complier/linker to get rid of the
    ! literal string. Since we want that string to be present in the executable or DLL, we need to use
    ! this trick instead. It fools the compiler into retaining the full string, so that a tool like
    ! "strings" on Linux can extract it.
    integer, protected, save :: offset = 5

end module mormerge_static_version_info
