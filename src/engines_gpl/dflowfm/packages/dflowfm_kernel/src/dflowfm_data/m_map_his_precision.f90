!----AGPL --------------------------------------------------------------------
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
!> keeps parameters for map/his writing of data in double/single precision 
module m_map_his_precision
    implicit none
    private
    public netcdf_data_type

    character(len=128), public :: md_nc_map_precision = 'double' !< NetCDF data precision in map files ('double', 'single' or 'float')
    character(len=128), public :: md_nc_his_precision = 'double' !< NetCDF data precision in his files ('double', 'single' or 'float')

    contains

    !> Extract the NetCDF data type from the user provided string.
    function netcdf_data_type(nc_precision_string) result(nc_data_type)
        use netcdf, only: nf90_float, nf90_double
        use MessageHandling, only: mess, LEVEL_ERROR
        use string_module, only: str_lower
        character(len=*), value, intent(in) :: nc_precision_string !< Description of the data type
        integer :: nc_data_type !< Result type as used by the NetCDF library

        call str_lower(nc_precision_string)
        select case (trim(nc_precision_string))
            case ('double')
                nc_data_type = nf90_double
            case ('float', 'single')
                nc_data_type = nf90_float
            case default
                call mess(LEVEL_ERROR, 'Did not recognise NetCDF precision string ' // trim(nc_precision_string) // '. It must be double or single.')
        end select
    end function netcdf_data_type

end module m_map_his_precision
