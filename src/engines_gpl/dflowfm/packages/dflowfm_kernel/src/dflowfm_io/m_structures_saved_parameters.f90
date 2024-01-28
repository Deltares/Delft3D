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
    
module m_structures_saved_parameters

implicit none

integer, PARAMETER :: DEFINE_NCDF_DATA_ID = 1
integer, PARAMETER :: WRITE_DATA_TO_FILE  = 2
integer, PARAMETER :: READ_DATA_FROM_FILE = 3

    contains

!> defines ID for fusav, rusav, ausav in NETCDF, writes and reads these variables to from a NETCDF file
subroutine process_structures_saved_parameters(action, id_file)
use m_flowexternalforcings, only : ncgen, fusav, rusav, ausav
use netcdf
use messagehandling 

implicit none

integer,       intent(in) :: action    !< DEFINE_NCDF_DATA_ID, WRITE_DATA_TO_FILE, or READ_DATA_FROM_FILE
integer,       intent(in) :: id_file   !< restart/map file ID

integer,       PARAMETER  :: ITIM = 1  !< time intervals
integer                   :: id_1, id_ncgen, id_fusav, id_rusav, id_ausav, dim1, status

if ( ncgen > 0 .and. allocated(fusav) ) then
    select case(action)
       case(DEFINE_NCDF_DATA_ID)
          dim1 = size(fusav,1)
          status = nf90_def_dim(id_file, 'id_1', dim1, id_1)
          status = nf90_def_dim(id_file, 'ncgen', ncgen, id_ncgen)

          status = nf90_def_var(id_file, 'general_structure_fu_v1', nf90_double, (/ id_1, id_ncgen /), id_fusav)
          call check_netcdf_status_and_writes_error_message(status, 'with creating general_structure_fu ID')
          status = nf90_put_att(id_file, id_fusav,   'long_name'    , 'partial computational value for fu (under/over/between gate, respectively) of general structure')
          
          status = nf90_def_var(id_file, 'general_structure_ru_v1', nf90_double, (/ id_1, id_ncgen /), id_rusav)
          call check_netcdf_status_and_writes_error_message(status, 'with creating general_structure_ru ID')
          status = nf90_put_att(id_file, id_rusav,   'long_name'    , 'partial computational value for ru (under/over/between gate, respectively) of general structure')
          
          status = nf90_def_var(id_file, 'general_structure_au_v1', nf90_double, (/ id_1, id_ncgen /), id_ausav)
          call check_netcdf_status_and_writes_error_message(status, 'with creating general_structure_au ID')
          status = nf90_put_att(id_file, id_ausav,   'long_name'    , 'partial computational value for au (under/over/between gate, respectively) of general structure')
          
       case(WRITE_DATA_TO_FILE)
          dim1 = size(fusav,1)
          status = nf90_inq_varid(id_file, 'general_structure_fu_v1', id_fusav)
          call check_netcdf_status_and_writes_error_message(status, 'with getting general_structure_fu ID')
          status = nf90_put_var(id_file, id_fusav, fusav, (/1, ITIM/), (/dim1, ncgen/))
          call check_netcdf_status_and_writes_error_message(status, 'with writing general_structure_fu array')
          
          status = nf90_inq_varid(id_file, 'general_structure_ru_v1', id_rusav)
          call check_netcdf_status_and_writes_error_message(status, 'with getting general_structure_ru ID')
          status = nf90_put_var(id_file, id_rusav, rusav, (/1, ITIM/), (/dim1, ncgen/))
          call check_netcdf_status_and_writes_error_message(status, 'with writing general_structure_ru array')
          
          status = nf90_inq_varid(id_file, 'general_structure_au_v1', id_ausav)
          call check_netcdf_status_and_writes_error_message(status, 'with getting general_structure_au ID')
          status = nf90_put_var(id_file, id_ausav, ausav, (/1, ITIM/), (/dim1, ncgen/))
          call check_netcdf_status_and_writes_error_message(status, 'with writing general_structure_au array')
          
     case(READ_DATA_FROM_FILE)
         status = nf90_inq_varid(id_file, 'general_structure_fu_v1', id_fusav)
         if ( status /= nf90_noerr ) then
             call mess(LEVEL_INFO, 'Restart/Map file does not have saved parameters for structures. A certain flow disturbance may occur.')
             return
         end if
         status = nf90_get_var(id_file, id_fusav, fusav)
         call check_netcdf_status_and_writes_error_message(status, 'with reading fusav array')
        
         status = nf90_inq_varid(id_file, 'general_structure_ru_v1', id_rusav)
         call check_netcdf_status_and_writes_error_message(status, 'with reading general_structure_ru ID')
         status = nf90_get_var(id_file, id_rusav, rusav)
         call check_netcdf_status_and_writes_error_message(status, 'with reading general_structure_ru array')
         
         status = nf90_inq_varid(id_file, 'general_structure_au_v1', id_ausav)
         call check_netcdf_status_and_writes_error_message(status, 'with reading general_structure_au ID')
         status = nf90_get_var(id_file, id_ausav, ausav)
         call check_netcdf_status_and_writes_error_message(status, 'with reading general_structure_au array')
         
    end select
end if
    
end subroutine process_structures_saved_parameters

!> checks netcdf status and prints a message when there is an error
subroutine check_netcdf_status_and_writes_error_message(status, message)
use netcdf
use messagehandling 
use iso_varying_string

implicit none

integer,      intent(in) :: status
character(*), intent(in) :: message

if ( status /= nf90_noerr ) then
    call mess(LEVEL_ERROR, 'NetCDF error: ', nf90_strerror(status), trim(message))
end if

end subroutine check_netcdf_status_and_writes_error_message

end module m_structures_saved_parameters