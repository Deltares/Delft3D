!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.
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
! Module to validate time-series data, containing several routines to check different
! properies of data.
! Will return an errormessage if data does not adhere to required format.

module m_time_validation
    
  use static_version_info
  use dlwq_data
  
  implicit none
  
  
  contains
  ! Routine to check if a time series in input is strictly increasing. If not, write an error message.
  ! Input:
  ! - lun (file unit for error message)
  ! - data_block (data to validate)
  ! - ierror (error number to be increased)
  ! Output:
  ! - ierror
    subroutine validate_time_series_strictly_increasing(lun, data_block, ierror)
  
      integer               , intent(in   ) :: lun         ! logical unit number for logging error message, if required
      type(t_dlwqdata)      , intent(in   ) :: data_block  ! data block containing time series to validate
      integer               , intent(inout) :: ierror      ! local error count
      character(:), allocatable             :: errformat   ! format for error message
      integer                               :: i
  
      errformat = "(/' ERROR: time value ',I0.1,' not larger than previous time value ',I0.1, '.')"
      do i = 2, size(data_block%times)
        if (data_block%times(i) <= data_block%times(i-1)) then
          write ( lun, errformat) data_block%times(i), data_block%times(i-1)
          ierror = ierror + 1
        end if
      end do
    end subroutine validate_time_series_strictly_increasing
    
end module m_time_validation
