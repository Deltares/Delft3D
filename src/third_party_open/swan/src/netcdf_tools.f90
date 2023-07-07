module netcdf_tools
   !----- LGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2011-2018.
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
   !  $URL$
   !!--description-----------------------------------------------------------------
   !
   !    Function: - Extension on netcdf library,
   !                to read dimensions/variables case insensitive
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
use netcdf

implicit none

private

character(len=:), allocatable  :: errmsg        !< error message

public :: nf90_inquire_variable_list, nf90_inquire_dimension_list, nccheck, removeErrMsg
public :: names_afreq, names_ndir, convertCstring

character(len=*), parameter :: names_afreq(2) = ['frequency', 'afreq    ']
character(len=*), parameter :: names_ndir(3)  = ['direction', 'ndir     ', 'cdir     ']

contains

    !> helper function to find variable id for one of the variables in the list
    !! search is case in sensitive
    !! multiple entries are assumed to be aliases for each other
    function nf90_inquire_variable_list(ncid, varid, names) result(ierr)
       integer, intent(in)           :: ncid      !< file id
       integer, intent(out)          :: varid     !< variable id
       character(len=*), intent(in)  :: names(:)  !< list of variable names
       integer                       :: ierr      !< function result

       ierr = nf90_inquire_list(ncid, varid, names, 'v')

    end function nf90_inquire_variable_list

    !> helper function to find dimension id for one of the dimensions in the list
    !! search is case in sensitive
    !! multiple entries are assumed to be aliases for each other
    function nf90_inquire_dimension_list(ncid, dimid, names) result(ierr)
       integer, intent(in)           :: ncid      !< file id
       integer, intent(out)          :: dimid     !< variable id
       character(len=*), intent(in)  :: names(:)  !< list of dimension names
       integer                       :: ierr      !< function result

       ierr = nf90_inquire_list(ncid, dimid, names, 'd')

    end function nf90_inquire_dimension_list

    !> joined code for nf90_inquire_dimension_list and nf90_inquire_variable_list
    function nf90_inquire_list(ncid, id, names, ctype) result(ierr)
       integer, intent(in)           :: ncid       !< NetCDF file id
       integer, intent(out)          :: id         !< variable/dimension id
       character(len=*), intent(in)  :: names(:)   !< list with names to compare with
       character, intent(in)         :: ctype      !< 'd' for dimensions, 'v' for variables
       integer                       :: ierr       !< function result

       character(len=:), allocatable  :: namei        !< name for dimension/variable i
       integer                        :: i            !< loop counter
       integer                        :: j            !< loop counter
       integer                        :: nVariables   !< number of variables in NetCDF file
       integer                        :: nDimensions  !< number of dimensions in NetCDF file
       integer                        :: nDim         !< number of variables/dimension

       logical, external :: eqcstr !< function to compare strings, case insensitive. also checks string length, so trim is needed

       id = -1
       ierr = nf90_inquire(ncid, nDimensions, nVariables)

       nDim = merge(nDimensions, nVariables, ctype == 'd')
       namei = repeat(' ', 2 + len(names(1)))

       outer: &
       do i = nDim, 1, -1  ! loop backwards, as it might speedup seach for variables (the first vars are dims)
           if (ctype == 'v') then
               ierr = nf90_inquire_variable(ncid, i, namei)
           else
               ierr = nf90_inquire_dimension(ncid, i, namei)
           endif
           do j = 1, size(names)
               if (eqcstr(trim(names(j)), trim(namei))) then
                   id = i
                   exit outer
               endif
           enddo
       enddo outer

       ! error handling if id not found:
       if ( id < 0 ) then
           errmsg = "NetCDF input file does not appear to have one of the " // merge('dimensions', 'variables ', ctype == 'd')
           do i = 1, size(names)
               errmsg = errmsg // ' "' // trim(names(i)) // '"'
               if (i < size(names)) errmsg = errmsg // ' and'
           enddo
       end if
    end function nf90_inquire_list

    !> check returned error code.
    !! if not OK, print message to screen and stop execution
    subroutine nccheck(status, msg)
        use OCPCOMM4, only : PRINTF
        integer, intent ( in )                   :: status  !< error code returned by a NetCDF function
        character(len=*), intent( in ), optional :: msg     !< optional error message

        integer :: i, units(2)

        if (status /= nf90_noerr) then
            units = [6, PRINTF]
            do i = 1, size(units)
                write(units(i),*) trim(nf90_strerror(status))
                if (allocated(errmsg)) write(units(i),*) errmsg
                if (present(msg)) write(units(i),*) msg
            enddo
            call swexitmpi
            stop 1
        end if
    end subroutine nccheck

    !> convert a C string to an Fortran string,
    !! by searching for char(0) and replace it with spaces untill the end of the string
    subroutine convertCstring(text)
        character(len=*), intent(inout) :: text    !< string to be converted

        integer :: i

        do i = 1, len(text)
            if (text(i:i) == char(0)) then
                text(i:) = ' '
                exit
            endif
        enddo
    end subroutine convertCstring

    !> clean up routine
    subroutine removeErrMsg
        if (allocated(errmsg)) deallocate(errmsg)
    end subroutine removeErrMsg

end module netcdf_tools
