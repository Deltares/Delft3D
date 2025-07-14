!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! ftnunit_store.f90 --
!     Module to store and retrieve arrays of data for the
!     "ftnunit" framework:
!     Quite often numerical routines require a lot of input
!     data and produce a lot of output data. This module
!     provides routines to store such data in an external
!     file and to retrieve them again at testing time.
!     The files it handles are simple unformatted files,
!     but they hold enough information to accomplish
!     the storage and retrieval:
!     - A header identifying the type of file
!     - A short description of the contents
!     - Per array:
!       - A record with metadata (data type and array dimensions)
!       - A record with the actual data
!
module ftnunit_store

    implicit none

    private
    integer, parameter :: single = kind(1.0)
    integer, parameter :: double = kind(1.0d0)

    character(len=40), parameter :: ftnunit_header    = 'FTNUNIT 1.0'
    character(len=10), parameter :: ftnunit_integer   = 'INTEGER'
    character(len=10), parameter :: ftnunit_logical   = 'LOGICAL'
    character(len=10), parameter :: ftnunit_real      = 'REAL'
    character(len=10), parameter :: ftnunit_double    = 'DOUBLE'
    character(len=10), parameter :: ftnunit_character = 'CHARACTER'
    character(len=10), parameter :: ftnunit_complex   = 'COMPLEX'

    interface test_retrieve_data
        module procedure test_retrieve_data_integer
        module procedure test_retrieve_data_integer1d
        module procedure test_retrieve_data_integer2d
        module procedure test_retrieve_data_integer3d
        module procedure test_retrieve_data_real
        module procedure test_retrieve_data_real1d
        module procedure test_retrieve_data_real2d
        module procedure test_retrieve_data_real3d
        module procedure test_retrieve_data_double
        module procedure test_retrieve_data_double1d
        module procedure test_retrieve_data_double2d
        module procedure test_retrieve_data_double3d
        module procedure test_retrieve_data_complex
        module procedure test_retrieve_data_complex1d
        module procedure test_retrieve_data_complex2d
        module procedure test_retrieve_data_complex3d
        module procedure test_retrieve_data_logical
        module procedure test_retrieve_data_logical1d
        module procedure test_retrieve_data_logical2d
        module procedure test_retrieve_data_logical3d
        module procedure test_retrieve_data_character
        module procedure test_retrieve_data_character1d
        module procedure test_retrieve_data_character2d
        module procedure test_retrieve_data_character3d
    end interface

    interface test_store_data
        module procedure test_store_data_integer
        module procedure test_store_data_integer1d
        module procedure test_store_data_integer2d
        module procedure test_store_data_integer3d
        module procedure test_store_data_real
        module procedure test_store_data_real1d
        module procedure test_store_data_real2d
        module procedure test_store_data_real3d
        module procedure test_store_data_double
        module procedure test_store_data_double1d
        module procedure test_store_data_double2d
        module procedure test_store_data_double3d
        module procedure test_store_data_complex
        module procedure test_store_data_complex1d
        module procedure test_store_data_complex2d
        module procedure test_store_data_complex3d
        module procedure test_store_data_logical
        module procedure test_store_data_logical1d
        module procedure test_store_data_logical2d
        module procedure test_store_data_logical3d
        module procedure test_store_data_character
        module procedure test_store_data_character1d
        module procedure test_store_data_character2d
        module procedure test_store_data_character3d
    end interface

    public :: test_retrieve_data, test_store_data
    public :: test_open_storage_file, test_close_storage_file
    ! public :: test_print_storage_file_summary

contains

! test_open_storage_file -
!     Open a new or existing storage file
!
! Arguments:
!     filename       Name of the file (input)
!     lun            LU-number of the opened file (output)
!     desc           Short description (input/output)
!                    (At most 80 characters)
!     output         Whether to open for output or not
!                    (Optional; default: input)
!
! Note:
!     The routine is rather fussy about these files:
!     An existing file will not be overwritten and a
!     file with the wrong contents is refused.
!
subroutine test_open_storage_file( filename, lun, desc, output )
    character(len=*), intent(in)    :: filename
    integer, intent(out)            :: lun
    character(len=*), intent(inout) :: desc
    logical, intent(in), optional   :: output

    logical                         :: output_
    character(len=80)               :: desc_
    character(len=40)               :: header
    integer                         :: ierr

    output_ = .false.
    if ( present(output) ) then
        output_ = output
    endif

    !
    ! Open the file and check
    !

    if ( output_ ) then
        open( newunit = lun, file = filename, form = 'unformatted', status = 'new', iostat = ierr )

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: unable to open file "' // trim(filename) // ' for writing'
            write(*,*) '    It should not exist yet - please check'
            stop
        endif

        write( lun ) ftnunit_header
        desc_ = desc
        write( lun ) desc_  ! Ensure a known length of the description

    else
        open( newunit = lun, file = filename, form = 'unformatted', status = 'old', iostat = ierr )

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: unable to open file "' // trim(filename) // ' for reading'
            write(*,*) '    It should exist - please check'
            stop
        endif

        read( lun, iostat = ierr ) header

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: corrupt file "' // trim(filename)
            write(*,*) '    Unable to read the header - please check'
            stop
        endif

        if ( header /= ftnunit_header ) then
            write(*,*) 'FTNUNIT: incorrect file "' // trim(filename)
            write(*,*) '    It is not a valid ftnunit storage file - please check'
            stop
        endif

        read( lun, iostat = ierr ) desc_  ! Ensure a known length of the description

        if ( ierr /= 0 ) then
            write(*,*) 'FTNUNIT: corrupt file "' // trim(filename)
            write(*,*) '    Unable to read the description - please check'
            stop
        endif

        desc = desc_
    endif

end subroutine test_open_storage_file


! test_close_storage_file -
!     Close the storage file
!
! Arguments:
!     lun            LU-number of the opened file (output)
!
! Note:
!     Just for symmetry
!
subroutine test_close_storage_file( lun )
    integer, intent(in)             :: lun

    close( lun )

end subroutine test_close_storage_file


! test_store_data_integer* -
!     Store integer data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_integer( lun, data, desc )
    integer, intent(in)             :: lun
    integer, intent(in)             :: data
    character(len=*), intent(in)    :: desc

    character(len=40)               :: desc_
    integer, dimension(10)          :: dimensions

    desc_ = desc
    dimensions = 0

                                  !char length, #dimensions
    write( lun ) ftnunit_integer, 0, 0, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_integer


subroutine test_store_data_integer1d( lun, data, desc )
    integer, intent(in)               :: lun
    integer, intent(in), dimension(:) :: data
    character(len=*), intent(in)      :: desc

    character(len=40)                 :: desc_
    integer, dimension(10)            :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_integer, 0, 1, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_integer1d


subroutine test_store_data_integer2d( lun, data, desc )
    integer, intent(in)                 :: lun
    integer, intent(in), dimension(:,:) :: data
    character(len=*), intent(in)        :: desc

    character(len=40)                   :: desc_
    integer, dimension(10)              :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_integer, 0, 2, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_integer2d


subroutine test_store_data_integer3d( lun, data, desc )
    integer, intent(in)                   :: lun
    integer, intent(in), dimension(:,:,:) :: data
    character(len=*), intent(in)          :: desc

    character(len=40)                     :: desc_
    integer, dimension(10)                :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_integer, 0, 3, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_integer3d


! test_retrieve_data_integer* -
!     Retrieve integer data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_retrieve_data_integer( lun, data, desc )
    integer, intent(in)             :: lun
    integer, intent(out)            :: data
    character(len=*), intent(out)   :: desc

    character(len=10)               :: type
    character(len=40)               :: desc_
    integer, dimension(10)          :: dimensions
    integer                         :: ierr
    integer                         :: char_length
    integer                         :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving integer data - please check'
        stop
    endif

    if ( type /= ftnunit_integer ) then
        write(*,*) 'FTNUNIT: expected integer data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 0 ) then
        write(*,*) 'FTNUNIT: expected scalar integer data - got array instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_integer


subroutine test_retrieve_data_integer1d( lun, data, desc )
    integer, intent(in)               :: lun
    integer, pointer, dimension(:)    :: data
    character(len=*)                  :: desc

    character(len=10)                 :: type
    character(len=40)                 :: desc_
    integer, dimension(10)            :: dimensions
    integer                           :: ierr
    integer                           :: char_length
    integer                           :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving integer data - please check'
        stop
    endif

    if ( type /= ftnunit_integer ) then
        write(*,*) 'FTNUNIT: expected integer data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 1 ) then
        write(*,*) 'FTNUNIT: expected one-dimensional integer array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_integer1d


subroutine test_retrieve_data_integer2d( lun, data, desc )
    integer, intent(in)               :: lun
    integer, pointer, dimension(:,:)  :: data
    character(len=*)                  :: desc

    character(len=10)                 :: type
    character(len=40)                 :: desc_
    integer, dimension(10)            :: dimensions
    integer                           :: ierr
    integer                           :: char_length
    integer                           :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving integer data - please check'
        stop
    endif

    if ( type /= ftnunit_integer ) then
        write(*,*) 'FTNUNIT: expected integer data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 2 ) then
        write(*,*) 'FTNUNIT: expected two-dimensional integer array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_integer2d


subroutine test_retrieve_data_integer3d( lun, data, desc )
    integer, intent(in)                :: lun
    integer, pointer, dimension(:,:,:) :: data
    character(len=*)                   :: desc

    character(len=10)                  :: type
    character(len=40)                  :: desc_
    integer, dimension(10)             :: dimensions
    integer                            :: ierr
    integer                            :: char_length
    integer                            :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving integer data - please check'
        stop
    endif

    if ( type /= ftnunit_integer ) then
        write(*,*) 'FTNUNIT: expected integer data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 3 ) then
        write(*,*) 'FTNUNIT: expected three-dimensional integer array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2), dimensions(3)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_integer3d


! test_store_data_real* -
!     Store real data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_real( lun, data, desc )
    integer, intent(in)          :: lun
    real, intent(in)             :: data
    character(len=*), intent(in) :: desc

    character(len=40)            :: desc_
    integer, dimension(10)       :: dimensions

    desc_ = desc
    dimensions = 0

    write( lun ) ftnunit_real, 0, 0, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_real


subroutine test_store_data_real1d( lun, data, desc )
    integer, intent(in)            :: lun
    real, intent(in), dimension(:) :: data
    character(len=*), intent(in)   :: desc

    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_real, 0, 1, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_real1d


subroutine test_store_data_real2d( lun, data, desc )
    integer, intent(in)              :: lun
    real, intent(in), dimension(:,:) :: data
    character(len=*), intent(in)     :: desc

    character(len=40)                :: desc_
    integer, dimension(10)           :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_real, 0, 2, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_real2d


subroutine test_store_data_real3d( lun, data, desc )
    integer, intent(in)                :: lun
    real, intent(in), dimension(:,:,:) :: data
    character(len=*), intent(in)       :: desc

    character(len=40)                  :: desc_
    real, dimension(10)                :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_real, 0, 3, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_real3d


! test_retrieve_data_real* -
!     Retrieve real data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_retrieve_data_real( lun, data, desc )
    integer, intent(in)           :: lun
    real, intent(out)             :: data
    character(len=*), intent(out) :: desc

    character(len=10)             :: type
    character(len=40)             :: desc_
    integer, dimension(10)        :: dimensions
    integer                       :: ierr
    integer                       :: char_length
    integer                       :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving real data - please check'
        stop
    endif

    if ( type /= ftnunit_real ) then
        write(*,*) 'FTNUNIT: expected real data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 0 ) then
        write(*,*) 'FTNUNIT: expected scalar real data - got array instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_real


subroutine test_retrieve_data_real1d( lun, data, desc )
    integer, intent(in)            :: lun
    real, pointer, dimension(:)    :: data
    character(len=*)               :: desc

    character(len=10)              :: type
    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions
    integer                        :: ierr
    integer                        :: char_length
    integer                        :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving real data - please check'
        stop
    endif

    if ( type /= ftnunit_real ) then
        write(*,*) 'FTNUNIT: expected real data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 1 ) then
        write(*,*) 'FTNUNIT: expected one-dimensional real array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_real1d


subroutine test_retrieve_data_real2d( lun, data, desc )
    integer, intent(in)            :: lun
    real, pointer, dimension(:,:)  :: data
    character(len=*)               :: desc

    character(len=10)              :: type
    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions
    integer                        :: ierr
    integer                        :: char_length
    integer                        :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving real data - please check'
        stop
    endif

    if ( type /= ftnunit_real ) then
        write(*,*) 'FTNUNIT: expected real data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 2 ) then
        write(*,*) 'FTNUNIT: expected two-dimensional real array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_real2d


subroutine test_retrieve_data_real3d( lun, data, desc )
    integer, intent(in)             :: lun
    real, pointer, dimension(:,:,:) :: data
    character(len=*)                :: desc

    character(len=10)               :: type
    character(len=40)               :: desc_
    integer, dimension(10)          :: dimensions
    integer                         :: ierr
    integer                         :: char_length
    integer                         :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving real data - please check'
        stop
    endif

    if ( type /= ftnunit_real ) then
        write(*,*) 'FTNUNIT: expected real data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 3 ) then
        write(*,*) 'FTNUNIT: expected three-dimensional real array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2), dimensions(3)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_real3d


! test_store_data_double* -
!     Store double data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_double( lun, data, desc )
    integer, intent(in)            :: lun
    real(kind=double), intent(in)  :: data
    character(len=*), intent(in)   :: desc

    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions

    desc_ = desc
    dimensions = 0

    write( lun ) ftnunit_double, 0, 0, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_double


subroutine test_store_data_double1d( lun, data, desc )
    integer, intent(in)                         :: lun
    real(kind=double), intent(in), dimension(:) :: data
    character(len=*), intent(in)                :: desc

    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_double, 0, 1, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_double1d


subroutine test_store_data_double2d( lun, data, desc )
    integer, intent(in)                           :: lun
    real(kind=double), intent(in), dimension(:,:) :: data
    character(len=*), intent(in)                  :: desc

    character(len=40)                             :: desc_
    integer, dimension(10)                        :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_double, 0, 2, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_double2d


subroutine test_store_data_double3d( lun, data, desc )
    integer, intent(in)                             :: lun
    real(kind=double), intent(in), dimension(:,:,:) :: data
    character(len=*), intent(in)                    :: desc

    character(len=40)                               :: desc_
    integer, dimension(10)                          :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_double, 0, 3, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_double3d


! test_retrieve_data_double* -
!     Retrieve double data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_retrieve_data_double( lun, data, desc )
    integer, intent(in)                        :: lun
    real(kind=double), intent(out)             :: data
    character(len=*), intent(out)              :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                    :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving double data - please check'
        stop
    endif

    if ( type /= ftnunit_double ) then
        write(*,*) 'FTNUNIT: expected double data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 0 ) then
        write(*,*) 'FTNUNIT: expected scalar double data - got array instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_double


subroutine test_retrieve_data_double1d( lun, data, desc )
    integer, intent(in)                         :: lun
    real(kind=double), pointer, dimension(:)    :: data
    character(len=*)                            :: desc

    character(len=10)                           :: type
    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions
    integer                                     :: ierr
    integer                                     :: char_length
    integer                                     :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving double data - please check'
        stop
    endif

    if ( type /= ftnunit_double ) then
        write(*,*) 'FTNUNIT: expected double data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 1 ) then
        write(*,*) 'FTNUNIT: expected one-dimensional double array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_double1d


subroutine test_retrieve_data_double2d( lun, data, desc )
    integer, intent(in)                        :: lun
    real(kind=double), pointer, dimension(:,:)  :: data
    character(len=*)                           :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                    :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving double data - please check'
        stop
    endif

    if ( type /= ftnunit_double ) then
        write(*,*) 'FTNUNIT: expected double data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 2 ) then
        write(*,*) 'FTNUNIT: expected two-dimensional double array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_double2d


subroutine test_retrieve_data_double3d( lun, data, desc )
    integer, intent(in)                          :: lun
    real(kind=double), pointer, dimension(:,:,:) :: data
    character(len=*)                             :: desc

    character(len=10)                            :: type
    character(len=40)                            :: desc_
    integer, dimension(10)                       :: dimensions
    integer                                      :: ierr
    integer                                      :: char_length
    integer                                      :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving double data - please check'
        stop
    endif

    if ( type /= ftnunit_double ) then
        write(*,*) 'FTNUNIT: expected double data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 3 ) then
        write(*,*) 'FTNUNIT: expected three-dimensional double array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2), dimensions(3)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_double3d


! test_store_data_logical* -
!     Store logical data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_logical( lun, data, desc )
    integer, intent(in)            :: lun
    logical, intent(in)            :: data
    character(len=*), intent(in)   :: desc

    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions

    desc_ = desc
    dimensions = 0

    write( lun ) ftnunit_logical, 0, 0, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_logical


subroutine test_store_data_logical1d( lun, data, desc )
    integer, intent(in)                         :: lun
    logical, intent(in), dimension(:)           :: data
    character(len=*), intent(in)                :: desc

    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_logical, 0, 1, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_logical1d


subroutine test_store_data_logical2d( lun, data, desc )
    integer, intent(in)                           :: lun
    logical, intent(in), dimension(:,:)           :: data
    character(len=*), intent(in)                  :: desc

    character(len=40)                             :: desc_
    integer, dimension(10)                        :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_logical, 0, 2, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_logical2d


subroutine test_store_data_logical3d( lun, data, desc )
    integer, intent(in)                             :: lun
    logical, intent(in), dimension(:,:,:)           :: data
    character(len=*), intent(in)                    :: desc

    character(len=40)                               :: desc_
    integer, dimension(10)                          :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_logical, 0, 3, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_logical3d


! test_retrieve_data_logical* -
!     Retrieve logical data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_retrieve_data_logical( lun, data, desc )
    integer, intent(in)                        :: lun
    logical, intent(out)                       :: data
    character(len=*), intent(out)              :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                     :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving logical data - please check'
        stop
    endif

    if ( type /= ftnunit_logical ) then
        write(*,*) 'FTNUNIT: expected logical data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 0 ) then
        write(*,*) 'FTNUNIT: expected scalar logical data - got array instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_logical


subroutine test_retrieve_data_logical1d( lun, data, desc )
    integer, intent(in)                         :: lun
    logical, pointer, dimension(:)              :: data
    character(len=*)                            :: desc

    character(len=10)                           :: type
    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions
    integer                                     :: ierr
    integer                                     :: char_length
    integer                                     :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving logical data - please check'
        stop
    endif

    if ( type /= ftnunit_logical ) then
        write(*,*) 'FTNUNIT: expected logical data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 1 ) then
        write(*,*) 'FTNUNIT: expected one-dimensional logical array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_logical1d


subroutine test_retrieve_data_logical2d( lun, data, desc )
    integer, intent(in)                        :: lun
    logical, pointer, dimension(:,:)           :: data
    character(len=*)                           :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                    :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving logical data - please check'
        stop
    endif

    if ( type /= ftnunit_logical ) then
        write(*,*) 'FTNUNIT: expected logical data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 2 ) then
        write(*,*) 'FTNUNIT: expected two-dimensional logical array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_logical2d


subroutine test_retrieve_data_logical3d( lun, data, desc )
    integer, intent(in)                          :: lun
    logical, pointer, dimension(:,:,:)           :: data
    character(len=*)                             :: desc

    character(len=10)                            :: type
    character(len=40)                            :: desc_
    integer, dimension(10)                       :: dimensions
    integer                                      :: ierr
    integer                                      :: char_length
    integer                                      :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving logical data - please check'
        stop
    endif

    if ( type /= ftnunit_logical ) then
        write(*,*) 'FTNUNIT: expected logical data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 3 ) then
        write(*,*) 'FTNUNIT: expected three-dimensional logical array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2), dimensions(3)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_logical3d


! test_store_data_complex* -
!     Store complex data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_complex( lun, data, desc )
    integer, intent(in)            :: lun
    complex, intent(in)            :: data
    character(len=*), intent(in)   :: desc

    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions

    desc_ = desc
    dimensions = 0

    write( lun ) ftnunit_complex, 0, 0, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_complex


subroutine test_store_data_complex1d( lun, data, desc )
    integer, intent(in)                         :: lun
    complex, intent(in), dimension(:)           :: data
    character(len=*), intent(in)                :: desc

    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_complex, 0, 1, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_complex1d


subroutine test_store_data_complex2d( lun, data, desc )
    integer, intent(in)                           :: lun
    complex, intent(in), dimension(:,:)           :: data
    character(len=*), intent(in)                  :: desc

    character(len=40)                             :: desc_
    integer, dimension(10)                        :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_complex, 0, 2, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_complex2d


subroutine test_store_data_complex3d( lun, data, desc )
    integer, intent(in)                             :: lun
    complex, intent(in), dimension(:,:,:)           :: data
    character(len=*), intent(in)                    :: desc

    character(len=40)                               :: desc_
    integer, dimension(10)                          :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_complex, 0, 3, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_complex3d


! test_retrieve_data_complex* -
!     Retrieve complex data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_retrieve_data_complex( lun, data, desc )
    integer, intent(in)                        :: lun
    complex, intent(out)                       :: data
    character(len=*), intent(out)              :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                    :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving complex data - please check'
        stop
    endif

    if ( type /= ftnunit_complex ) then
        write(*,*) 'FTNUNIT: expected complex data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 0 ) then
        write(*,*) 'FTNUNIT: expected scalar complex data - got array instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_complex


subroutine test_retrieve_data_complex1d( lun, data, desc )
    integer, intent(in)                         :: lun
    complex, pointer, dimension(:)              :: data
    character(len=*)                            :: desc

    character(len=10)                           :: type
    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions
    integer                                     :: ierr
    integer                                     :: char_length
    integer                                     :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving complex data - please check'
        stop
    endif

    if ( type /= ftnunit_complex ) then
        write(*,*) 'FTNUNIT: expected complex data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 1 ) then
        write(*,*) 'FTNUNIT: expected one-dimensional complex array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_complex1d


subroutine test_retrieve_data_complex2d( lun, data, desc )
    integer, intent(in)                        :: lun
    complex, pointer, dimension(:,:)           :: data
    character(len=*)                           :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                    :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving complex data - please check'
        stop
    endif

    if ( type /= ftnunit_complex ) then
        write(*,*) 'FTNUNIT: expected complex data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 2 ) then
        write(*,*) 'FTNUNIT: expected two-dimensional complex array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_complex2d


subroutine test_retrieve_data_complex3d( lun, data, desc )
    integer, intent(in)                          :: lun
    complex, pointer, dimension(:,:,:)           :: data
    character(len=*)                             :: desc

    character(len=10)                            :: type
    character(len=40)                            :: desc_
    integer, dimension(10)                       :: dimensions
    integer                                      :: ierr
    integer                                      :: char_length
    integer                                      :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving complex data - please check'
        stop
    endif

    if ( type /= ftnunit_complex ) then
        write(*,*) 'FTNUNIT: expected complex data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 3 ) then
        write(*,*) 'FTNUNIT: expected three-dimensional complex array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2), dimensions(3)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_complex3d


! test_store_data_character* -
!     Store complex data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_store_data_character( lun, data, desc )
    integer, intent(in)            :: lun
    character(len=*), intent(in)   :: data
    character(len=*), intent(in)   :: desc

    character(len=40)              :: desc_
    integer, dimension(10)         :: dimensions

    desc_ = desc
    dimensions = 0

    write( lun ) ftnunit_character, len(data), 0, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_character


subroutine test_store_data_character1d( lun, data, desc )
    integer, intent(in)                         :: lun
    character(len=*), intent(in), dimension(:)  :: data
    character(len=*), intent(in)                :: desc

    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:1) = shape(data)

    write( lun ) ftnunit_character, len(data(1)), 1, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_character1d


subroutine test_store_data_character2d( lun, data, desc )
    integer, intent(in)                           :: lun
    character(len=*), intent(in), dimension(:,:)  :: data
    character(len=*), intent(in)                  :: desc

    character(len=40)                             :: desc_
    integer, dimension(10)                        :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:2) = shape(data)

    write( lun ) ftnunit_character, len(data(1,1)), 2, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_character2d


subroutine test_store_data_character3d( lun, data, desc )
    integer, intent(in)                             :: lun
    character(len=*), intent(in), dimension(:,:,:)  :: data
    character(len=*), intent(in)                    :: desc

    character(len=40)                               :: desc_
    integer, dimension(10)                          :: dimensions

    desc_ = desc
    dimensions = 0
    dimensions(1:3) = shape(data)

    write( lun ) ftnunit_character, len(data(1,1,1)), 3, dimensions, desc_
    write( lun ) data

end subroutine test_store_data_character3d


! test_retrieve_data_character* -
!     Retrieve complex data
!
! Arguments:
!     lun            LU-number of the opened file
!     data           Data to store
!     desc           Short description (at most 40 long)
!
!
subroutine test_retrieve_data_character( lun, data, desc )
    integer, intent(in)                        :: lun
    character(len=*), intent(out)              :: data
    character(len=*), intent(out)              :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                     :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving character data - please check'
        stop
    endif

    if ( type /= ftnunit_character ) then
        write(*,*) 'FTNUNIT: expected character data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( char_length /= len(data) ) then
        write(*,*) 'FTNUNIT: expected character string of length ', len(data)
        write(*,*) '    Actual length: ', char_length
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 0 ) then
        write(*,*) 'FTNUNIT: expected scalar character data - got array instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_character


subroutine test_retrieve_data_character1d( lun, data, desc )
    integer, intent(in)                         :: lun
    character(len=*), pointer, dimension(:)     :: data
    character(len=*)                            :: desc

    character(len=10)                           :: type
    character(len=40)                           :: desc_
    integer, dimension(10)                      :: dimensions
    integer                                     :: ierr
    integer                                     :: char_length
    integer                                     :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving character data - please check'
        stop
    endif

    if ( type /= ftnunit_character ) then
        write(*,*) 'FTNUNIT: expected character data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 1 ) then
        write(*,*) 'FTNUNIT: expected one-dimensional character array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( char_length /= len(data(1)) ) then
        write(*,*) 'FTNUNIT: expected character string of length ', len(data(1))
        write(*,*) '    Actual length: ', char_length
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_character1d


subroutine test_retrieve_data_character2d( lun, data, desc )
    integer, intent(in)                        :: lun
    character(len=*), pointer, dimension(:,:)  :: data
    character(len=*)                           :: desc

    character(len=10)                          :: type
    character(len=40)                          :: desc_
    integer, dimension(10)                     :: dimensions
    integer                                    :: ierr
    integer                                    :: char_length
    integer                                    :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving character data - please check'
        stop
    endif

    if ( type /= ftnunit_character ) then
        write(*,*) 'FTNUNIT: expected complex data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 2 ) then
        write(*,*) 'FTNUNIT: expected two-dimensional character array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( char_length /= len(data(1,1)) ) then
        write(*,*) 'FTNUNIT: expected character string of length ', len(data(1,1))
        write(*,*) '    Actual length: ', char_length
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_character2d


subroutine test_retrieve_data_character3d( lun, data, desc )
    integer, intent(in)                          :: lun
    character(len=*), pointer, dimension(:,:,:)  :: data
    character(len=*)                             :: desc

    character(len=10)                            :: type
    character(len=40)                            :: desc_
    integer, dimension(10)                       :: dimensions
    integer                                      :: ierr
    integer                                      :: char_length
    integer                                      :: number_dims

    read( lun, iostat = ierr ) type, char_length, number_dims, dimensions, desc_

    if ( ierr /= 0 ) then
        write(*,*) 'FTNUNIT: read error retrieving complex data - please check'
        stop
    endif

    if ( type /= ftnunit_character ) then
        write(*,*) 'FTNUNIT: expected character data - got ' // trim(type) // ' instead - please check'
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( number_dims /= 3 ) then
        write(*,*) 'FTNUNIT: expected three-dimensional character array - please check'
        write(*,*) '    Actual dimension: ', number_dims
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    if ( char_length /= len(data(1,1,1)) ) then
        write(*,*) 'FTNUNIT: expected character string of length ', len(data(1,1,1))
        write(*,*) '    Actual length: ', char_length
        write(*,*) '    Description of the record: ' // trim(desc_)
        stop
    endif

    allocate( data(dimensions(1), dimensions(2), dimensions(3)) )

    read( lun ) data
    desc = desc_

end subroutine test_retrieve_data_character3d

end module ftnunit_store
