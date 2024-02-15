!!!  Copyright (C)  Stichting Deltares, 2022-2024.
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

!
!

! maptonetcdf.f90 --
!      Program to convert a traditional binary map file into a NetCDF map file

program maptonetcdf
    use m_outmnc

    implicit none

    character(len=20)                             :: string
    character(len=40), dimension(4)               :: title
    character(len=20), dimension(:), allocatable  :: syname
    character(len=100), dimension(:), allocatable :: systd
    character(len=40), dimension(:), allocatable  :: syunit
    character(len=60), dimension(:), allocatable  :: sydesc
    real, dimension(:,:), allocatable             :: conc
    real, dimension(:,:), allocatable             :: extreme
    real, dimension(:), allocatable               :: volume
    integer, dimension(:,:), allocatable          :: wqid1, wqid2
    integer, dimension(:), allocatable            :: iknmrk

    character(len=255)                            :: mapfile
    character(len=255)                            :: mncfile
    character(len=255)                            :: ugridfile

    integer                                       :: ncidmap, timeid, bndtimeid, mncrec, lunut
    integer                                       :: i, k, ierr, nosys, noseg, time, nolayers

    lunut = 20
    open( 20, file = 'maptonetcdf.out' )

    if ( command_argument_count() == 3 ) then
        call get_command_argument( 1, mapfile )
        call get_command_argument( 2, ugridfile )
        call get_command_argument( 3, string )
        read( string, * ) nolayers
    else
        open( 10, file = 'maptonetcdf.inp', status = 'old', iostat = ierr )
        if ( ierr /= 0 ) then
            write(*,'(a)' ) 'No arguments or too few arguments given and file "maptonetcdf.inp" does not exist'
            call print_usage
            stop
        end if

        write(*,'(a)') 'No arguments or too few arguments given, using file "maptonetcdf.inp"'
                         read( 10, '(a)', iostat = ierr ) mapfile
        if ( ierr == 0 ) read( 10, '(a)', iostat = ierr ) ugridfile
        if ( ierr == 0 ) read( 10, *, iostat = ierr )     nolayers
        close( 10 )

        if ( ierr /= 0 ) then
            write(*,'(a)' ) 'Error reading "maptonetcdf.inp"'
            call print_usage
            stop
        endif
    endif

    !
    ! Construct the out file name
    !
    k = index( mapfile, '.', back = .true. )
    if ( k <= 0 ) then
        k = len_trim(mapfile) + 1
    endif
    mncfile = mapfile(1:k-1) // '_map.nc'

    write( lunut, * ) 'Output file: ', trim(mncfile)

    !
    ! Open the map file and read the header
    !
    open( 11, file = mapfile, access = 'stream', status = 'old', iostat = ierr )

    if ( ierr /= 0 ) then
        write(*,'(a)') 'Map file "' // trim(mapfile) // '"  could not be opened!'
        stop
    endif

    read( 11 ) title
    read( 11 ) nosys, noseg

    !
    ! Check the number of layers - simple check
    !
    if ( mod(noseg,nolayers) /= 0 ) then
        write(*,'(a)') 'Error: mistake in the number of layers!'
        write(*,'(a)') '       Number given: ', nolayers
        write(*,'(a)') '       Should divide the number of segments in the file, being: ', noseg
        write(*,'(a)') 'Please correct this'
        stop
    endif

    allocate( syname(nosys), conc(nosys,noseg) )
    allocate( volume(noseg), iknmrk(noseg) )
    allocate( syunit(nosys), sydesc(nosys), systd(nosys) )
    allocate( wqid1(nosys,3), wqid2(nosys,3) )
    allocate( extreme(nosys,2) )

    read( 11 ) syname

    call cleanup_names( syname )

    syunit = ''  ! Dummy arrays ...
    systd  = ''
    sydesc = syname
    iknmrk = 1
    volume = 1.0

    !
    ! We are ready to start the conversion
    !
    ncidmap = -1
    mncrec  =  0

    extreme(:,1) =  huge(1.0)
    extreme(:,2) = -huge(1.0)

    do
        read( 11, iostat = ierr ) time, conc

        if ( ierr /= 0 ) then
            exit
        endif

        write(*,*) time

        mncrec = mncrec + 1

        call outmnc ( ncidmap, mncfile, ugridfile, timeid, bndtimeid, mncrec, time, title,  &
                      noseg, 0, conc, syname, systd, syunit, sydesc, wqid1, nosys, &
                      conc, syname, systd, syunit, sydesc, wqid2, volume, iknmrk, lunut)

        extreme(:,1) = min( extreme(:,1), minval( conc, dim = 1 ) )
        extreme(:,2) = max( extreme(:,2), maxval( conc, dim = 1 ) )
    enddo

    write( lunut, '(a20,2a12)' ) 'Parameter', 'Minimum', 'Maximum'
    write( lunut, '(a,2g12.4)' ) (syname(i), extreme(i,1), extreme(i,2) ,i=1,nosys)

    write(*,'(a)') 'Done'
    write(*,'(a)') 'Output available in ', trim(mncfile)

contains
subroutine cleanup_names( name )
    character(len=*), dimension(:) :: name

    integer :: i, k

    do i = 1,size(name)
        do
            k = index( name(i), '/' )
            if ( k > 0 ) then
                name(i)(k:k) = '_'
            else
                exit
            endif
        enddo
    enddo
end subroutine cleanup_names

subroutine print_usage
    write(*,'(a)') ''
    write(*,'(a)') 'Usage: maptonetcdf mapfile ugridfile number-layers'
    write(*,'(a)') 'Alternatively:'
    write(*,'(a)') 'Provide an input file "maptonetcdf.inp" containing, in this order:'
    write(*,'(a)') '- Name of the map file to be converted'
    write(*,'(a)') '- Name of the UGRID file (waqgeom) that contains the grid information'
    write(*,'(a)') '- Number of layers (this information is not contained in the UGRID file)'
    write(*,'(a)') '  and is also used to check the UGRID file'
end subroutine print_usage
end program
