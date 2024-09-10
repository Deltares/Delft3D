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

module m_waq_itdate

    character (len = 8) :: refdat
    integer :: itdate      ! should be user specified for (asc routines)

    INTEGER :: jul0, imonth0, iday0, iyear0

end module m_waq_itdate

module m_waq_timespace_read
    use m_waq_precision
    use m_stop_exit
    use m_meteo1temphelpers
    implicit none

    integer, parameter :: maxnamelen = 256
    double precision, parameter :: dmiss_default = -999.0_sp       ! Default missing value in meteo arrays
    double precision, parameter :: xymiss = -999.0_sp       ! Default missing value in elementset
    character(300), target :: errormessage = ' '             ! When an error occurs, a message is set in message.
    ! function getmeteoerror returns the message

    double precision :: pi                      ! pi
    double precision :: d2r                     ! degrees to radials
    double precision :: r2d                     ! degrees to radials
    double precision, private, parameter :: earthrad = 6378137.0_sp ! Mathworld, IUGG

contains

    function readwindseriesfiles(minp) result(success)
        implicit none
        logical :: success
        integer :: minp

        success = .false.

    end function readwindseriesfiles


    ! read arcinfo time and field
    function reaarctim(minp, d, mx, num_cells_u_dir, tread, dmiss, start_upleft) result(success)

        integer :: minp
        integer :: mx
        integer :: num_cells_u_dir
        integer :: l
        double precision :: tread
        double precision :: dmiss
        double precision, dimension(:, :) :: d
        character(132) :: rec
        character(4) :: keywrd
        logical :: success, start_upleft
        !
        if (size(d, 1) /= mx .or. size(d, 2) /= num_cells_u_dir) then
            errormessage = 'REAARCTIM: wrong sizes'
            success = .false.
            return
        endif
        keywrd = 'TIME'
        do
            read (minp, '(a)', end = 999) rec
            if (index(rec, keywrd)/=0) exit
            cycle
            999 continue
            errormessage = 'Keyword TIME not found in arcinfo file'
            success = .false.
            return
        enddo
        l = index(rec, ')')
        if (l == 0) then
            l = index(rec, '=')
        endif
        read(rec(l + 1:), *) tread
        if (index(rec, 'HRS') > 0 .or. index(rec, 'hrs') > 0 .or. index(rec, 'hours') > 0) then
            tread = tread * 60d0
        endif
        success = readarcinfoblock(minp, d, mx, num_cells_u_dir, dmiss, start_upleft)
    end function reaarctim


    ! Routine ad hoc to read a grid file - assumes a fixed structure
    subroutine readgrid(gridfile, num_columns, num_rows, x, y)
        character(len = *) :: gridfile
        integer, intent(out) :: num_columns, num_rows
        double precision, dimension(:), allocatable :: x, y

        integer :: lugrd, ierror, i, j
        character(len = 10) :: header
        character(len = 1) :: dummy
        double precision, dimension(:, :), allocatable :: xc, yc

        open(newunit = lugrd, file = gridfile, status = 'old')

        do
            read(lugrd, '(a)', iostat = ierror) header
            if (header == 'Coordinate') then
                exit
            endif
            if (ierror /= 0) then
                goto 101
            endif
        enddo

        read(lugrd, *) num_columns, num_rows
        read(lugrd, *) header

        !!  allocate( x((num_columns-1)*(num_rows-1)), y((num_columns-1)*(num_rows-1)) )
        allocate(x(2), y(2)) ! Terrible hack: assume a rectilinear grid!
        allocate(xc(num_columns, num_rows), yc(num_columns, num_rows))

        do j = 1, num_rows
            read(lugrd, *) dummy, dummy, xc(:, j)
        enddo

        do j = 1, num_rows
            read(lugrd, *) dummy, dummy, yc(:, j)
        enddo

        x(1) = xc(1, 1)
        y(1) = yc(1, 1)
        x(2) = xc(2, 1) - xc(1, 1)
        y(2) = yc(1, 2) - yc(1, 1)

        deallocate(xc, yc)

        return

        101 continue
        write(*, *) 'ERROR: Premature end of grid file ', trim(gridfile)
        call stop_exit(1)

    end subroutine readgrid

    function readarcinfoblock(minp, d, mx, num_cells_u_dir, dmiss, start_upleft) result(success)

        ! read arcinfoveld

        implicit none
        !
        integer :: minp
        integer :: mx
        integer :: num_cells_u_dir
        integer :: j
        integer :: i
        double precision :: dmiss
        double precision, dimension(:, :) :: d
        logical :: success, start_upleft
        character(16) :: tex
        character(4000) :: rec
        !
        if (size(d, 1) /= mx .or. size(d, 2) /= num_cells_u_dir) then
            errormessage = 'READARCINFOBLOCK: wrong sizes'
            success = .false.
            return
        endif
        if (start_upleft) then
            do j = num_cells_u_dir, 1, -1
                read(minp, '(a)', end = 100) rec
                read(rec, *, err = 101) (d(i, j), i = 1, mx)
            enddo
        else
            do j = 1, num_cells_u_dir, 1
                read(minp, '(a)', end = 100) rec
                read(rec, *, err = 101) (d(i, j), i = 1, mx)
            enddo
        endif
        do i = 1, mx
            do j = 1, num_cells_u_dir
                if (d(i, j) == dmiss) d(i, j) = dmiss_default
            enddo
        enddo
        success = .true.
        return
        ! error handling
        100 continue
        errormessage = 'Unexpected end of file in arcinfo file'
        success = .false.
        return
        101 continue
        write(tex, '(2i8)') i, j
        write(errormessage, '(2a)') 'Error reading arc-info block in colnr, rownr :', trim(tex)
        success = .false.
        return
    end function readarcinfoblock


    function reacurtim(minp, d, mfirst, mlast, nfirst, nlast, mrow, tread, dmiss) result(success)
        !
        ! read arcinfo time and field
        !
        implicit none
        !
        integer, intent(in) :: minp
        integer, intent(in) :: mfirst
        integer, intent(in) :: mlast
        integer, intent(in) :: nfirst
        integer, intent(in) :: nlast
        double precision, intent(out) :: tread
        double precision, intent(in) :: dmiss
        double precision, dimension(:, :), intent(out) :: d
        logical, intent(in) :: mrow
        !
        character(132) :: rec
        character(4) :: keywrd
        integer :: l
        logical :: success
        !
        keywrd = 'TIME'
        do
            read (minp, '(a)', end = 999) rec
            if (index(rec, keywrd)/=0) exit
            cycle
            999 continue
            errormessage = 'Keyword TIME not found in arcinfo file'
            success = .false.
            return
        enddo
        l = index(rec, ')')
        if (l == 0) then
            l = index(rec, '=')
        endif
        read(rec(l:+1), *) tread
        if (index(rec, 'HRS') > 0 .or. index(rec, 'hrs') > 0 .or. index(rec, 'hours') > 0) then
            tread = tread * 60d0
        endif
        success = readcurviblock(minp, d, mfirst, mlast, nfirst, nlast, mrow, dmiss)
    end function reacurtim


    function readcurviblock(minp, d, mfirst, mlast, nfirst, nlast, mrow, dmiss) result(success)
        !
        ! read curvifield
        !
        implicit none
        !
        integer, intent(in) :: minp
        integer, intent(in) :: mfirst
        integer, intent(in) :: mlast
        integer, intent(in) :: nfirst
        integer, intent(in) :: nlast
        double precision, intent(in) :: dmiss
        double precision, dimension(:, :), intent(out) :: d
        logical, intent(in) :: mrow
        !
        integer :: j
        integer :: i
        integer :: mincr
        integer :: nincr
        logical :: success
        character(16) :: tex
        character(4000) :: rec
        !
        mincr = sign(1, mlast - mfirst)
        nincr = sign(1, nlast - nfirst)
        if (size(d, 1) /= max(mfirst, mlast) &
                .or. size(d, 2) /= max(nfirst, nlast)) then
            errormessage = 'READCURVIINFOBLOCK: wrong sizes'
            success = .false.
            return
        endif
        if (mrow) then
            do i = mfirst, mlast, mincr
                read(minp, '(a)', end = 100) rec
                read(rec, *, err = 101) (d(i, j), j = nfirst, nlast, nincr)
            enddo
        else
            do j = nfirst, nlast, nincr
                read(minp, '(a)', end = 100) rec
                read(rec, *, err = 101) (d(i, j), i = mfirst, mlast, mincr)
            enddo
        endif
        do i = mfirst, mlast, mincr
            do j = nfirst, nlast, nincr
                if (d(i, j) == dmiss) d(i, j) = dmiss_default
            enddo
        enddo
        success = .true.
        return
        ! error handling
        100 continue
        errormessage = 'Unexpected end of file in curvilinear wind file'
        success = .false.
        return
        101 continue
        write(tex, '(2i8)') i, j
        write(errormessage, '(2a)') 'Error reading curvilinear wind block in colnr, rownr :', trim(tex)
        success = .false.
        return
    end function readcurviblock


    function readseries(minp, d, kx, tread) result(success)
        !
        !  Read uniform time serie
        !  number of columns is number of dimensions
        !
        implicit none
        !
        integer :: minp
        integer :: kx
        integer :: k
        double precision :: tread
        double precision, dimension(:) :: d
        logical :: success
        character(132) :: rec
        !
        if (size(d, 1) < kx) then
            errormessage = 'READSERIES: wrong sizes'
            success = .false.
            return
        endif
        10 read (minp, '(a)', end = 100) rec
        if (rec(1:1) == '*') goto 10
        read(rec, *, err = 101) tread, (d(k), k = 1, kx)
        success = .true.
        return
        100 continue
        errormessage = 'Unexpected end of file in uniform time serie file'
        success = .false.
        return
        101 continue
        write(errormessage, '(2a)') 'Error reading timeseries : ', trim(rec)
        success = .false.
        return
    end function readseries


    function reaspv(minp, d, mx, num_cells_u_dir, kx, tread) result(success)
        !
        ! Read Space Varying Wind
        !
        implicit none
        !
        integer :: minp
        integer :: mx
        integer :: num_cells_u_dir
        integer :: kx
        integer :: i
        integer :: j
        double precision :: tread
        double precision, dimension(:, :, :) :: d
        logical :: success
        character(132) :: rec
        !
        if (size(d, 1) /= mx .or. size(d, 2) /= num_cells_u_dir .or. size(d, 3) /= kx) then
            errormessage = 'REASPV: wrong sizes'
            success = .false.
            return
        endif
        read (minp, '(a)', end = 100) rec
        read(rec, *, err = 101) tread
        !
        ! Loop over the first dimension in flow
        !
        do j = 1, mx
            read(minp, *, end = 100, err = 102) (d(j, i, 1), i = 1, num_cells_u_dir)
        enddo
        do j = 1, mx
            read(minp, *, end = 100, err = 103) (d(j, i, 2), i = 1, num_cells_u_dir)
        enddo
        do j = 1, mx
            read(minp, *, end = 100, err = 104) (d(j, i, 3), i = 1, num_cells_u_dir)
        enddo
        success = .true.
        return
        100 continue
        errormessage = 'Unexpected end of file in space varying wind file'
        success = .false.
        return
        101 continue
        write(errormessage, '(2a)') 'Error reading time in space varying wind file : ', trim(rec)
        success = .false.
        return
        102 continue
        errormessage = 'Error reading wind u-field'
        success = .false.
        return
        103 continue
        errormessage = 'Error reading wind v-field'
        success = .false.
        return
        104 continue
        errormessage = 'Error reading atmospheric pressure field'
        success = .false.
        return
    end function reaspv


    function reaspwtim(minp, d, mx, num_cells_u_dir, tread, x0r, y0r) result(success)
        !
        ! Read spiderweb time and field
        !
        implicit none
        !
        integer :: i
        integer :: j
        integer :: minp
        integer :: mx
        integer :: num_cells_u_dir
        double precision :: pdrop
        double precision :: tread
        double precision :: x0r
        double precision :: y0r
        double precision, dimension(:, :, :) :: d
        character(132) :: rec
        logical :: success
        !
        if (size(d, 1) /= mx .or. size(d, 2) /= num_cells_u_dir) then
            errormessage = 'REASPWTIM: wrong sizes'
            success = .false.
            return
        endif
        read (minp, '(a)', end = 100) rec
        read (rec, *, err = 101)      tread
        read (minp, '(a)', end = 100) rec
        read (rec, *, err = 102)      x0r, y0r, pdrop
        do j = 2, num_cells_u_dir
            read(minp, *, end = 100, err = 201) (d(i, j, 1), i = 1, mx - 1)
        enddo
        do j = 2, num_cells_u_dir
            read(minp, *, end = 100, err = 202) (d(i, j, 2), i = 1, mx - 1)
        enddo
        do j = 2, num_cells_u_dir
            read(minp, *, end = 100, err = 203) (d(i, j, 3), i = 1, mx - 1)
        enddo
        do i = 1, mx - 1
            d(i, 1, 1) = 0
            d(i, 1, 2) = 0
            !
            ! Fill central point
            !
            d(i, 1, 3) = pdrop
        enddo
        do j = 1, num_cells_u_dir
            !
            ! Fill 360 degrees
            !
            d(mx, j, 1) = d(1, j, 1)
            d(mx, j, 2) = d(1, j, 2)
            d(mx, j, 3) = d(1, j, 3)
        enddo
        success = .true.
        return
        100 continue
        errormessage = 'Unexpected end of file in spiderweb wind file'
        success = .false.
        return
        101 continue
        write(errormessage, '(2a)') 'Error reading time in spiderweb wind file : ', trim(rec)
        success = .false.
        return
        102 continue
        write(errormessage, '(2a)') 'Error reading x0, y0, pdrop of cyclone eye : ', trim(rec)
        success = .false.
        return
        201 continue
        errormessage = 'Error reading cyclone wind-u field'
        success = .false.
        return
        202 continue
        errormessage = 'Error reading cyclone wind-v field'
        success = .false.
        return
        203 continue
        errormessage = 'Error reading cyclone pressure drop field'
        success = .false.
        return
    end function reaspwtim

    function readarcinfoheader(minp, num_columns, num_rows, x0, y0, &
            & dxa, dya, dmiss) result(success)

        use m_waq_precision
        !
        implicit none
        !
        ! Global variables
        !
        integer :: minp
        integer, intent(out) :: num_columns
        integer, intent(out) :: num_rows
        double precision, intent(out) :: dxa
        double precision, intent(out) :: dya
        double precision, intent(out) :: dmiss
        double precision, intent(out) :: x0
        double precision, intent(out) :: y0
        logical :: success
        !
        ! Local variables
        !
        integer :: jacornerx
        integer :: jacornery
        integer :: k
        integer :: l
        !    integer        :: numbersonline
        character(132) :: rec
        character(30) :: keyword
        character(1) :: dummy
        integer :: i
        integer :: linecount
        !
        !! executable statements -------------------------------------------------------
        !
        num_columns = -1
        num_rows = -1
        jacornerx = -1
        jacornery = -1
        dxa = -1.0
        dya = -1.0

        linecount = 0
        do
            read (minp, '(A)', end = 100) rec
            linecount = linecount + 1

            read(rec, *) keyword
            select case (keyword)
            case('n_cols')
                read (rec, *, err = 101) dummy, dummy, num_columns
            case('n_rows')
                read (rec, *, err = 101) dummy, dummy, num_rows
            case('x_llcenter')
                read (rec, *, err = 101) dummy, dummy, x0
                jacornerx = 0
            case('y_llcenter')
                read (rec, *, err = 101) dummy, dummy, y0
                jacornery = 0
            case('x_llcorner')
                read (rec, *, err = 101) dummy, dummy, x0
                jacornerx = 1
            case('y_llcorner')
                read (rec, *, err = 101) dummy, dummy, y0
                jacornery = 1
            case('dx')
                read (rec, *, err = 101) dummy, dummy, dxa
            case('dy')
                read (rec, *, err = 101) dummy, dummy, dya
            case('cellsize')
                k = numbersonline(rec)
                if (k == 2) then
                    read (rec, *, err = 101) dummy, dxa
                    dya = dxa
                    jacornery = jacornerx
                else
                    read (rec, *, err = 101) dummy, dxa, dya
                endif
            case('NODATA_value')
                read (rec, *, err = 101) dummy, dummy, dmiss
            case('missing')
                read (rec, *, err = 101) dummy, dmiss
            case('TIME')
                linecount = linecount - 1
                exit ! We found the start of a data block
            case default
                if (index(rec, "END OF HEADER") > 0) then
                    exit ! End of the header lines
                endif
                if (rec(1:1)=='*' .or. rec(2:2)=='*' .or. rec(1:1)=='#' .or. rec(2:2)=='#') then
                    cycle
                endif
                ! Ignore any other keyword
            end select
        enddo
        !
        ! Data in an arcinfo grid file is always defined in the cell centres.
        ! Data is assumed to be given at the points x0+i*dx,y0+j*dy
        ! If the x0/y0 line contains the word corner, the corner coordinates
        ! have been specified in the file. Therefore, shift x0 and y0 by half
        ! a grid cell to the cell centres.
        !
        if (jacornerx == 1) x0 = x0 + dxa / 2
        if (jacornery == 1) y0 = y0 + dya / 2
        !
        ! Reposition the file
        !
        rewind(minp)
        do i = 1, linecount
            read(minp, '(a)') dummy
        enddo

        !
        ! Check if all relevant data were present
        !
        if (num_columns < 0 .or. num_rows < 0 .or. jacornerx < 0 .or. jacornery < 0 .or. dxa < 0.0 .or. dya < 0.0) then
            goto 102
        endif

        success = .true.
        return
        !
        ! error handling
        !
        100 continue
        errormessage = 'Unexpected end of file while reading header of arcinfo wind file'
        goto 999
        101 continue
        write(errormessage, '(2a)') 'Reading data for keyword ' // trim(keyword) // ' failed - record:', trim(rec)
        goto 999
        102 continue
        write(errormessage, '(2a)') 'Header does not contain all relevant information'
        goto 999
        999 continue
        success = .false.
        return
    end function readarcinfoheader

    function readarcinfoheader_d3d(minp, num_columns, num_rows, x, y, &
            & kcs, dmiss) result(success)

        use m_waq_precision
        !
        implicit none
        !
        ! Global variables
        !
        integer :: minp
        integer, intent(out) :: num_columns
        integer, intent(out) :: num_rows
        double precision, intent(out) :: dmiss
        double precision, dimension(:), allocatable, intent(out) :: x
        double precision, dimension(:), allocatable, intent(out) :: y
        integer, dimension(:), allocatable, intent(out) :: kcs
        logical :: success
        !
        ! Local variables
        !
        integer :: jacornerx
        integer :: jacornery
        integer :: k
        !    integer        :: numbersonline
        character(132) :: rec
        character(30) :: keyword
        character(1) :: dummy
        integer :: i
        integer :: linecount

        integer :: n_quantity

        character(len = 4) :: fileversion
        character(len = 40) :: filetype
        character(len = 40) :: first_data_value
        character(len = 40) :: data_row
        character(len = 40) :: quantity1
        character(len = 40) :: unit1
        character(len = 200) :: gridfile
        !
        !! executable statements -------------------------------------------------------
        !
        num_columns = -1
        num_rows = -1

        linecount = 0
        do
            read (minp, '(A)', end = 100) rec
            linecount = linecount + 1

            read(rec, *) keyword
            select case (keyword)
            case('FileVersion')
                read (rec, *, err = 101) dummy, dummy, fileversion  ! Must be "1.03"
                if (fileversion /= '1.03') then
                    write(*, *) 'ERROR: FileVersion must be ''1.03'''
                    goto 101
                endif
            case('FileType')
                read (rec, *, err = 101) dummy, dummy, filetype     ! Ignored
            case('NODATA_value')
                read (rec, *, err = 101) dummy, dummy, dmiss
            case('grid_file')
                read (rec, *, err = 101) dummy, dummy, gridfile
                if (.not. allocated(x)) then
                    call readgrid(gridfile, num_columns, num_rows, x, y)
                    allocate(kcs(num_columns * num_rows))
                    kcs = 1
                endif
            case('first_data_value')
                read (rec, *, err = 101) dummy, dummy, first_data_value
                if (first_data_value /= 'grid_llcorner') then
                    write(*, *) 'ERROR: first_data_value must be ''grid_llcorner'''
                    goto 101
                endif
            case('data_row')
                read (rec, *, err = 101) dummy, dummy, data_row
                if (data_row /= 'grid_row') then
                    write(*, *) 'ERROR: data_row must be ''grid_row'''
                    goto 101
                endif
            case('n_quantity')
                read (rec, *, err = 101) dummy, dummy, n_quantity
                if (n_quantity /= 1) then
                    write(*, *) 'ERROR: only one quantity supported per meteo file'
                    goto 101
                endif
            case('quantity1')
                read (rec, *, err = 101) dummy, dummy, quantity1 ! Ignored
            case('unit1')
                read (rec, *, err = 101) dummy, dummy, unit1 ! Ignored
            case('TIME')
                linecount = linecount - 1
                exit ! We found the start of a data block
            case default
                if (index(rec, "END OF HEADER") > 0) then
                    exit ! End of the header lines
                endif
                if (rec(1:1)=='*' .or. rec(2:2)=='*' .or. rec(1:1)=='#' .or. rec(2:2)=='#') then
                    cycle
                endif
                ! Ignore any other keyword
            end select
        enddo
        !
        ! Reposition the file
        !
        rewind(minp)
        do i = 1, linecount
            read(minp, '(a)') dummy
        enddo

        !
        ! Check if all relevant data were present
        !
        if (num_columns < 0 .or. num_rows < 0) then
            goto 102
        endif

        success = .true.
        return
        !
        ! error handling
        !
        100 continue
        errormessage = 'Unexpected end of file while reading header of arcinfo wind file'
        goto 999
        101 continue
        write(errormessage, '(2a)') 'Reading data for keyword ' // trim(keyword) // ' failed - record:', trim(rec)
        goto 999
        102 continue
        write(errormessage, '(2a)') 'Header does not contain all relevant information'
        goto 999
        999 continue
        success = .false.
        return
    end function readarcinfoheader_d3d


    function reaspwheader(minp, mx, num_cells_u_dir, dxa, dya, &
            & mncoor) result(success)

        use m_waq_precision
        implicit none
        !
        ! Global variables
        !
        integer :: minp
        integer, intent(out) :: mncoor
        integer :: mx
        integer :: num_cells_u_dir
        double precision, intent(out) :: dxa
        double precision, intent(out) :: dya
        logical :: success
        !
        ! Local variables
        !
        integer :: k
        double precision :: radius, pi
        logical :: sferic
        character(132) :: rec
        !
        !
        !! executable statements -------------------------------------------------------
        !
        mncoor = 0
        sferic = .false.
        pi = acos(-1d0)
        do k = 1, 9
            read (minp, '(A)', end = 100) rec
            if (index(rec, 'MNCOOR')/=0) mncoor = 1
            if (index(rec, 'LON,LAT')/=0) sferic = .true.
            if (k==3) read (rec(2:), *, err = 101) num_cells_u_dir, mx
            if (k==4) read (rec(2:), *, err = 102) radius
        enddo


        !
        mx = mx + 1              ! 1 KOLOM EXTRA VOOR 360 GRADEN = 0 GRADEN IVM INTERPOLATIE
        dxa = 2 * pi / dble(mx - 1)   ! MX-1 INTERVALLEN IN HOEK
        num_cells_u_dir = num_cells_u_dir + 1              ! 1 RIJ EXTRA VOOR DE PUNTEN OP STRAAL = 0
        dya = radius / dble(num_cells_u_dir - 1) ! num_cells_u_dir-1 INTERVALLEN IN DE STRAAL
        success = .true.
        return
        !
        ! error handling
        !
        100 continue
        errormessage = 'Unexpected end of file while reading header of spiderweb wind file'
        goto 999
        101 continue
        write(errormessage, '(2a)') 'Looking for web dimensions (spiderweb wind file), but getting ', trim(rec)
        goto 999
        102 continue
        write(errormessage, '(2a)') 'Looking for web radius (m) (spiderweb wind file), but getting ', trim(rec)
        goto 999
        999 continue
        success = .false.
        return
    end function reaspwheader

    function readcurviheader(minp, gridfilnam, mfirst, mlast, nfirst, nlast, mrow, dmiss) result(success)

        use m_waq_precision
        implicit none
        !
        ! Global variables
        !
        integer :: minp
        integer, intent(out) :: mfirst
        integer, intent(out) :: mlast
        integer, intent(out) :: nfirst
        integer, intent(out) :: nlast
        double precision, intent(out) :: dmiss
        logical, intent(out) :: mrow
        logical :: success
        character(*), intent(out) :: gridfilnam
        !
        ! Local variables
        !
        integer :: l
        character(132) :: rec
        !
        !! executable statements -------------------------------------------------------
        !
        gridfilnam = ' '
        mfirst = 0
        mlast = 0
        nfirst = 0
        nlast = 0
        mrow = .false.
        10 continue
        read (minp, '(A)', end = 100) rec
        if (rec(1:1)=='*' .or. rec(2:2)=='*') then
            if (index(rec, 'gridfile')/=0) then
                l = index(rec, '=') + 1
                read(rec(l:), '(a)', err = 101) gridfilnam
            elseif (index(rec, 'firstrow')/=0) then
                l = index(rec, '=') + 1
                if (index(rec(l:), 'num_columns') > 0) then
                    mrow = .true.
                    mfirst = 2
                    mlast = 1
                elseif (index(rec(l:), 'num_rows') > 0) then
                    mrow = .false.
                    nfirst = 2
                    nlast = 1
                endif
            elseif (index(rec, 'lastrow')/=0) then
                l = index(rec, '=') + 1
                if (index(rec(l:), 'num_columns') > 0) then
                    mrow = .true.
                    mfirst = 1
                    mlast = 2
                elseif (index(rec(l:), 'num_rows') > 0) then
                    mrow = .false.
                    nfirst = 1
                    nlast = 2
                endif
            elseif (index(rec, 'firstcol')/=0) then
                l = index(rec, '=') + 1
                if (index(rec(l:), 'num_columns') > 0) then
                    mrow = .false.
                    mfirst = 2
                    mlast = 1
                elseif (index(rec(l:), 'num_rows') > 0) then
                    mrow = .true.
                    nfirst = 2
                    nlast = 1
                endif
            elseif (index(rec, 'lastcol')/=0) then
                l = index(rec, '=') + 1
                if (index(rec(l:), 'num_columns') > 0) then
                    mrow = .false.
                    mfirst = 1
                    mlast = 2
                elseif (index(rec(l:), 'num_rows') > 0) then
                    mrow = .true.
                    nfirst = 1
                    nlast = 2
                endif
            endif
            goto 10
        endif
        read (rec(13:), *, err = 106) dmiss
        success = .true.
        return
        !
        ! error handling
        !
        100 continue
        errormessage = 'Unexpected end of file while reading header of curvilinear wind file'
        goto 999
        101 continue
        write(errormessage, '(2a)') 'Looking for grid file name (curvilinear wind file), but getting ', trim(rec)
        goto 999
        106 continue
        write(errormessage, '(2a)') 'Looking for missing value (curvilinear wind file), but getting ', trim(rec)
        goto 999
        999 continue
        success = .false.
        return
    end function readcurviheader


    function numbersonline(rec)
        implicit none
        !
        ! Global variables
        !
        integer :: numbersonline
        character(*) :: rec
        !
        !
        ! Local variables
        !
        integer :: i
        integer :: istarti
        integer :: leeg
        integer :: lend
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        numbersonline = 0
        leeg = 1
        lend = len_trim(rec)
        do i = 1, lend
            if (index(rec(i:i), ' ')==0) then
                !           hier staat iets
                if (leeg==1) then
                    leeg = 0
                    istarti = i
                    numbersonline = numbersonline + 1
                endif
            else
                leeg = 1
            endif
        enddo
    end function numbersonline


end module m_waq_timespace_read

module m_waq_timespace_data
    use m_waq_precision
    use m_waq_timespace_read
    implicit none

    ! Deze module doet ruimte/tijdinterpolatie
    ! Voor een gegeven quantity met ruimtedefinitie in een elementset,
    ! worden de bijdragen van alle dataproviders aan die quantity gesommeerd.
    ! Hierbij heeft iedere dataprovider een eigen tijd/ruimtedefinitie.
    ! Zitten meerdere quantities of dezelfde tijd/ruimtedefinitie dan hoeft de tijd/ruimteinterpolatie
    ! maar 1 keer uitgevoerd te worden.
    ! De gevraagde grootheid moet dan niet als scalair maar als vector aangeboden worden.


    ! enumeration for filetypes van de providers
    integer, parameter :: uniform = 1  ! kx values per tijdstap 1 dim arr       uni
    integer, parameter :: unimagdir = 2  ! kx values per tijdstap 1 dim arr, mag/dir transf op index 1,2 u,v
    integer, parameter :: svwp = 3  ! 3 velden per tijdstap 3 dim array      noint
    integer, parameter :: arcinfo = 4  ! 1 veld per tijdstap 2 dim array        bilin/direct
    integer, parameter :: spiderweb = 5  ! 3 veld per tijdstap 3 dim array        bilin/spw
    integer, parameter :: curvi = 6  ! 1 veld per tijdstap 2 dim array        bilin/findnm
    integer, parameter :: triangulation = 7  ! 1 veld per tijdstap                    triang
    integer, parameter :: triangulationmagdir = 8  ! 2 velden u,v per tijdstap 3 dim array  triang, vectormax = 2
    ! op basis van windreeksen op stations mag/dir
    integer, parameter :: poly_tim = 9  ! for line oriented bnd conditions, refs to uniform, fourier or harmonic
    integer, parameter :: fourier = 10  ! period(hrs), ampl(m), phas(deg)

    integer, parameter :: multiple_uni = 11  ! multiple time series, no spatial relation
    integer, parameter :: d3d_flow_arcinfo = 12  ! "same" file as Delft3D-FLOW

    integer, parameter :: max_file_types = 12  !  max nr of supported types



    ! het filetype legt vast  :  a) format file
    !                            b) vectormax van grootheid / heden in file
    !                            c) elementset waarop grootheid is gedefinieerd
    !                            d) is daarmee bepalend voor de toepasbare interpolatiemethodes
    !

    integer :: mdia = 0 !  -1  ! -1 = write dia, 0 = do not write dia


    ! enumeration for interpolation methods of providers

    integer, parameter :: justupdate = 0  ! provider just updates, another provider that
    ! pointers to this one does the actual interpolation
    integer, parameter :: spaceandtime = 1  ! intp space and time (getval)
    ! keep  2 meteofields in memory
    integer, parameter :: spacefirst = 2  ! first intp space (update), next intp. time (getval)
    ! keep 2 flowfields in memory
    integer, parameter :: weightfactors = 3  ! save weightfactors, intp space and time (getval)
    ! keep 2 pointer- and weight sets in memory

    double precision :: timelast = -1d10  ! time of most recent value requested
    ! if time =< timelast, no updates

    double precision :: t01ini = -1d10  ! initial time for dataproviders t0 and t1 fields

    ! AvD: NOTE
    ! De pointers in alle onderstaande types worden puur gebruikt om dynamisch
    ! te kunnen alloceren. In Fortran 95 mag je namelijk geen allocatables in
    ! user-defined types opnemen. In Fortran 2003 mag dit wel, dus waarom
    ! binnenkort niet overstappen?
    ! Naar allocatables mag je ook pointeren (xyen => provider%xyen), en verder
    ! gebruiken we uberhaupt geen pointer(omleg-)functionaliteit. Performance
    ! schijnt ook slechter te zijn van pointers.
    ! allocables hoef je ook niet te nullifyen om de allocated check te laten
    ! slagen. Dit geld wel voor de associated check van pointers.

    ! type definitions
    type telementset                            ! algemene roosterdefinitie, zowel voor vraag als aanbod
        double precision, allocatable :: x(:)    !  => null() ! als size = 2, dan x0 en dx in x(1) en x(2)
        double precision, allocatable :: y(:)    !  => null() ! als size = 2, dan y0 en dy in y(1) en y(2)
        integer, allocatable :: kcs(:)    !=> null() ! value = 0 : invalid point   value /= 0 : valid point
        integer :: mnx          ! size of ! bedoeld only for debug now, may be removed later
        logical :: sferic       ! true  = sferical coordinates
        double precision, allocatable :: xyen(:, :) !=> null() ! optional, 'end'point outside of cells, defining search range for providers.
    end type telementset

    type tfield                                 ! definitie plaats- tijd- en veldinformatie aanbodkant
        double precision :: time         ! tijd
        integer :: ielset       ! pointer naar elementset bijhorend bij datagrid (plaats)
        double precision, pointer :: arr1d(:) => null() ! 1-dim array veld
        double precision, pointer :: arr2d(:, :) => null() ! 2-dim array veld
        double precision, pointer :: arr3d(:, :, :) => null() ! 3-dim array veld
    end type tfield

    type tdataprovider                          ! definitie aanbodkant
        character(maxnamelen) :: qid          ! id of this quantity
        character(maxnamelen) :: filename     ! file containing data
        integer :: filetype     ! type of file
        integer :: minp         ! handle to file of omi handle
        integer :: it0          !
        integer :: it1          ! index oude of nieuwe velden
        integer :: mx           ! size of meteo fields  m
        integer :: num_cells_u_dir           !                       n
        integer :: kx           !                       k
        double precision :: dmiss        ! missing value

        type(tfield), pointer :: field(:) => null()   ! oude en nieuwe velden, dimensie = 2

        integer :: method       ! initially, store the desired spacetime method
        ! later, these methods are collected and put in relations
        character(len = 1) :: operand      ! operand
        integer :: ielsetq      ! pointer to elementset on which this provider should give a quantity
        ! only relevant for method >= 2
        double precision, pointer :: wfn(:, :) => null() ! weightfactors, bv (3,mnx) voor triang, (2,mnx) voor poly
        integer, pointer :: indxn(:, :) => null() ! indices      , bv (3,nmx) voor triang, (2,mnx) voor poly
        integer :: refresh = 1 !< Whether or not weightfactors need to be updated.
    end type tdataprovider

    type tquantity                              ! definitie vraagkant
        character(maxnamelen) :: qid          ! name that you give to a dataset/quantity at initialisation
        integer :: ielset       ! pointer naar elementset van het bijhorend modelgrid
        integer :: kx           ! vectormax. quantity kan meerdere grootheden bevatten
        ! die allen op hetzelfde tijd/ruimteframe zitten
        integer, pointer :: providernrs(:) => null() ! welke aanbodnrs zijn allemaal additief voor deze grootheid
    end type tquantity

    type tsubdom                                ! en dit is alleen nodig voor de threadsaveheid
        type(telementset), pointer :: elementsets(:) => null() ! bevat roosters zowel van vraagkant als aanbodkant
        type(tdataprovider), pointer :: dataproviders(:) => null() ! bevat het aanbod
        type(tquantity), pointer :: quantities(:) => null() ! bevat de vraag
        integer :: ini = 0
    end type tsubdom

    !  type(tsubdom), pointer, save   :: subdoms(:)       ! as many subdoms as there are threads
    type(tsubdom), allocatable, save :: subdoms(:)       ! as many subdoms as there are threads

    integer :: idom             ! current threadnr = domnr


    !  type tindexweight
    !  end type tindexweight

    !  type(tindexweight), pointer, save :: indexweight(:) !

contains


    function addelementset(elementsets, x, y, kcs, ielset, xyen) result(success)
        implicit none
        logical :: success

        ! arguments
        type(telementset), pointer :: elementsets(:)
        integer :: n       ! dimension of x,y,kcs
        integer :: ielset  ! pointer to elementset nr
        double precision :: x(:)
        double precision :: y(:)
        integer :: kcs(:)

        double precision, optional :: xyen(:, :)
        double precision :: foo

        ! locals
        integer :: k, kk, nn, i, ja

        success = .true.
        foo = timelast

        n = size(x)
        kk = size(elementsets)

        if (n > 2) then ! op stack kijken alleen als elset niet constant (dus dim > 2)
            ! constante elsets gaan altijd op de stack, kost weinig en is nodig voor spiderweb
            do k = 1, kk                          ! check if elementset already present
                nn = size(elementsets(k)%x)
                if (nn == n) then
                    ja = 1
                    do i = 1, nn
                        if (x(i)   /= elementsets(k)%x(i)) ja = 0
                        if (y(i)   /= elementsets(k)%y(i)) ja = 0
                        if (kcs(i) /= elementsets(k)%kcs(i)) ja = 0
                    enddo
                    if (ja == 1) then
                        ielset = k                  ! if equal, elementset already present, just pointer to it
                        return
                    endif
                endif
            enddo
        endif

        success = increaseelsetssize(kk, elementsets)  ! otherwise, allocate a new one

        k = kk + 1
        allocate (elementsets(k)%x(n))
        allocate (elementsets(k)%y(n))
        allocate (elementsets(k)%kcs(n))
        elementsets(k)%x = x                  ! and fill with the given arrays
        elementsets(k)%y = y
        elementsets(k)%kcs = kcs
        elementsets(k)%mnx = n
        elementsets(k)%sferic = .false.
        ielset = k
        if (present(xyen)) then
            allocate (elementsets(k)%xyen(2, n))
            elementsets(k)%xyen = xyen
            ! No check wether size(xyen,1) == 2...
        endif

        return
    end function addelementset

    function destroyelementsets(elementsets) result(success)
        implicit none
        logical :: success

        ! arguments
        type(telementset), pointer :: elementsets(:)

        ! locals
        integer :: k, kk

        success = .true.

        kk = size(elementsets)
        do k = 1, kk
            if (allocated(elementsets(k)%x)) deallocate (elementsets(k)%x)
            if (allocated(elementsets(k)%y)) deallocate (elementsets(k)%y)
            if (allocated(elementsets(k)%kcs)) deallocate (elementsets(k)%kcs)
            if (allocated(elementsets(k)%xyen)) deallocate (elementsets(k)%xyen)
        enddo

    end function destroyelementsets

    function increaseelsetssize(k, elementsets) result (success)
        implicit none
        logical :: success
        type(telementset), pointer :: elementsets(:)
        integer :: k

        ! locals
        type(telementset), pointer :: helementsets(:)

        success = .true.

        if (k >= 1) then

            allocate (helementsets(k))
            success = copyelementsets(k, elementsets, helementsets)
            deallocate (elementsets)
        endif

        allocate (elementsets(k + 1))

        if (k >= 1) then
            success = copyelementsets(k, helementsets, elementsets)
            success = destroyelementsets(helementsets)
        endif

    end function increaseelsetssize


    function copyelementsets(kk, elementsets, helementsets) result(success)
        implicit none
        logical :: success
        type(telementset), pointer :: elementsets(:), helementsets(:)
        integer :: k, kk, nn, nh, n

        success = .true.

        do k = 1, kk
            helementsets(k)%sferic = elementsets(k)%sferic
            helementsets(k)%mnx = elementsets(k)%mnx
            nn = size(elementsets(k)%x)
            !    nh = size( helementsets(k)%x)
            !    if ( nh == 0 ) then
            !    if ( .not. associated( helementsets(k)%x ) ) then
            allocate (helementsets(k)%x(nn))
            helementsets(k)%x = elementsets(k)%x
            allocate (helementsets(k)%y(nn))
            helementsets(k)%y = elementsets(k)%y
            allocate (helementsets(k)%kcs(nn))
            helementsets(k)%kcs = elementsets(k)%kcs

            if (allocated(elementsets(k)%xyen)) then
                ! Officially, we also need to check on allocated(%xyen or size(%xyen) > 0
                ! but %xyen is never a pointer to some non-matrix array.
                allocate (helementsets(k)%xyen(2, nn))
                helementsets(k)%xyen = elementsets(k)%xyen
            end if
            !    endif
        enddo
    end function copyelementsets


    function addquantity(idom, qid, kx, x, y, kcs, numq, ielsetq, xyen) result(success)
        implicit none
        logical :: success


        ! arguments
        integer :: idom          ! threadnr
        character(len = *) :: qid           ! name that you give to a quantity at initialisation
        integer :: kx            ! vectormax of this quantity
        double precision :: x(:)          ! als size = 2, dan x0 en dx in x(1) en x(2)
        double precision :: y(:)          ! als size = 2, dan y0 en dy in y(1) en y(2)
        integer :: kcs(:)        ! value = 0 : invalid point   value /= 0 : valid point
        integer, intent (out) :: ielsetq       ! for method >= 2 then

        integer, intent (out) :: numq          ! this is quantity nr ...

        double precision, intent(in), optional :: xyen(:, :) ! cellsize of associated eleset points

        ! locals
        integer :: k, kk

        type(telementset), pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
        type(tquantity), pointer :: quantities(:)    ! bevat de vraag

        type(tquantity), pointer :: hquantities(:)

        elementsets => subdoms(idom)%elementsets
        quantities => subdoms(idom)%quantities

        success = .true.

        kk = size(quantities)
        do k = 1, kk                                 ! check if this qid is new
            if (trim(quantities(k)%qid) == trim(qid)) then
                if (quantities(k)%kx /= kx) then
                    errormessage = 'addquantity: quantity already defined with another vectormax' // trim(qid)
                    success = .false.
                else
                    numq = k                           ! quantity already present, just pointer to it
                    ielsetq = quantities(k)%ielset
                    return
                endif
            endif
        enddo
        ! otherwise, increase size

        if (kk >= 1) then                         ! copy if necessary
            allocate(hquantities(kk))
            call copyquantities(kk, quantities, hquantities)
            deallocate (quantities)
        endif

        numq = kk + 1
        allocate(quantities(numq))

        if (kk >= 1) then
            call copyquantities(kk, hquantities, quantities) ! restore if necessary
            deallocate(hquantities)
        endif

        quantities(numq)%qid = qid
        quantities(numq)%kx = kx

        if (present(xyen)) then
            success = addelementset(elementsets, x, y, kcs, ielsetq, xyen)
        else
            success = addelementset(elementsets, x, y, kcs, ielsetq)
        endif

        quantities(numq)%ielset = ielsetq

        subdoms(idom)%elementsets => elementsets
        subdoms(idom)%quantities => quantities

    end function addquantity

    subroutine copyquantities(kk, quantities, hquantities)
        implicit none
        type(tquantity), pointer :: quantities(:)
        type(tquantity), pointer :: hquantities(:)
        integer :: kk
        hquantities(1:kk) = quantities(1:kk)
    end subroutine copyquantities

    function addfield(field, m, n, k, time, dmiss, ielset) result(success)
        implicit none
        logical :: success
        type(tfield), pointer :: field
        integer :: m
        integer :: n
        integer :: k
        integer :: ielset
        double precision :: dmiss
        double precision :: time

        success = .true.
        field%time = time
        field%ielset = ielset
        allocate(field % arr3d(1:m, 1:n, 1:k))
        field%arr3d(1:m, 1:n, 1:k) = dmiss ! 0
        field%arr2d => field%arr3d(:, :, 1)
        field%arr1d => field%arr3d(1, 1, :)
    end function addfield

    function destroyfield(field) result(success)
        implicit none
        logical :: success
        type(tfield) :: field
        success = .true.
        if(associated(field % arr3d)) deallocate(field % arr3d)
    end function destroyfield


    function addprovider(idom, qid, kx, filename, filetype, method, operand, nump, ielsetq) result(success)
        implicit none
        logical :: success

        ! this subroutine adds a provider in domainnr=threadnr idom (use 1 of no threads)
        ! for a requested quantity with id=qid

        ! arguments
        integer, intent(in) :: idom       ! threadnr = domainnr
        character(*), intent(in) :: qid        ! unique quantity identification

        integer, intent(in) :: kx         ! vectormax

        character(*), intent(in) :: filename   ! file name for meteo data file
        integer, intent(in) :: filetype   ! spw, arcinfo, uniuvp etc
        integer, intent(in) :: method     ! time/space interpolation method
        character(1), intent(in) :: operand    ! operand
        integer, intent(in) :: ielsetq    ! pointer to elementset on which this provider should give a quantity
        ! only relevant for method >= 2
        integer, intent(out) :: nump       ! index number of this provider


        ! locals
        integer :: mx
        integer :: num_cells_u_dir
        integer :: minp, mncoor
        double precision, allocatable :: xe(:)      ! lokale elementset waarop de provider is gedefinieerd
        double precision, allocatable :: ye(:)      ! wordt gedimensioneerd in de leesroutine
        integer, allocatable :: kcse(:)    ! en daarna doorgeschoven naar addelementset
        double precision :: dmiss
        double precision :: x0, y0, dxa, dya



        ! and extra for the triangulation/polytim :
        double precision, allocatable :: xe0(:)      ! dummy
        double precision, allocatable :: ye0(:)      ! dummy
        integer, allocatable :: kcse0(:)    ! indexen van aangeboden providers (meteostations)
        character(len = maxnamelen) :: filename0   ! file name for meteo data file
        character(len = maxnamelen) :: qidc        ! child qid bij multiple_uni, (gedeelte achtyer de underscore )
        integer :: filetype0   ! spw, arcinfo, uniuvp etc
        integer :: num0        ! number of single meteostations in this triangulation
        integer :: num02       ! 2*num0
        integer :: mnx0 = 1000 ! max number of single meteostations in this triangulation
        integer :: minp0       ! lun
        integer :: ielsetq0    !
        integer :: method0     ! if justupdate this provider can be part of another one that intp
        character(1) :: operand0    ! operand
        character(4) :: tex         ! stringetje
        integer :: ja, k, L, mx0, nx0
        integer :: num00 = 0
        logical :: jawel
        integer :: ierr, kk

        ! and for pointering to the timespaceworld
        type(telementset), pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
        type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
        type(tquantity), pointer :: quantities(:)    ! bevat de vraag

        success = .false.

        if (mdia < 0) call newfil(mdia, 'timespace.dia')

        dataproviders => subdoms(idom)%dataproviders
        elementsets => subdoms(idom)%elementsets
        quantities => subdoms(idom)%quantities

        if (filetype <= 0 .or. filetype > max_file_types) then
            errormessage = 'In addprovider: file ' // filename // ' filetype unknown'
            success = .false.
            return
        endif

        mx = 1  ! default dimension of provided field
        num_cells_u_dir = 1  !
        dmiss = dmiss_default
        allocate(xe(2))
        allocate(ye(2))
        allocate(kcse(2))! default dimensionering kale elementset
        xe = dmiss ; ye = dmiss ; kcse = 1

        if (filetype /= multiple_uni) then
            call oldfil(minp, filename)
        endif
        success = .true.

        select case (filetype)

        case (multiple_uni)

            L = len_trim(qid)
            qidc = qid(14:L)
            kk = 0
            do k = 1, size(dataproviders)
                if (trim(qidc) == trim(dataproviders(k)%qid)) then
                    kk = kk + 1
                    elementsets(ielsetq)%kcs(kk) = k
                endif
            enddo
            mx = size(elementsets(ielsetq)%kcs) ! de grootte van het providersfield gehaald uit de elementset v/d quantity

        case (uniform)

        case (unimagdir)

        case (svwp)

        case (arcinfo)

            success = readarcinfoheader(minp, mx, num_cells_u_dir, x0, y0, dxa, dya, dmiss)
            xe(1) = x0 ; ye(1) = y0 ; xe(2) = dxa ; ye(2) = dya

        case (d3d_flow_arcinfo)

            deallocate(xe, ye, kcse)
            success = readarcinfoheader_d3d(minp, mx, num_cells_u_dir, xe, ye, kcse, dmiss)
        case (curvi)

        case (triangulationmagdir)

            ja = 1; num0 = 0
            allocate(xe0(mnx0))
            allocate(ye0(mnx0))
            allocate(kcse0(mnx0))
            xe0 = dmiss ; ye0 = dmiss ; kcse0 = 0
            do while (ja == 1)
                success = connectsinglestationfile(minp, x0, y0, filename0, filetype0, method0, operand0, minp0, ja)
                if (ja == 1) then
                    xe(1) = x0 ; ye(1) = y0 ; num0 = num0 + 1
                    method0 = justupdate  ! this provider just updates
                    success = adddataprovider(elementsets, dataproviders, qid, filename0, filetype0, minp0, &
                            mx, num_cells_u_dir, kx, dmiss, xe, ye, kcse, method0, operand0, nump, ielsetq0)
                    kcse0 (num0) = nump ; xe0(num0) = x0 ; ye0(num0) = y0
                endif
            enddo

            if (num0 < 3) then
                success = .false.
                errormessage = 'minimum nr. of required stations for triangulation = 3'
                return
            endif

            success = .true.
            deallocate(xe, ye, kcse)
            num02 = 2 * num0  ! hierdoor wordt de kcs 2 maal groter en kan je later bijhouden of stationnetjes aan of uitgaan
            allocate(xe(num02))
            allocate(ye(num02))
            allocate(kcse(num02)) ! dimensionering op totaal aantal stationnetjes
            kcse(1:num02) = kcse0(1:num02) ; xe(1:num02) = xe0(1:num02) ; ye(1:num02) = ye0(1:num02)
            deallocate(xe0, ye0, kcse0)

            mx = size(elementsets(ielsetq)%kcs) ! de grootte van het providersfield gehaald uit de elementset v/d quantity

        case (poly_tim)

            method0 = justupdate
            operand0 = 'O'

            allocate(xe0(mnx0))
            allocate(ye0(mnx0))
            allocate(kcse0(mnx0))
            xe0 = dmiss ; ye0 = dmiss ; kcse0 = 0

            call read1polylin(minp, xe0, ye0, num0)
            if (num0 < 2) then
                success = .false.
                errormessage = 'minimum nr. of required support points for polyline boundary = 2'
                return
            endif

            L = index(filename, '.') - 1

            ielsetq0 = ielsetq
            do k = 1, num0
                filetype0 = -999                      ! unknown type
                write(tex, '(i4.4)') k

                filename0 = filename(1:L) // '_' // tex // '.tim'
                inquire (file = trim(filename0), exist = jawel)
                if (jawel) then
                    filetype0 = uniform        ! uniform=single time series vectormax = ..
                    call oldfil (minp0, filename0) ! hier file aansluiten en openlatenstaan voor lezen tijdseries
                    mx0 = 1 ; nx0 = 1
                endif

                if (filetype0 == -999) then
                    filename0 = filename(1:L) // '_' // tex // '.cmp'
                    inquire (file = trim(filename0), exist = jawel)
                    if (jawel) then
                        filetype0 = fourier            ! fourier components, watch this....
                        call oldfil (minp0, filename0) ! file aansluiten
                        success = readfourierdims(minp0, mx0, nx0)  ! lezen en rewind voor later lezen componenten
                    else
                        errormessage = 'In adddataprovider: file ' // trim(filename0) // ' was not found'
                        success = .false.
                        return
                    endif
                endif

                if (filetype /= -999) then
                    num00 = num00 + 1
                    xe(1) = xe0(k) ; ye(1) = ye0(k)
                    success = adddataprovider(elementsets, dataproviders, qid, filename0, filetype0, minp0, &
                            mx0, nx0, kx, dmiss, xe, ye, kcse, method0, operand0, nump, ielsetq0)

                    kcse0 (k) = nump       ! provider nump is attached to this support point
                else
                    kcse0 (k) = 0          ! this polyline support point has no provider attached
                endif

            enddo

            success = .true.
            deallocate(xe, ye, kcse)
            num02 = 2 * num0  ! hierdoor wordt de kcs 2 maal groter en kan je later bijhouden of stationnetjes aan of uitgaan
            allocate(xe(num02))
            allocate(ye(num02))
            allocate(kcse(num02)) ! dimensionering op totaal aantal stationnetjes
            kcse(1:num02) = kcse0(1:num02) ; xe(1:num02) = xe0(1:num02) ; ye(1:num02) = ye0(1:num02)
            deallocate(xe0, ye0, kcse0)

            mx = size(elementsets(ielsetq)%kcs) ! de grootte van het providersfield gehaald uit de elementset v/d quantity

        case (spiderweb)

            success = reaspwheader(minp, mx, num_cells_u_dir, dxa, dya, mncoor)
            xe(2) = dxa ; ye(2) = dya

        case (fourier)

            success = readfourierdims(minp, mx, num_cells_u_dir)

        end select

        if (.not. success) return

        success = adddataprovider(elementsets, dataproviders, qid, filename, filetype, minp, &
                mx, num_cells_u_dir, kx, dmiss, xe, ye, kcse, method, operand, nump, ielsetq)

        deallocate (xe, ye, kcse)

        subdoms(idom)%dataproviders => dataproviders
        subdoms(idom)%elementsets => elementsets
        subdoms(idom)%quantities => quantities

    end function addprovider

    function connectsinglestationfile(minp, x0, y0, filename0, filetype0, method0, operand0, minp0, ja) result(success)
        implicit none

        ! globals
        logical :: success

        character(*) :: filename0    ! single station filename
        double precision :: x0, y0       ! single station coordinates
        integer :: filetype0    ! type of file
        integer :: method0      ! type of file
        integer :: minp0        ! lun of single station file
        integer :: minp         ! lun of file that lists single stations
        integer :: ja           ! ja of nee, 1 of 0
        character (len = 1) :: operand0     ! + or =
        ! locals
        integer :: l1           ! indexpos
        character (len = maxnamelen) :: rec, keywrd, qid0
        double precision :: transformcoef0(0)

        success = .false.

        call readprovider(minp, qid0, filename0, filetype0, method0, operand0, transformcoef0, ja)
        if (ja == 1) then
            call oldfil(minp0, filename0)
            ! backspace(minp) ! tja....
            keywrd = 'XCOORDIN='
            call zoekja(minp, rec, keywrd, ja)
            l1 = index(rec, '=') + 1
            read(rec(l1:), *) x0

            keywrd = 'YCOORDIN='
            call zoekja(minp, rec, keywrd, ja)
            l1 = index(rec, '=') + 1
            read(rec(l1:), *) y0
            success = .true.
        endif

    end function connectsinglestationfile


    function adddataprovider(elementsets, dataproviders, qid, filename, filetype, minp, &
            my, num_cells_v_dir, ky, dmiss, x, y, kcs, method, operand, nump, ielsetq) result(success)
        implicit none
        ! globals
        logical :: success

        type(telementset), pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
        type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod

        character(*) :: qid          ! id of this quantity
        character(*) :: filename     ! file containing data
        character(1) :: operand      ! + or =
        integer :: filetype     ! type of file
        integer :: minp         ! unit nr
        integer :: my           ! field dim 1
        integer :: num_cells_v_dir           ! field dim 2
        integer :: ky           ! field dim 3
        double precision :: dmiss        ! misssing value
        double precision :: x(:)         ! elset, als size = 2, dan x0 en dx in x(1) en x(2)
        double precision :: y(:)         ! elset, als size = 2, dan y0 en dy in y(1) en y(2)
        integer :: kcs(:)       ! elset, value = 0 : invalid point   value /= 0 : valid point
        integer :: method       ! time/space interpolation method
        integer, intent(out) :: nump         ! index number of this provider
        integer, intent(in) :: ielsetq      ! pointer to elementset on which this provider should give a quantity
        ! only relevant for method >= 2



        ! locals
        type(tdataprovider), pointer :: hdataproviders(:)  ! h for help
        type(tfield), pointer :: hfield
        double precision :: time           ! starttijd
        integer :: ielset       ! pointer naar elementset bijhorend bij datagrid
        integer :: mx           ! size of meteo fields  m
        integer :: num_cells_u_dir           !                       n
        integer :: kx           !                       k

        integer :: kk, i, k, mnx

        success = .true.

        kk = size(dataproviders)
        if (kk >= 1) then
            allocate(hdataproviders(kk))

            do k = 1, kk
                hdataproviders(k) = dataproviders(k)
                allocate (hdataproviders(k)%field(0:1))
                mx = dataproviders(k)%mx
                num_cells_u_dir = dataproviders(k)%num_cells_u_dir
                kx = dataproviders(k)%kx
                do i = 0, 1
                    time = dataproviders(k)%field(i)%time
                    ielset = dataproviders(k)%field(i)%ielset
                    hfield => hdataproviders(k)%field(i)
                    success = addfield(hfield, mx, num_cells_u_dir, kx, time, dmiss, ielset)
                    hdataproviders(k)%field(i) = hfield
                    hdataproviders(k)%field(i)%arr3d = dataproviders(k)%field(i)%arr3d
                enddo
                if (associated(dataproviders(k)%indxn)) then
                    hdataproviders(k)%indxn => dataproviders(k)%indxn
                    hdataproviders(k)%wfn => dataproviders(k)%wfn
                    hdataproviders(k)%refresh = dataproviders(k)%refresh
                end if
            enddo
            deallocate (dataproviders)
        endif

        allocate   (dataproviders(kk + 1))

        if (kk >= 1) then
            do k = 1, kk
                dataproviders(k) = hdataproviders(k)
                allocate(dataproviders(k)%field(0:1))
                mx = hdataproviders(k)%mx
                num_cells_u_dir = hdataproviders(k)%num_cells_u_dir
                kx = hdataproviders(k)%kx
                do i = 0, 1
                    time = hdataproviders(k)%field(i)%time
                    ielset = hdataproviders(k)%field(i)%ielset
                    hfield => hdataproviders(k)%field(i)
                    success = addfield(hfield, mx, num_cells_u_dir, kx, time, dmiss, ielset)
                    dataproviders(k)%field(i) = hfield
                    dataproviders(k)%field(i)%arr3d = hdataproviders(k)%field(i)%arr3d
                enddo
            enddo
            deallocate (hdataproviders)
        endif

        k = kk + 1
        dataproviders(k)%qid = qid
        dataproviders(k)%filename = filename
        dataproviders(k)%filetype = filetype
        dataproviders(k)%minp = minp
        dataproviders(k)%dmiss = dmiss
        dataproviders(k)%method = method
        dataproviders(k)%operand = operand
        dataproviders(k)%ielsetq = ielsetq
        dataproviders(k)%it0 = 0
        dataproviders(k)%it1 = 1
        dataproviders(k)%mx = my
        dataproviders(k)%num_cells_u_dir = num_cells_v_dir
        dataproviders(k)%kx = ky
        nullify(dataproviders(k)%indxn)
        nullify(dataproviders(k)%wfn)

        allocate (dataproviders(k)%field(0:1))

        do i = 0, 1                                                 ! en voeg de velden toe
            success = addelementset(elementsets, x, y, kcs, ielset)  ! each field is defined on its own elementset
            hfield => dataproviders(k)%field(i)
            time = t01ini
            success = addfield(hfield, my, num_cells_v_dir, ky, time, dmiss, ielset)
        enddo

        if (method == weightfactors) then
            mnx = size(elementsets(ielsetq)%kcs)
            success = addindexweights(dataproviders(k)%indxn, dataproviders(k)%wfn, mnx)
            dataproviders(k)%refresh = 1
        endif

        nump = k

    end function adddataprovider

    function addindexweights(indxn, wfn, mnx)  result(success)
        implicit none
        ! globals
        logical :: success
        integer, pointer :: indxn(:, :)
        double precision, pointer :: wfn(:, :)
        integer :: mnx

        ! locals
        integer :: ierr

        success = .false.
        if (associated(indxn)) then
            deallocate(indxn)
            deallocate(wfn)
        end if
        allocate (indxn(3, mnx), stat = ierr)
        allocate (wfn(3, mnx), stat = ierr)
        success = .true.

    end function addindexweights


    !> Read the next quantity block that is found in a file.
    !! The (external forcing) file is opened elsewhere and read block-by-block
    !! by consecutive calls to this routine.
    subroutine readprovider(minp, qid, filename, filetype, method, operand, transformcoef, ja)
        implicit none

        ! globals
        integer, intent(in) :: minp      !< File pointer to already opened input file.
        integer, intent(out) :: filetype  !< File type of current quantity.
        integer, intent(out) :: method    !< Time-interpolation method for current quantity.
        character (len = *), intent(out) :: filename  !< Name of data file for current quantity.
        character (len = *), intent(out) :: qid       !< Identifier of current quantity (i.e., 'waterlevelbnd')
        character (len = 1), intent(out) :: operand   !< Operand w.r.t. previous data ('O'verride or '+'Append)
        double precision, intent(out) :: transformcoef(:) !< Transformation coefficients
        integer, intent(out) :: ja        !< Whether a block was successfully read or not.

        ! locals
        character (len = maxnamelen) :: rec, keywrd
        integer :: l1, i, jaopt

        keywrd = 'QUANTITY'
        call zoekja(minp, rec, trim(keywrd), ja)
        if (ja == 1) then
            l1 = index(rec, '=') + 1
            read(rec(l1:), '(a)', err = 990) qid
        else
            return
        endif

        keywrd = 'FILENAME'
        call zoekja(minp, rec, trim(keywrd), ja)
        if (ja == 1) then
            l1 = index(rec, '=') + 1
            read(rec(l1:), '(a)', err = 990) filename
        else
            return
        endif

        keywrd = 'FILETYPE'
        call zoekja(minp, rec, trim(keywrd), ja)
        if (ja == 1) then
            l1 = index(rec, '=') + 1
            read(rec(l1:), *, err = 990) filetype
        else
            return
        endif

        keywrd = 'METHOD'
        method = spaceandtime  ! default : spaceandtime
        call zoekja(minp, rec, trim(keywrd), ja)
        if (ja == 1) then
            l1 = index(rec, '=') + 1
            read(rec(l1:), *, err = 990) method
        else
            return
        endif

        keywrd = 'OPERAND'
        OPERAND = 'O'  ! hk : default =O
        call zoekja(minp, rec, trim(keywrd), ja)
        if (ja == 1) then
            l1 = index(rec, '=') + 1
            read(rec(l1:l1), '(a1)', err = 990) operand
        else
            return
        endif

        keywrd = 'VALUE'
        call zoekopt(minp, rec, trim(keywrd), jaopt)
        if (jaopt == 1) then
            transformcoef(1) = -999d0
            read (rec, *) transformcoef(1)
        end if

        keywrd = 'FACTOR'
        call zoekopt(minp, rec, trim(keywrd), jaopt)
        if (jaopt == 1) then
            transformcoef(2) = -999d0
            read (rec, *) transformcoef(2)
        end if

        return

        990 call readerror('reading ' // trim(keywrd) // ' but getting ', rec, minp)

    end subroutine readprovider


    subroutine read1polylin(minp, xs, ys, ns)

        implicit none

        double precision :: xs(:)
        double precision :: ys(:)
        integer :: ns
        integer :: minp

        ! locals
        character (len = maxnamelen) :: rec
        integer :: k

        ns = 0

        10 read(minp, '(a)', end = 999) rec
        if  (rec(1:1) == '*') goto 10

        read(minp, '(a)', end = 999) rec
        read(rec, *, err = 888) ns

        do k = 1, ns
            read(minp, '(a)', end = 999) rec
            read(rec, *, err = 777) xs(k), ys(k)
        enddo

        call doclose(minp)

        return

        999 call eoferror(minp) ; return

        888 call readerror('reading nrows but getting ', rec, minp) ; return

        777 call readerror('reading x, y  but getting ', rec, minp) ; return

    end subroutine read1polylin


    function timespaceinitialfield(xz, yz, zz, num_cells_u_dir, filename, filetype, method, operand, transformcoef) result(success)  !
        implicit none

        logical :: success

        integer, intent(in) :: num_cells_u_dir
        double precision, intent(in) :: xz(num_cells_u_dir)
        double precision, intent(in) :: yz(num_cells_u_dir)
        double precision, intent(out) :: zz(num_cells_u_dir)
        character(*), intent(in) :: filename   ! file name for meteo data file
        integer, intent(in) :: filetype   ! spw, arcinfo, uniuvp etc
        integer, intent(in) :: method     ! time/space interpolation method
        character(1), intent(in) :: operand    ! file name for meteo data file
        double precision, intent(in) :: transformcoef(:) !< Transformation coefficients

        double precision, allocatable :: xpli(:), ypli(:)
        integer :: maxpli = 1000
        integer :: minp0, inside, npli, k

        success = .false.
        call oldfil(minp0, filename)
        if (filetype == 10) then       ! polyfil
            allocate(xpli(maxpli), ypli(maxpli))

            call read1polylin(minp0, xpli, ypli, npli)
            do k = 1, num_cells_u_dir
                call pinpok(xz(k), yz(k), npli, xpli, ypli, inside)
                if (inside == 1) then
                    if (operand == '+') then
                        zz(k) = zz(k) + transformcoef(1)
                    else
                        zz(k) = transformcoef(1)
                    end if
                endif
            enddo
            deallocate(xpli, ypli)

        else if (filetype == 4) then  ! arcinfo bilinear todo

        else if (filetype == 7) then  ! triangulation    todo

        endif
        success = .true.

    end function timespaceinitialfield

    function timespaceinitialfield_int(xz, yz, zz, num_cells_u_dir, filename, filetype, method, operand, transformcoef)  result(success) ! deze subroutine moet veralgemeniseerd en naar meteo module

        implicit none

        logical :: success

        integer, intent(in) :: num_cells_u_dir
        double precision, intent(in) :: xz(num_cells_u_dir)
        double precision, intent(in) :: yz(num_cells_u_dir)
        integer, intent(out) :: zz(num_cells_u_dir)
        character(*), intent(in) :: filename   ! file name for meteo data file
        integer, intent(in) :: filetype   ! spw, arcinfo, uniuvp etc
        integer, intent(in) :: method     ! time/space interpolation method
        character(1), intent(in) :: operand    ! file name for meteo data file
        double precision, intent(in) :: transformcoef(:) !< Transformation coefficients

        double precision, allocatable :: xpli(:), ypli(:)
        integer :: maxpli = 1000
        integer :: minp0, inside, npli, k

        success = .false.

        allocate(xpli(maxpli), ypli(maxpli))

        call oldfil(minp0, filename)
        call read1polylin(minp0, xpli, ypli, npli)
        do k = 1, num_cells_u_dir
            call pinpok(xz(k), yz(k), npli, xpli, ypli, inside)
            if (inside == 1) then
                if (operand == '+') then
                    zz(k) = zz(k) + transformcoef(1)
                else
                    zz(k) = transformcoef(1)
                end if
            end if
        end do

        deallocate(xpli, ypli)

        success = .true.

    end function timespaceinitialfield_int


    function readfourierdims(minp, mx, num_cells_u_dir) result(success)
        implicit none

        integer :: minp
        integer :: mx      ! standard = 3, for omeg, ampl, phas
        integer :: num_cells_u_dir      ! nr of fourier components
        logical :: success


        ! locals
        character (len = maxnamelen) :: rec
        double precision :: omeg, ampl, phas
        integer :: L, i1, i2
        character(len = 10) :: compname

        success = .false.
        mx = 3 ; num_cells_u_dir = 0

        10 read(minp, '(a)', end = 999) rec
        if  (rec(1:1) == '*' .or. len_trim(rec) == 0) goto 10

        call ilocatestring(rec, i1, i2)
        if (ifirstletter(rec(i1:i2)) /= 0)  then ! probably m2, s2 etc instead of period in minutes

            read(rec(i1:i2), '(a)', err = 889)   compname

        else

            read(rec, *, err = 888) omeg, ampl, phas   ! todo als hier meer ampl, phas cols staan, increase mx todo

        endif
        num_cells_u_dir = num_cells_u_dir + 1
        goto 10

        999 rewind (minp)
        success = .true.
        return

        888 call readerror('reading omeg, ampl, phas, but getting ', rec, minp) ; return

        889 call readerror('reading component name, ampl, phas, but getting ', rec, minp) ; return

    end function readfourierdims


    function readfouriercompstim(minp, d0, d1, mx, num_cells_u_dir, kx, tim, tread) result(success)
        use m_waq_itdate
        !
        ! Read fourier components initially, next generate
        !
        implicit none
        !
        integer, intent (inout) :: minp
        integer, intent (in) :: mx
        integer, intent (in) :: num_cells_u_dir
        integer, intent (in) :: kx
        double precision, intent (in) :: tim
        double precision, intent (inout) :: tread
        double precision, dimension(:, :), intent (out) :: d0, d1
        logical :: success

        ! locals
        character(len = maxnamelen) :: rec
        character(len = 8) :: compname
        integer :: k, n, L, ierrs, i1, i2
        double precision :: fff, omeg, ampl, phas, omegt, phast

        success = .false.

        if (tread == t01ini) then

            n = 0
            10    read(minp, '(a)', end = 999) rec
            if  (rec(1:1) == '*' .or. len_trim(rec) == 0) goto 10

            n = n + 1

            call ilocatestring(rec, i1, i2)
            if (ifirstletter(rec(i1:i2)) /= 0)  then ! probably m2, s2 etc instead of period in minutes

                read(rec(i1:i2), '(a)', err = 889)   compname
                read(rec(i2 + 1:), *, err = 889)   (d0(2 + (k - 1) * kx, n), d0(3 + (k - 1) * kx, n), k = 1, kx)

                if (index(compname, 'A0') /= 0) then
                    d0(1, n) = 0 ; d0(3, :) = 0
                else
                    call asc(d0(1, n), d0(2, n), d0(3, n), compname, itdate, ierrs)
                endif

            else

                read(rec, *, err = 888) d0(1, n), (d0(2 + (k - 1) * kx, n), d0(3 + (k - 1) * kx, n), k = 1, kx)
                if (d0(1, n) /= 0) then
                    d0(1, n) = 2d0 * pi / d0(1, n)                     ! period specified in minutes => d0(1,*) (rad/min)
                endif

                do k = 1, kx
                    d0(3 + (k - 1) * kx, n) = d0(3 + (k - 1) * kx, n) * d2r ! pi/180d0 ! on input, fases in degrees, now converted to radians
                enddo

            endif

            goto 10
        endif

        999 continue
        if (tread == t01ini) then
            call doclose(minp)
        endif
        tread = tim          ! trick into being up to date, this will allways be update

        do k = 1, kx          ! for all kx quantities
            fff = 0d0
            do n = 1, num_cells_u_dir        ! for all num_cells_u_dir components
                omeg = d0(1, n)
                ampl = d0(2 + (k - 1) * kx, n)
                phas = d0(3 + (k - 1) * kx, n)

                omegt = d0(1, n) * tim
                phast = omegt - phas

                fff = fff + ampl * cos(phast)
            enddo
            d1(1, k) = fff
        enddo
        success = .true.

        return

        888 call readerror('reading omeg, ampl, phas, but getting ', rec, minp) ; return

        889 call readerror('reading component name, ampl, phas, but getting ', rec, minp) ; return

    end function readfouriercompstim

    function getmeteoerror() result(retval)
        implicit none
        character(300), pointer :: retval
        retval => errormessage
    end function getmeteoerror

    subroutine asc(omeg, ampl, phas, inaam, itdate, ierrs)
        use m_waq_precision
        implicit none
        !    include 'globdat.igd'
        !    include 'pardef.igd'
        !
        ! Global variables
        !
        double precision :: omeg, ampl, phas
        integer :: itdate, ierrs
        character(len = 8) :: inaam


        ! local

        !!  Number of error messages
        integer, parameter :: kcmp = 1
        integer, parameter :: mxkc = 234
        integer, dimension(6) :: jdatum               ! Date and time

        double precision, dimension(kcmp) :: fr                   ! Amplitude factors for the referenced components

        double precision, dimension(kcmp) :: v0u                  ! Astronomical arguments of the referenced components rad

        double precision, dimension(kcmp) :: w                    ! Angular velocity of the referenced components rad/hr

        integer :: i                    ! Help var.
        integer :: ik                   ! Help var.
        integer :: il                   ! Help var.
        integer :: j                    ! Help var.
        integer :: jaar                 ! Present year
        integer, dimension(16 * mxkc) :: jnaam                ! Help var.
        character(8), dimension(mxkc) :: knaam                ! Array with the names of all components
        character(80), dimension(mxkc) :: kombes               ! Array with tidal components
        double precision :: t                    ! Time in hours referred to January 1, 00:00 of the year 'JAAR'
        double precision, dimension(15) :: v                    ! Help var. to calculate V0U()
        double precision, dimension(25) :: f                    ! Help var. to calculate FR()



        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !
        !
        !     **************************
        !     ** LEES C0021KOMPBES IN **
        !     **************************
        !
        !     transform itdate into jdatum (integer)
        !     example, if datum = 28 07 1991 then
        !     itdate = 19910728
        !        jdatum (1) = 1991
        !        jdatum (2) =    7
        !        jdatum (3) =   28
        !        jdatum (4) =    0
        !        jdatum (5) =    0
        !        jdatum (6) =    0
        !
        !
        jdatum(3) = itdate - 100 * (itdate / 100)
        jdatum(2) = (itdate - 10000 * (itdate / 10000)) / 100
        jdatum(1) = itdate / 10000
        jdatum(4) = 0
        jdatum(5) = 0
        jdatum(6) = 0
        call kompbs(kombes)
        !
        ik = -15
        do i = 1, mxkc
            ik = ik + 16
            il = ik + 15
            read (kombes(i), '(a8,10i3,3(i1,i2))') knaam(i), (jnaam(j), j = ik, il)
        enddo
        !
        !     ***************************************
        !     ** LUS OVER DE OPGEGEVEN TIJDSTIPPEN **
        !     ***************************************
        !
        jaar = jdatum(1)
        !
        call datumi(jaar, jdatum, t)
        call hulpgr(jaar, t, v, f)
        call bewvuf(ierrs, kcmp, mxkc, inaam, knaam, &
                & jnaam, w, v0u, fr, v, &
                & f)
        omeg = w(1) / 60d0
        ampl = ampl * fr(1)
        phas = phas * d2r - v0u(1)
        !    phas = phas*d2r + v0u(1)
    end subroutine asc
    subroutine datumi(jaar, jdatum, t)
        use m_waq_precision
        !
        implicit none
        !
        ! Global variables
        !
        integer, intent(in) :: jaar   !!  Year
        integer, dimension(6), intent(in) :: jdatum !!  Date and time
        double precision :: t      !!  Time in hours referred to January 1,
        !!  00:00 of the year 'JAAR'
        !
        ! Local variables
        !
        integer :: i     ! Help var.
        integer :: jhulp ! Help var.
        integer :: mnd   ! Help var. for the month
        double precision :: rlen  ! Length of a year in hours
        double precision, dimension(12) :: rmd   ! The number of days of the cumulated counted months
        !
        !! executable statements -------------------------------------------------------
        !
        rmd(1) = 0d0
        rmd(2) = 31d0
        rmd(3) = 59d0
        rmd(4) = 90d0
        rmd(5) = 120d0
        rmd(6) = 151d0
        rmd(7) = 181d0
        rmd(8) = 212d0
        rmd(9) = 243d0
        rmd(10) = 273d0
        rmd(11) = 304d0
        rmd(12) = 334d0
        !
        jhulp = jdatum(1)
        !
        ! bereken maand definities voor schrikkeljaren
        ! jaar deelbaar door 4 minus eeuwen welke niet deelbaar zijn door 4
        !
        if (mod(jhulp, 4) == 0) then
            if (mod(jhulp, 100)/=0 .or. mod(jhulp, 400)==0) then
                do i = 3, 12
                    rmd(i) = rmd(i) + 1d0
                enddo
            endif
        endif
        !
        mnd = jdatum(2)
        t = rmd(mnd) * 24d0 + real(jdatum(3) - 1, dp) * 24d0 + real(jdatum(4), dp)          &
                & + real(jdatum(5), dp) / 60d0 + real(jdatum(6), dp) / 3600d0
        !
        ! hypothetisch geval (jhulp = jdatum(1) en jaar = jdatum(1))
        !
        if (jhulp /= jaar) then
            rlen = 8760d0
            if (jhulp <= jaar) then
                if (mod(jhulp, 4) == 0) rlen = 8784d0
                t = t - rlen
            else
                if (mod(jaar, 4) == 0) rlen = 8784d0
                t = t + rlen
            endif
        endif
    end subroutine datumi
    subroutine hulpgr(jaar, tm1, v, f)
        use m_waq_precision
        !
        implicit none
        !
        ! Global variables
        !
        integer, intent(in) :: jaar !!  Present year
        double precision, intent(in) :: tm1  !!  Given time in hours referred to
        !!  January 1, 00:00:00
        double precision, dimension(15) :: v    !!  Help var. to calculate V0U()
        double precision, dimension(25) :: f    !!  Help var. to calculate FR()
        !
        ! Local variables
        !
        integer :: ischrk  ! Number of leap-years since 1900
        integer :: j
        double precision :: ci
        double precision :: ci4
        double precision :: cri
        double precision :: dhalf   ! Value for 0.5 in SIGN function
        double precision :: p
        double precision :: pix2    ! PI*2.
        double precision :: q
        double precision :: rad     ! PI/180.
        double precision :: ri
        double precision :: rjaar   ! Real value of JAAR - 1900
        double precision :: rk
        double precision :: rn1
        double precision :: s2ri
        double precision :: si
        double precision :: si4
        double precision :: sri
        double precision :: sri3
        double precision :: tm3     ! ISCHRK + TM1/24.0, i.e. the number of correction-days since January 1, 1900 00:00 hour, after the length of a year is set to 365 days in the first instance
        double precision :: z
        !
        !! executable statements -------------------------------------------------------
        !
        ! bereken tm3 uitgaande van tm1 plus aantal schrikkeldagen extra
        ! sinds 1900. Niet door 4 deelbare eeuwen zijn geen schrikkeldagen)
        !
        pix2 = 8d0 * atan(1d0)
        dhalf = 0.5d0
        rad = pix2 / 360d0
        rjaar = real(jaar - 1900, dp)
        ischrk = int((rjaar - 0.99d0) / 4d0) - int((rjaar - 0.99d0) / 100d0)        &
                & + int((rjaar + 300d0 - 0.99d0) / 400d0)
        tm3 = real(ischrk, dp) + tm1 / 24d0
        !
        v(1) = (180.000d0 + 360.0000000d0 * tm3) * rad
        v(2) = (277.026d0 + 129.3848200d0 * rjaar + 13.176396800000d0 * tm3) * rad
        v(3) = (334.384d0 + 40.6624700d0 * rjaar + 0.111404000000d0 * tm3) * rad
        v(4) = (280.190d0 - 0.2387136d0 * rjaar + 0.985647360000d0 * tm3) * rad
        v(5) = (281.221d0 + 0.0171800d0 * rjaar + 0.000047064943d0 * tm3) * rad
        v(8) = (259.156d0 + 340.6718100d0 * rjaar - 0.052953945000d0 * tm3) * rad
        !
        z = 0.009415d0
        p = atan(z * sin(v(8)) / (1d0 + z * (1d0 - cos(v(8)))))
        z = -0.17794d0
        q = atan(z * sin(v(8)) / (1d0 + z * (1d0 - cos(v(8)))))
        !
        v(6) = -p - q
        v(7) = p - q
        !
        rk = 0.9137d0 - 0.03569d0 * cos(v(8))
        ri = atan(sqrt(1d0 - rk * rk) / rk)
        !
        v(9) = ri
        !
        p = mod(v(3), pix2) - pix2 * (sign(dhalf, v(3)) - dhalf)
        rk = v(6)
        rn1 = v(7)
        !
        ! Initialisatie van regelmatig voorkomende argumenten
        !
        s2ri = sin(2d0 * ri)
        sri = sin(ri)
        si = sin(0.5d0 * ri)
        cri = cos(ri)
        ci = cos(0.5d0 * ri)
        !
        v(10) = atan(s2ri * sin(rn1) / (s2ri * cos(rn1) + 0.3347d0))
        v(11) = atan(sri * sri * sin(2d0 * rn1) / (sri * sri * cos(2d0 * rn1) + 0.0727d0))
        v(12) = atan(sin(2d0 * (p - rk)) / (3d0 * cri / (ci * ci) + cos(2d0 * (p - rk))))
        v(13) = atan(sin(2d0 * (p - rk)) / (ci * ci / (si * si * 6d0) - cos(2d0 * (p - rk)))&
                &)
        v(14) = 3d0 * v(10)
        v(15) = 0d0
        !
        ! Alle hoeken terugbrengen tot het interval 0 - 2*pi radialen
        !
        do j = 1, 15
            v(j) = mod(v(j), pix2) - pix2 * (sign(dhalf, v(j)) - dhalf)
        enddo
        !
        ci4 = ci * ci * ci * ci
        si4 = si * si * si * si
        sri3 = sri * sri * sri
        !
        f(1) = (2d0 / 3d0 - sri * sri) / 0.5021d0
        f(2) = sri * sri / 0.1578d0
        f(3) = sri * ci * ci / 0.38d0
        f(4) = s2ri / 0.7214d0
        f(5) = sri * si * si / 0.0164d0
        f(6) = ci4 / 0.9154d0
        f(7) = sri * sri / 0.1565d0
        f(8) = si4 / 0.0017d0
        f(9) = (sri - 1.25d0 * sri3) / 0.3192d0
        f(10) = sri3 / 0.063d0
        f(11) = sri * sri * ci * ci / 0.1518d0
        f(12) = (1d0 - 10d0 * si * si + 15d0 * si4) * ci * ci / 0.5873d0
        f(13) = (1d0 - 10d0 * ci * ci + 15d0 * ci4) * si * si / 0.2147d0
        f(14) = sri * ci4 / 0.3658d0
        f(15) = (ci * ci - 2d0 / 3d0) * sri * ci * ci / 0.1114d0
        f(16) = (ci * ci - 1d0 / 3d0) * sri * si * si / 0.0103d0
        f(17) = ci4 * ci * ci / 0.8758d0
        f(18) = ci4 * si * si / 0.038d0
        f(19) = sqrt(0.8965d0 * s2ri * s2ri + 0.6001d0 * s2ri * cos(rn1) + 0.1006d0)
        f(20) = sqrt(19.0444d0 * sri3 * sri + 2.7702d0 * sri * sri * cos(2d0 * rn1)           &
                & + 0.0981d0)
        f(21) = 6d0 * cri * cos(2d0 * (p - rk)) / (ci * ci) + 9d0 * cri * cri / (ci4)
        f(21) = 2.6316d0 * sri * ci * ci * 0.5d0 * sqrt(1d0 + f(21))
        f(22) = 36d0 * si4 / (ci4) - 12d0 * si * si / (ci * ci) * cos(2d0 * (p - rk))
        f(22) = 1.0924d0 * ci4 * sqrt(1d0 + f(22))
    end subroutine hulpgr
    function cmpnum(num)
        use m_waq_precision
        implicit none
        !
        ! Local parameters
        !
        integer, parameter :: mxcmp = 234 !  Description and declaration in tfzeta.igs
        !
        ! Global variables
        !
        integer, intent(in) :: num
        !!  Pointer for the tidal components
        character(8) :: cmpnum
        !!  Name of the chosen tidal component
        !
        !
        ! Local variables
        !
        character(8), dimension(0:mxcmp) :: l ! Array with the names of the tidal components
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !
        !
        !
        l(0) = 'A0      '
        l(1) = 'SA      '
        l(2) = 'SSA     '
        l(3) = 'MSM     '
        l(4) = 'MM      '
        l(5) = 'MSF     '
        l(6) = 'MS0     '
        l(7) = 'MF      '
        l(8) = 'KO0     '
        l(9) = 'MK0     '
        l(10) = 'SNU     '
        l(11) = 'SN      '
        l(12) = 'MSTM    '
        l(13) = 'MFM     '
        l(14) = '2SM     '
        l(15) = 'MSQM    '
        l(16) = 'MQM     '
        l(17) = '2SMN    '
        l(18) = '2OK1    '
        l(19) = '2Q1     '
        l(20) = 'NJ1     '
        l(21) = 'SIGMA1  '
        l(22) = 'MUK1    '
        l(23) = 'NUJ1    '
        l(24) = 'Q1      '
        l(25) = 'NK1     '
        l(26) = 'RO1     '
        l(27) = 'NUK1    '
        l(28) = 'O1      '
        l(29) = 'TAU1    '
        l(30) = 'MP1     '
        l(31) = 'M1B     '
        l(32) = 'M1C     '
        l(33) = 'M1A     '
        l(34) = 'M1      '
        l(35) = 'NO1     '
        l(36) = 'CHI1    '
        l(37) = 'LP1     '
        l(38) = 'PI1     '
        l(39) = 'TK1     '
        l(40) = 'P1      '
        l(41) = 'SK1     '
        l(42) = 'S1      '
        l(43) = 'K1      '
        l(44) = 'MO1     '
        l(45) = 'SP1     '
        l(46) = 'PSI1    '
        l(47) = 'RP1     '
        l(48) = 'FI1     '
        l(49) = 'KP1     '
        l(50) = 'THETA1  '
        l(51) = 'LABDAO1 '
        l(52) = 'J1      '
        l(53) = 'MQ1     '
        l(54) = '2PO1    '
        l(55) = 'SO1     '
        l(56) = 'OO1     '
        l(57) = '2KO1    '
        l(58) = 'UPSILON1'
        l(59) = 'KQ1     '
        l(60) = '2MN2S2  '
        l(61) = '3MKS2   '
        l(62) = '2NS2    '
        l(63) = '3MS2    '
        l(64) = 'OQ2     '
        l(65) = 'MNK2    '
        l(66) = 'EPSILON2'
        l(67) = 'MNS2    '
        l(68) = '2ML2S2  '
        l(69) = 'MNUS2   '
        l(70) = 'MNK2S2  '
        l(71) = '2MS2K2  '
        l(72) = 'O2      '
        l(73) = 'NLK2    '
        l(74) = '2MK2    '
        l(75) = '2N2     '
        l(76) = 'MU2     '
        l(77) = '2MS2    '
        l(78) = 'SNK2    '
        l(79) = 'NA2     '
        l(80) = 'N2      '
        l(81) = 'KQ2     '
        l(82) = 'NB2     '
        l(83) = 'NU2     '
        l(84) = '3MSN2   '
        l(85) = '2KN2S2  '
        l(86) = 'OP2     '
        l(87) = 'MSK2    '
        l(88) = 'GAMMA2  '
        l(89) = 'ALFA2   '
        l(90) = 'MPS2    '
        l(91) = 'MA2     '
        l(92) = 'M2      '
        l(93) = 'KO2     '
        l(94) = 'MSP2    '
        l(95) = 'MB2     '
        l(96) = 'DELTA2  '
        l(97) = 'MKS2    '
        l(98) = 'M2(KS)2 '
        l(99) = '2SN(MK)2'
        l(100) = 'LABDA2  '
        l(101) = 'SNM2    '
        l(102) = '2MN2    '
        l(103) = 'L2      '
        l(104) = 'L2A     '
        l(105) = 'L2B     '
        l(106) = '2SK2    '
        l(107) = 'T2      '
        l(108) = 'S2      '
        l(109) = 'KP2     '
        l(110) = 'R2      '
        l(111) = 'K2      '
        l(112) = 'MSNU2   '
        l(113) = 'MSN2    '
        l(114) = 'ZETA2   '
        l(115) = 'ETA2    '
        l(116) = 'KJ2     '
        l(117) = 'MKN2    '
        l(118) = '2KM(SN)2'
        l(119) = '2SM2    '
        l(120) = 'SKM2    '
        l(121) = '2MS2N2  '
        l(122) = '2SNU2   '
        l(123) = '2SN2    '
        l(124) = 'SKN2    '
        l(125) = 'MQ3     '
        l(126) = 'NO3     '
        l(127) = 'MO3     '
        l(128) = '2MK3    '
        l(129) = '2MP3    '
        l(130) = 'M3      '
        l(131) = 'NK3     '
        l(132) = 'SO3     '
        l(133) = 'MP3     '
        l(134) = 'MK3     '
        l(135) = 'SP3     '
        l(136) = '2MQ3    '
        l(137) = 'SK3     '
        l(138) = '2SO3    '
        l(139) = 'K3      '
        l(140) = '4MS4    '
        l(141) = '2MNS4   '
        l(142) = '3MK4    '
        l(143) = 'MNLK4   '
        l(144) = '3MS4    '
        l(145) = 'MSNK4   '
        l(146) = 'MN4     '
        l(147) = 'MNU4    '
        l(148) = '2MLS4   '
        l(149) = '2MSK4   '
        l(150) = 'M4      '
        l(151) = '2MKS4   '
        l(152) = 'SN4     '
        l(153) = '3MN4    '
        l(154) = '2SMK4   '
        l(155) = 'MS4     '
        l(156) = 'MK4     '
        l(157) = '2SNM4   '
        l(158) = '2MSN4   '
        l(159) = 'SL4     '
        l(160) = 'S4      '
        l(161) = 'SK4     '
        l(162) = '2SMN4   '
        l(163) = '3SM4    '
        l(164) = '2SKM4   '
        l(165) = 'MNO5    '
        l(166) = '3MK5    '
        l(167) = '3MP5    '
        l(168) = 'M5      '
        l(169) = 'MNK5    '
        l(170) = '2MP5    '
        l(171) = 'MSO5    '
        l(172) = '3MO5    '
        l(173) = 'MSK5    '
        l(174) = '3KM5    '
        l(175) = '2(MN)S6 '
        l(176) = '3MNS6   '
        l(177) = '4MK6    '
        l(178) = '2NM6    '
        l(179) = '4MS6    '
        l(180) = '2MSNK6  '
        l(181) = '2MN6    '
        l(182) = '2MNU6   '
        l(183) = '3MSK6   '
        l(184) = 'M6      '
        l(185) = 'MSN6    '
        l(186) = 'MNK6    '
        l(187) = '4MN6    '
        l(188) = 'MKNU6   '
        l(189) = '2(MS)K6 '
        l(190) = '2MS6    '
        l(191) = '2MK6    '
        l(192) = '2SN6    '
        l(193) = '3MSN6   '
        l(194) = 'MKL6    '
        l(195) = '2SM6    '
        l(196) = 'MSK6    '
        l(197) = 'S6      '
        l(198) = '2MNO7   '
        l(199) = '2NMK7   '
        l(200) = 'M7      '
        l(201) = '2MSO7   '
        l(202) = 'MSKO7   '
        l(203) = '2(MN)8  '
        l(204) = '3MN8    '
        l(205) = '3MNKS8  '
        l(206) = 'M8      '
        l(207) = '2MSN8   '
        l(208) = '2MNK8   '
        l(209) = '3MS8    '
        l(210) = '3MK8    '
        l(211) = '2SNM8   '
        l(212) = 'MSNK8   '
        l(213) = '2(MS)8  '
        l(214) = '2MSK8   '
        l(215) = '3SM8    '
        l(216) = '2SMK8   '
        l(217) = 'S8      '
        l(218) = '2(MN)K9 '
        l(219) = '3MNK9   '
        l(220) = '4MK9    '
        l(221) = '3MSK9   '
        l(222) = '4MN10   '
        l(223) = 'M10     '
        l(224) = '3MSN10  '
        l(225) = '4MS10   '
        l(226) = '2(MS)N10'
        l(227) = '2MNSK10 '
        l(228) = '3M2S10  '
        l(229) = '4MSK11  '
        l(230) = 'M12     '
        l(231) = '4MSN12  '
        l(232) = '5MS12   '
        l(233) = '3MNKS12 '
        l(234) = '4M2S12  '
        !
        cmpnum = 'ERROR   '
        if (num>=0 .and. num<=mxcmp) then
            cmpnum = l(num)
        endif
    end function cmpnum
    subroutine kompbs(l)
        use m_waq_precision
        implicit none
        !
        ! Global variables
        !
        character(80), dimension(234), intent(out) :: l
        !!  Array with tidal components
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !
        !
        l(1) = 'SA                 1                            '
        l(2) = 'SSA                2                            '
        l(3) = 'MSM          1  1 -2                  1 1       '
        l(4) = 'MM           1 -1                     1 1       '
        l(5) = 'MSF          2    -2                  1 1       '
        l(6) = 'MS0          2    -2    -2  2         1 6       '
        l(7) = 'MF           2          -2            1 2       '
        l(8) = 'KO0          2          -2  1 -2-10   1 3119    '
        l(9) = 'MK0          2          -2  2   -11   120       '
        l(10) = 'SNU          3  1 -4    -2  2         1 6       '
        l(11) = 'SN           3 -1 -2    -2  2         1 6       '
        l(12) = 'MSTM         3  1 -2    -2            1 2       '
        l(13) = 'MFM          3 -1       -2            1 2       '
        l(14) = '2SM          4    -4    -4  4         2 6       '
        l(15) = 'MSQM         4    -2    -2            1 2       '
        l(16) = 'MQM          4 -2       -2            1 2       '
        l(17) = '2SMN         5 -1 -4    -4  4         2 6       '
        l(18) = '2OK1      1 -4     1     4 -2 -1+10   2 3119    '
        l(19) = '2Q1       1 -4  2  1     2 -1  1      1 3       '
        l(20) = 'NJ1       1 -4  2  1     2 -1  1      1 41 6    '
        l(21) = 'SIGMA1    1 -4     3     2 -1  1      1 3       '
        l(22) = 'MUK1      1 -4     3     2 -2   +10   1 6119    '
        l(23) = 'NUJ1      1 -4     3     2 -1  1      1 41 6    '
        l(24) = 'Q1        1 -3  1  1     2 -1  1      1 3       '
        l(25) = 'NK1       1 -3  1  1     2 -2  1+10   1 6119    '
        l(26) = 'RO1       1 -3 -1  3     2 -1  1      1 3       '
        l(27) = 'NUK1      1 -3 -1  3     2 -2 +1+10   1 6119    '
        l(28) = 'O1        1 -2     1     2 -1  1      1 3       '
        l(29) = 'TAU1      1 -2     3       -1 -1      1 4       '
        l(30) = 'MP1       1 -2     3     2 -2 -1      1 6       '
        l(31) = 'M1B       1 -1 -1  1     2 -1 -1      1 3       '
        l(32) = 'M1C       1 -1     1     1 -1         112       '
        l(33) = 'M1A       1 -1  1  1       -1 -1      1 4       '
        l(34) = 'M1        1 -1  1  1       -1 -1-12   121       '
        l(35) = 'NO1       1 -1  1  1       -1 -1      1 31 6    '
        l(36) = 'CHI1      1 -1 -1 +3       -1 -1      1 4       '
        l(37) = 'LP1       1 -1 -1  3     2 -2  1-13   122       '
        l(38) = 'PI1       1       -2 +1        1                '
        l(39) = 'TK1       1       -2  1        1+10   119       '
        l(40) = 'P1        1       -1           1                '
        l(41) = 'SK1       1       -1           1+10   119       '
        l(42) = 'S1        1                                     '
        l(43) = 'K1        1        1          -1-10   119       '
        l(44) = 'MO1       1        1       -1 -1      1 31 6    '
        l(45) = 'SP1       1        1          -1                '
        l(46) = 'PSI1      1        2 -1       -1                '
        l(47) = 'RP1       1        2 -1        1                '
        l(48) = 'FI1       1        3          -1                '
        l(49) = 'KP1       1        3          -1-11   120       '
        l(50) = 'THETA1    1  1  1 -1       -1 -1      1 4       '
        l(51) = 'LABDAO1   1  1  1 -1       -1  1      1 31 6    '
        l(52) = 'J1        1  1 -1  1       -1 -1      1 4       '
        l(53) = 'MQ1       1  1 -1  1       -1 -1      1 31 6    '
        l(54) = '2PO1      1  2    -3    -2  1  1      1 3       '
        l(55) = 'SO1       1  2    -1    -2  1 -1      1 3       '
        l(56) = 'OO1       1  2     1    -2 -1 -1      1 5       '
        l(57) = '2KO1      1  2     1    -2  1  1-10-101 3219    '
        l(58) = 'UPSILON1  1  3 -1  1    -2 -1  1      1 5       '
        l(59) = 'KQ1       1  3 -1  1    -2  1 -1-11   1 3120    '
        l(60) = '2MN2S2    2 -7  1  6     6 -6         3 6       '
        l(61) = '3MKS2     2 -6     4     6 -6   +11   3 6120    '
        l(62) = '2NS2      2 -6  2  4     4 -4         2 6       '
        l(63) = '3MS2      2 -6     6     6 -6         3 6       '
        l(64) = 'OQ2       2 -5  1  2     4 -2  2      2 3       '
        l(65) = 'MNK2      2 -5  1  2     4 -4   +11   2 6120    '
        l(66) = 'EPSILON2  2 -5  1  4     2 -2         1 6       '
        l(67) = 'MNS2      2 -5  1  4     4 -4         2 6       '
        l(68) = '2ML2S2    2 -5 -1  6     6 -6  2-13   2 6122    '
        l(69) = 'MNUS2     2 -5 -1  6     4 -4         2 6       '
        l(70) = 'MNK2S2    2 -5  1  6     4 -4  0-11   2 6120    '
        l(71) = '2MS2K2    2 -4           4 -4   +11+112 6220    '
        l(72) = 'O2        2 -4     2     4 -2  2      2 3       '
        l(73) = 'NLK2      2 -4     2     4 -4  2+11-131 6120122 '
        l(74) = '2MK2      2 -4     2     4 -4   +11   1 6120    '
        l(75) = '2N2       2 -4  2  2     2 -2         1 6       '
        l(76) = 'MU2       2 -4     4     2 -2         1 6       '
        l(77) = '2MS2      2 -4     4     4 -4         2 6       '
        l(78) = 'SNK2      2 -3  1        2 -2   +11   1 6120    '
        l(79) = 'NA2       2 -3  1  1  1                         '
        l(80) = 'N2        2 -3  1  2     2 -2         1 6       '
        l(81) = 'KQ2       2 -3  1  2     2 -1   -10   1 3119    '
        l(82) = 'NB2       2 -3  1  3 -1                         '
        l(83) = 'NU2       2 -3 -1  4     2 -2         1 6       '
        l(84) = '3MSN2     2 -3  1  6     4 -4         4 6       '
        l(85) = '2KN2S2    2 -3  1  6     2 -2   -11-111 6220    '
        l(86) = 'OP2       2 -2           2 -1  2      1 3       '
        l(87) = 'MSK2      2 -2           2 -2   +11   1 6120    '
        l(88) = 'GAMMA2    2 -2  2        2 -2  2      1 6       '
        l(89) = 'ALFA2     2 -2     1     2 -2  2      1 6       '
        l(90) = 'MPS2      2 -2     1     2 -2  1      1 6       '
        l(91) = 'MA2       2 -2     1                            '
        l(92) = 'M2        2 -2     2     2 -2         1 6       '
        l(93) = 'KO2       2 -2     2     2 -1   -10   1 3119    '
        l(94) = 'MSP2      2 -2     3     2 -2 -1      1 6       '
        l(95) = 'MB2       2 -2     3                            '
        l(96) = 'DELTA2    2 -2     4       -2  0      1 7       '
        l(97) = 'MKS2      2 -2     4     2 -2   -11   1 6120    '
        l(98) = 'M2(KS)2   2 -2     6     2 -2   -11-111 6220    '
        l(99) = '2SN(MK)2  2 -1  1 -2            +11   2 6120    '
        l(100) = 'LABDA2    2 -1  1        2 -2  2      1 6       '
        l(101) = 'SNM2      2 -1  1                     2 6       '
        l(102) = '2MN2      2 -1 -1  2     2 -2         3 6       '
        l(103) = 'L2        2 -1 -1  2     2 -2  2-13   122       '
        l(104) = 'L2A       2 -1 -1  2     2 -2  2      1 6       '
        l(105) = 'L2B       2 -1  1  2       -2         1 7       '
        l(106) = '2SK2      2       -2            +11   120       '
        l(107) = 'T2        2       -1  1                         '
        l(108) = 'S2        2                                     '
        l(109) = 'KP2       2                     -10   119       '
        l(110) = 'R2        2        1 -1        2                '
        l(111) = 'K2        2        2            -11   120       '
        l(112) = 'MSNU2     2  1  1 -2                            '
        l(113) = 'MSN2      2  1 -1                     2 6       '
        l(114) = 'ZETA2     2  1  1          -2         1 7       '
        l(115) = 'ETA2      2  1 -1  2       -2         1 7       '
        l(116) = 'KJ2       2  1 -1  2       -1 -2-10   1 4119    '
        l(117) = 'MKN2      2  1 -1  2            -11   2 6120    '
        l(118) = '2KM(SN)2  2  1 -1  4            -11-112 6220    '
        l(119) = '2SM2      2  2    -2    -2  2         1 6       '
        l(120) = 'SKM2      2  2          -2  2   -11   1 6120    '
        l(121) = '2MS2N2    2  2 -2                     2 6       '
        l(122) = '2SNU2     2  3  1 -4    -2  2         1 6       '
        l(123) = '2SN2      2  3 -1 -2    -2  2         1 6       '
        l(124) = 'SKN2      2  3 -1       -2  2   -11   1 6120    '
        l(125) = 'MQ3       3 -5  1  3     4 -3  1      1 31 6    '
        l(126) = 'NO3       3 -5  1  3     4 -3  1      1 31 6    '
        l(127) = 'MO3       3 -4     3     4 -3  1      1 31 6    '
        l(128) = '2MK3      3 -4     3     4 -4  1+10   2 6119    '
        l(129) = '2MP3      3 -4     5     4 -4 -1      2 6       '
        l(130) = 'M3        3 -3     3     3 -3         117       '
        l(131) = 'NK3       3 -3  1  3     2 -2 -1-10   1 6119    '
        l(132) = 'SO3       3 -2     1     2 -1  1      1 3       '
        l(133) = 'MP3       3 -2     1     2 -2  1      1 6119    '
        l(134) = 'MK3       3 -2     3     2 -2 -1-10   1 6119    '
        l(135) = 'SP3       3       -1           1                '
        l(136) = '2MQ3      3 -1 -1  3     2 -3 -1      1 32 6    '
        l(137) = 'SK3       3        1          -1-10   119       '
        l(138) = '2SO3      3  2    -1    -2  1 -1      1 3       '
        l(139) = 'K3        3        3          -1-10-11119120    '
        l(140) = '4MS4      4 -8     8     8 -8         4 6       '
        l(141) = '2MNS4     4 -7  1  6     6 -6         3 6       '
        l(142) = '3MK4      4 -6     4     6 -6   +11   3 6120    '
        l(143) = 'MNLK4     4 -6     4     6 -6  2+11-132 6120122 '
        l(144) = '3MS4      4 -6     6     6 -6         3 6       '
        l(145) = 'MSNK4     4 -5  1  2     4 -4   +11   2 6120    '
        l(146) = 'MN4       4 -5  1  4     4 -4         2 6       '
        l(147) = 'MNU4      4 -5 -1  6     4 -4         2 6       '
        l(148) = '2MLS4     4 -5 -1  6     6 -6  2-13   2 6122    '
        l(149) = '2MSK4     4 -4     2     4 -4   +11   2 6120    '
        l(150) = 'M4        4 -4     4     4 -4         2 6       '
        l(151) = '2MKS4     4 -4     6     4 -4   -11   2 6120    '
        l(152) = 'SN4       4 -3  1  2     2 -2         1 6       '
        l(153) = '3MN4      4 -3 -1  4     4 -4         4 6       '
        l(154) = '2SMK4     4 -2           2 -2   +11   1 6120    '
        l(155) = 'MS4       4 -2     2     2 -2         1 6       '
        l(156) = 'MK4       4 -2     4     2 -2   -11   1 6120    '
        l(157) = '2SNM4     4 -1  1                     2 6       '
        l(158) = '2MSN4     4 -1 -1  2     2 -2         3 6       '
        l(159) = 'SL4       4 -1 -1  2     2 -2  2-13   122       '
        l(160) = 'S4        4                                     '
        l(161) = 'SK4       4        2            -11   120       '
        l(162) = '2SMN4     4  1 -1                     2 6       '
        l(163) = '3SM4      4  2    -2    -2  2         1 6       '
        l(164) = '2SKM4     4  2          -2  2   -11   1 6120    '
        l(165) = 'MNO5      5 -7  1  5     6 -5  1      1 32 6    '
        l(166) = '3MK5      5 -6     5     6 -6  1+10   3 6119    '
        l(167) = '3MP5      5 -6     7     6 -6 -1      3 6       '
        l(168) = 'M5        5 -5  1  5     4 -5 -1-12   2 6121    '
        l(169) = 'MNK5      5 -5  1  5     4 -4 -1-10   2 6119    '
        l(170) = '2MP5      5 -4     3     4 -4  1      2 6       '
        l(171) = 'MSO5      5 -4     3     4 -3         1 31 6    '
        l(172) = '3MO5      5 -4     5     4 -5 -1      1 33 6    '
        l(173) = 'MSK5      5 -2     3     2 -2 -1-10   1 6119    '
        l(174) = '3KM5      5 -2     5     2 -2 -3-14   1 6319    '
        l(175) = '2(MN)S6   6-10  2  8     8 -8         4 6       '
        l(176) = '3MNS6     6 -9  1  8     8 -8         4 6       '
        l(177) = '4MK6      6 -8     6     8 -8   +11   4 6120    '
        l(178) = '2NM6      6 -8  2  6     6 -6         3 6       '
        l(179) = '4MS6      6 -8     8     8 -8         4 6       '
        l(180) = '2MSNK6    6 -7  1  4     6 -6   +11   3 6120    '
        l(181) = '2MN6      6 -7  1  6     6 -6         3 6       '
        l(182) = '2MNU6     6 -7 -1  8     6 -6         3 6       '
        l(183) = '3MSK6     6 -6     4     6 -6   +11   3 6120    '
        l(184) = 'M6        6 -6     6     6 -6         3 6       '
        l(185) = 'MSN6      6 -5  1  4     4 -4         2 6       '
        l(186) = 'MNK6      6 -5  1  6     4 -4   -11   2 6120    '
        l(187) = '4MN6      6 -5 -1  6     6 -6         5 6       '
        l(188) = 'MKNU6     6 -5 -1  8     4 -4   -11   2 6120    '
        l(189) = '2(MS)K6   6 -4     2     4 -4   +11   2 6120    '
        l(190) = '2MS6      6 -4     4     4 -4         2 6       '
        l(191) = '2MK6      6 -4     6     4 -4   -11   2 6120    '
        l(192) = '2SN6      6 -3  1  2     2 -2         1 6       '
        l(193) = '3MSN6     6 -3 -1  4     4 -4         4 6       '
        l(194) = 'MKL6      6 -3 -1  6     4 -4  2-11-131 6120122 '
        l(195) = '2SM6      6 -2     2     2 -2         1 6       '
        l(196) = 'MSK6      6 -2     4     2 -2   -11   1 6120    '
        l(197) = 'S6        6                                     '
        l(198) = '2MNO7     7 -9  1  7     8 -7  1      1 33 6    '
        l(199) = '2NMK7     7 -8  2  7     6 -6 -1-10   3 6119    '
        l(200) = 'M7        7 -7  1  7     6 -7 -1-12   3 6121    '
        l(201) = '2MSO7     7 -6     5     6 -5  1      1 32 6    '
        l(202) = 'MSKO7     7 -4     5     4 -3  1-11   1 31 6120 '
        l(203) = '2(MN)8    8-10  2  8     8 -8         4 6       '
        l(204) = '3MN8      8 -9  1  8     8 -8         4 6       '
        l(205) = '3MNKS8    8 -9  1 10     8 -8   -11   4 6120    '
        l(206) = 'M8        8 -8     8     8 -8         4 6       '
        l(207) = '2MSN8     8 -7  1  6     6 -6         3 6       '
        l(208) = '2MNK8     8 -7  1  8     6 -6   -11   3 6120    '
        l(209) = '3MS8      8 -6     6     6 -6         3 6       '
        l(210) = '3MK8      8 -6     8     6 -6   -11   3 6120    '
        l(211) = '2SNM8     8 -5  1  4     4 -4         2 6       '
        l(212) = 'MSNK8     8 -5  1  6     4 -4   -11   2 6120    '
        l(213) = '2(MS)8    8 -4     4     4 -4         2 6       '
        l(214) = '2MSK8     8 -4     6     4 -4   -11   2 6120    '
        l(215) = '3SM8      8 -2     2     2 -2         1 6       '
        l(216) = '2SMK8     8 -2     4     2 -2   -11   1 6120    '
        l(217) = 'S8        8                                     '
        l(218) = '2(MN)K9   9-10  2  9     8 -8 -1-10   4 6119    '
        l(219) = '3MNK9     9 -9  1  9     8 -8 -1-10   4 6119    '
        l(220) = '4MK9      9 -8     9     8 -8 -1-10   4 6119    '
        l(221) = '3MSK9     9 -6     7     6 -6 -1-10   3 6119    '
        l(222) = '4MN10    10-11  1 10    10-10         5 6       '
        l(223) = 'M10      10-10    10    10-10         5 6       '
        l(224) = '3MSN10   10 -9  1  8     8 -8         4 6       '
        l(225) = '4MS10    10 -8     8     8 -8         4 6       '
        l(226) = '2(MS)N10 10 -7  1  6     6 -6         3 6       '
        l(227) = '2MNSK10  10 -7  1  8     6 -6   -11   3 6120    '
        l(228) = '3M2S10   10 -6     6     6 -6         3 6       '
        l(229) = '4MSK11   11 -8     9     8 -8 -1-10   4 6119    '
        l(230) = 'M12      12-12    12    12-12         6 6       '
        l(231) = '4MSN12   12-11  1 10    10-10         5 6       '
        l(232) = '5MS12    12-10    10    10-10         5 6       '
        l(233) = '3MNKS12  12 -9  1 10     8 -8   -11   4 6120    '
        l(234) = '4M2S12   12 -8     8     8 -8         4 6       '
    end subroutine kompbs

    subroutine bewvuf(ierrs, kcmp, mxkc, inaam, knaam, &
            & jnaam, w, v0u, fr, v, &
            & f)
        use m_waq_precision
        !
        implicit none
        !
        ! Global variables
        !
        integer :: ierrs !!  Number of error messages
        integer, intent(in) :: kcmp
        integer, intent(in) :: mxkc
        integer, dimension(mxkc * 16), intent(in) :: jnaam !!  Help var.
        character(8), intent(in) :: inaam !!  Name of the referenced components
        character(8), dimension(mxkc), intent(in) :: knaam !!  Names of all components
        double precision, dimension(15), intent(in) :: v     !!  Help var. to calculate V0U()
        double precision, dimension(25), intent(in) :: f     !!  Help var. to calculate FR()
        double precision, dimension(kcmp) :: fr    !!  Amplitude factors for the referenced
        !!  components
        double precision, dimension(kcmp) :: v0u   !!  Astronomical arguments of the
        !!  referenced components
        double precision, dimension(kcmp) :: w     !!  Angular velocity of the referenced
        !!  components
        !
        ! Local variables
        !
        integer :: ia1
        integer :: ia2
        integer :: iar
        integer :: ie1
        integer :: ie2
        integer :: iex
        integer :: ikomp
        integer :: j
        integer :: kw
        integer :: kx
        integer :: mh
        integer :: mp
        integer :: mp1
        integer :: ms
        integer :: mt
        double precision :: dhalf   ! Value for 0.5 in SIGN function
        double precision :: pix2
        double precision :: s1
        double precision :: s2
        !
        !! executable statements -------------------------------------------------------
        !
        pix2 = 8d0 * atan(1d0)
        dhalf = 0.5d0
        !
        ! loop over given components
        !
        do ikomp = 1, kcmp
            !
            ! loop over the elements of kompbes
            !
            do j = 1, mxkc
                !
                ! test on name of present component
                !
                if (inaam==knaam(j)) then
                    !
                    ! compute angular velocity
                    !
                    mt = jnaam(16 * j - 15)
                    ms = jnaam(16 * j - 14)
                    mp = jnaam(16 * j - 13)
                    mh = jnaam(16 * j - 12)
                    mp1 = jnaam(16 * j - 11)
                    w(ikomp) = mt * 15d0 + ms * 0.54901653d0 + mp * 0.0046418333d0 + &
                            & mh * 0.04106864d0 + mp1 * 0.0000019610393d0
                    w(ikomp) = (w(ikomp) * pix2) / 360d0
                    !
                    ! compute v0+u
                    !
                    v0u(ikomp) = (jnaam(16 * j - 8) * pix2) / 4d0
                    do kw = 1, 7
                        kx = 16 * j - 16 + kw
                        v0u(ikomp) = v0u(ikomp) + v(kw) * jnaam(kx)
                    enddo
                    ie1 = jnaam(16 * j - 7)
                    if (ie1/=0) then
                        ia1 = abs(ie1)
                        s1 = real(ie1 / ia1, dp)
                        v0u(ikomp) = v0u(ikomp) + s1 * v(ia1)
                        ie2 = jnaam(16 * j - 6)
                        if (ie2/=0) then
                            ia2 = abs(ie2)
                            s2 = real(ie2 / ia2, dp)
                            v0u(ikomp) = v0u(ikomp) + s2 * v(ia2)
                        endif
                    endif
                    v0u(ikomp) = mod(v0u(ikomp), pix2)                                 &
                            & - pix2 * (sign(dhalf, v0u(ikomp)) - dhalf)
                    !
                    ! compute f
                    !
                    fr(ikomp) = 1d0
                    iex = jnaam(16 * j - 5)
                    if (iex/=0) then
                        iar = jnaam(16 * j - 4)
                        fr(ikomp) = (f(iar))**iex
                        iex = jnaam(16 * j - 3)
                        if (iex/=0) then
                            iar = jnaam(16 * j - 2)
                            fr(ikomp) = fr(ikomp) * (f(iar))**iex
                            iex = jnaam(16 * j - 1)
                            if (iex/=0) then
                                iar = jnaam(16 * j)
                                fr(ikomp) = fr(ikomp) * (f(iar))**iex
                            endif
                        endif
                    endif
                    exit
                endif
                if (j>=mxkc) then
                    ierrs = ierrs + 1
                    write (*, '(a,a,a)') '*** ERROR Component ', inaam, &
                            & ' not in internal component base'
                    exit
                endif
            enddo
        enddo
    end subroutine bewvuf


end module m_waq_timespace_data


Module m_waq_arcuv                       !plotbuitenbeentje
    implicit none
    double precision, allocatable :: arcuv(:, :, :)
End module m_waq_arcuv


module m_waq_timespace_triangle

    use m_waq_precision
    use m_waq_timespace_data
    use m_alloc

    implicit none

    integer :: nsold   ! nr of samples in previous triangulation
    integer :: numtri
    integer, allocatable, dimension(:, :) :: indx
    double precision, allocatable, dimension(:) :: xcent
    double precision, allocatable, dimension(:) :: ycent

    interface triint
        module procedure triint_z1D
        module procedure triint_z2D
        module procedure triint_z3D
    end interface triint

    interface get_extend
        module procedure get_extend1D
        module procedure get_extend2D
    end interface get_extend

    interface find_nearest
        module procedure find_nearest1D
        module procedure find_nearest2D
        module procedure find_nearest1D_missing_value
        module procedure find_nearest2D_missing_value
    end interface find_nearest


contains




    ! This subroutine interpolates one unstructured dataset xss, yss, zss, kcss, nss to another x, y, z, kcs, num_cells_u_dir
    ! It is the only one in this module that is of practical interest to the meteo module.
    ! The rest of the subroutines in this module are assisting this one.
    ! JDLA = 1 (re)triangulates

    subroutine triint_z2D(xss, yss, zss, kcsss, nss, &
            x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

        implicit none


        ! Global variables
        integer, intent(in) :: nss      ! Dimension of samples
        double precision, dimension(:), intent(in) :: xss      ! samples
        double precision, dimension(:), intent(in) :: yss
        double precision, dimension(:), intent(in) :: zss      ! dimension: nss*kx
        integer, dimension(:), intent(in) :: kcsss    ! samples mask

        integer, intent(in) :: mnx       ! Dimension of grid
        integer, intent(in) :: kx       ! vectormax
        double precision, dimension(:), intent(in) :: x        ! grid
        double precision, dimension(:), intent(in) :: y
        double precision, dimension(:, :), intent(out) :: z        ! dimension: num_cells_u_dir*kx
        integer, dimension(:), intent(in) :: kcs      ! grid mask
        integer, intent(in) :: jdla     ! refresh delauney yes /no

        integer, optional :: indxn(:, :) ! if present get weightfactors and indices
        double precision, optional :: wfn  (:, :)

        call triint_z1D(xss, yss, zss, kcsss, nss, &
                x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

    end subroutine triint_z2D

    subroutine triint_z3D(xss, yss, zss, kcsss, nss, &
            x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

        implicit none


        ! Global variables
        integer, intent(in) :: nss      ! Dimension of samples
        double precision, dimension(:), intent(in) :: xss      ! samples
        double precision, dimension(:), intent(in) :: yss
        double precision, dimension(:), intent(in) :: zss      ! dimension: nss*kx
        integer, dimension(:), intent(in) :: kcsss    ! samples mask

        integer, intent(in) :: mnx       ! Dimension of grid
        integer, intent(in) :: kx       ! vectormax
        double precision, dimension(:), intent(in) :: x        ! grid
        double precision, dimension(:), intent(in) :: y
        double precision, dimension(:, :, :), intent(out) :: z        ! dimension: num_cells_u_dir*kx
        integer, dimension(:), intent(in) :: kcs      ! grid mask
        integer, intent(in) :: jdla     ! refresh delauney yes /no

        integer, optional :: indxn(:, :) ! if present get weightfactors and indices
        double precision, optional :: wfn  (:, :)

        call triint_z1D(xss, yss, zss, kcsss, nss, &
                x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

    end subroutine triint_z3D


    subroutine triint_z1D(xss, yss, zss, kcsss, nss, &
            x, y, z, kcs, kx, mnx, jdla, indxn, wfn)

        implicit none


        ! Global variables
        integer, intent(in) :: nss      ! Dimension of samples
        double precision, dimension(:), intent(in) :: xss      ! samples
        double precision, dimension(:), intent(in) :: yss
        double precision, dimension(:), intent(in) :: zss      ! dimension: nss*kx
        integer, dimension(:), intent(in) :: kcsss    ! samples mask

        integer, intent(in) :: mnx      ! Dimension of grid
        integer, intent(in) :: kx       ! vectormax
        double precision, dimension(:), intent(in) :: x        ! grid
        double precision, dimension(:), intent(in) :: y
        double precision, dimension(kx * mnx), intent(out) :: z        ! dimension: mnx*kx
        integer, dimension(:), intent(in) :: kcs      ! grid mask
        integer, intent(in) :: jdla     ! refresh delauney yes /no

        integer, optional :: indxn(:, :) ! if present get weightfactors and indices
        double precision, optional :: wfn  (:, :)

        ! Local variables

        double precision, dimension(8) :: x_set
        double precision, dimension(8) :: y_set
        integer, dimension(8) :: kcs_set = 1
        double precision, dimension(4) :: x_extr
        double precision, dimension(4) :: y_extr
        double precision, dimension(4) :: z_extr
        double precision, dimension(3) :: zp
        integer, dimension(3) :: indxp

        double precision, dimension(:), allocatable :: xs
        double precision, dimension(:), allocatable :: ys
        double precision, dimension(:), allocatable :: zs
        integer, dimension(:), allocatable :: kcss
        integer :: ns
        integer :: k, n, jgetw ! , MOUT

        logical :: extra = .false. ! nu even niet

        !! executable statements -------------------------------------------------------
        !
        !     JDLA=1, DO DE LAUNEY
        !     JSLO=1, ALSO SLOPES RD4

        if (nss<1) then
            return
        endif

        call realloc(xs, nss + 8, 1)
        call realloc(ys, nss + 8, 1)
        call realloc(zs, nss + 8, 1)
        call realloc(kcss, nss + 8, 1)

        ns = 0
        do k = 1, nss
            if (kcsss(k) == 1) then
                ns = ns + 1
                xs(ns) = xss(k)
                ys(ns) = yss(k)
                do n = 1, kx
                    zs(kx * (ns - 1) + n) = zss(kx * (k - 1) + n)
                enddo
                kcss(ns) = 1
            endif
        enddo

        if (extra) then
            call get_extend(mnx, x, y, kcs, x_set(1:4), y_set(1:4))
            call get_extend(ns, xs, ys, kcss, x_set(5:8), y_set(5:8))
            call get_extend(8, x_set, y_set, kcs_set, x_extr, y_extr)

            call extrapolate(ns, xs, ys, zs, kcss, 4, x_extr, y_extr, z_extr)

            xs(ns + 1:ns + 4) = x_extr
            ys(ns + 1:ns + 4) = y_extr
            zs(ns + 1:ns + 4) = z_extr

            ns = ns + 4
        endif

        if (jdla==1) then
            call dlauny(xs, ys, ns)
        endif

        jgetw = 0                                            ! niets met gewichten, doe interpolatie
        if (present(indxn) .and. jdla == 1) jgetw = 1     ! haal gewichten       doe interpolatie , gebruik gewichten
        if (present(indxn) .and. jdla == 0) jgetw = 2     !                      doe interpolatie , gebruik gewichten

        do n = 1, mnx
            if (kcs(n) == 1) then
                if (jgetw <= 1) then
                    call findtri_indices_weights (x(n), y(n), xs, ys, ns, zp, indxp)     ! zoeken bij 0 en 1
                endif

                if (jgetw == 1) then                                                   ! zetten bij 1
                    do k = 1, 3
                        indxn(k, n) = indxp(k)
                        wfn(k, n) = zp(k)
                    enddo
                else if (jgetw == 2) then                                              ! halen bij 2, je hoeft niet te zoeken
                    do k = 1, 3
                        indxp(k) = indxn(k, n)
                        zp(k) = wfn(k, n)
                    enddo
                endif

                ! en altijd interpoleren

                do k = 1, kx                                                              ! over vectormax loop
                    if (indxp(1)==0 .or. indxp(2)==0 .or. indxp(3)==0) then
                        !  z(mnx*(k-1) + n) = -999
                    else
                        z(mnx * (k - 1) + n) = zp(1) * zs(kx * (indxp(1) - 1) + k) + zp(2) * zs(kx * (indxp(2) - 1) + k) + zp(3) * zs(kx * (indxp(3) - 1) + k)
                    endif
                enddo

            endif
        enddo

        deallocate(xs)
        deallocate(ys)
        deallocate(zs)
        deallocate(kcss)

    end subroutine triint_z1D


    subroutine dlauny(x, y, ns)


        ! implicit none
        !
        ! Local parameters
        !
        integer, parameter :: nh = 1
        ! COMMON variables
        !
        !
        ! Global variables
        !
        integer :: ns
        double precision, dimension(ns + 4) :: x
        double precision, dimension(ns + 4) :: y
        !
        !
        ! Local variables
        !
        integer :: i
        integer :: i1
        integer :: i2
        integer :: i3
        integer :: ie
        integer :: ierr
        integer :: in
        integer :: inew
        integer :: inewe
        integer :: interval
        integer :: j
        integer :: je
        integer :: k
        integer :: l
        integer :: match
        integer :: maxtri
        integer :: nart
        integer :: ndel
        integer :: newel
        integer :: nn
        integer :: nsm
        double precision :: cx
        double precision :: cy
        double precision :: den
        double precision :: dx
        double precision :: dxy
        double precision :: dy
        double precision :: r2
        double precision :: rn2
        double precision :: x2
        double precision :: x3
        double precision :: xl
        double precision :: xmax
        double precision :: xmin
        double precision :: xn1
        double precision :: xr
        double precision :: y2
        double precision :: y3
        double precision :: yl
        double precision :: ymax
        double precision :: ymin
        double precision :: yr
        double precision :: z
        double precision :: zero
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !
        !     ******************************************************************
        !     *                                                                *
        !     * PERFORMS A DELAUNAY TRIANGULARISATION OF A REGION GIVEN A SET  *
        !     * OF MESH POINTS.                                                *
        !     *   X,Y    :- 1D ARRAYS HOLDING COORDINATES OF MESH POINTS.      *
        !     *             DIMENS2ONED AT LEAST NS+4.                      *
        !     *   NS  :- NUMBER OF MESH POINTS.                             *
        !     *   INDX :- INTEGER ARRAY, DIMENS2ONED 3*nsmax,  WHICH ON EXIT*
        !     *             CONTAINS THE INDEX OF GLOBAL NS ASSOCIATED WITH *
        !     *             EACH ELEMENT.                                      *
        !     *   X,YCENT :- ARRAY GIVING CO-ORDINATES OF ELEMENT CIRCUMCENTRES
        !
        !     *   NUMTRI :- ON EXIT CONTAINS THE NUMBER OF ELEMENTS IN THE     *
        !     *             TRIANGULARISATION.                                 *
        !     *                                                                *
        !     *   N.B.  A NON-DELAUNAY TRIANGULATION MAY OCCUR IF ROUNDING     *
        !     *         ERRORS CAUSE A WRONG DECISION IN THE TEST FOR NEW      *
        !     *         POINT BEING INS2DE CIRCUMCIRCLE                        *
        !     *                                                                *
        !     *                                   P.K.SWEBY                    *
        !     *                                   ADAPTED FOR AMDAHL BY        *
        !     *                                   J.J.BARLEY (17/5/88)         *
        !     ******************************************************************
        !

        if (ns<3) then
            return
        endif
        maxtri = 0
        numtri = 0
        nsm = 3 * ns + 20
        !
        if (size(xcent) < nsm) then
            if (allocated (xcent)) deallocate (xcent, ycent, indx)
            allocate (xcent(nsm), ycent(nsm), indx(3, nsm), stat = ierr)
        endif
        !
        zero = 0.01d0

        !
        !     CALCULATE ARTIFICIAL NS NS+I I=1,2,3,4 AND CONSTRUCT FIRST
        !     TWO (ARTIFICIAL) ELEMENTS.
        !
        xmin = x(1)
        xmax = x(1)
        ymin = y(1)
        ymax = y(1)
        do i = 2, ns
            xmin = min(xmin, x(i))
            xmax = max(xmax, x(i))
            ymin = min(ymin, y(i))
            ymax = max(ymax, y(i))
        enddo
        !
        dx = xmax - xmin
        dy = ymax - ymin
        dxy = 0.1d0 * max(dx, dy)
        zero = 10 * dxy * 1.0D-9
        xl = xmin - 4d0 * dx - dxy
        xr = xmax + 4d0 * dx + dxy
        yl = ymin - 4d0 * dy - dxy
        yr = ymax + 4d0 * dy + dxy
        x(ns + 1) = xl
        y(ns + 1) = yl
        x(ns + 2) = xl
        y(ns + 2) = yr
        x(ns + 3) = xr
        y(ns + 3) = yr
        x(ns + 4) = xr
        y(ns + 4) = yl
        indx(1, 1) = ns + 1
        indx(2, 1) = ns + 2
        indx(3, 1) = ns + 3
        indx(1, 2) = ns + 3
        indx(2, 2) = ns + 4
        indx(3, 2) = ns + 1
        numtri = 2
        !
        do ie = 1, 2
            i1 = indx(1, ie)
            i2 = indx(2, ie)
            i3 = indx(3, ie)
            x2 = x(i2) - x(i1)
            x3 = x(i3) - x(i1)
            y2 = y(i2) - y(i1)
            y3 = y(i3) - y(i1)
            den = y2 * x3 - y3 * x2
            if (den/=0) then
                z = (x2 * (x2 - x3) + y2 * (y2 - y3)) / den
            else
            endif
            xcent(ie) = 0.5D0 * (x3 - z * y3)
            ycent(ie) = 0.5D0 * (y3 + z * x3)
        enddo
        !
        interval = max(1, ns / 100)
        do in = 1, ns

            !
            !     ADD ONE MESH POINT AT A TIME AND REMESH LOCALLY IF NECESSARY
            !
            ndel = 0
            newel = 0
            do ie = 1, numtri
                !
                !     IS POINT IN INS2DED CIRCUMCIRCLE OF ELEMENT IE ?
                !
                i1 = indx(1, ie)
                i2 = indx(2, ie)
                i3 = indx(3, ie)
                cx = xcent(ie)
                cy = ycent(ie)
                r2 = cx**2 + cy**2
                xn1 = x(in) - x(i1)
                rn2 = (xn1 - cx)**2 + (y(in) - y(i1) - cy)**2
                !
                if (rn2>r2) then
                    cycle
                endif
                !
                !     YES IT IS INS2DE,CREATE NEW ELEMENTS AND MARK OLD FOR DELETION.
                !
                !
                do j = 1, 3
                    do k = 1, 3
                        indx(k, numtri + newel + j) = indx(k, ie)
                    enddo
                    maxtri = max(maxtri, numtri + newel + 3)
                    if (maxtri>nsm) then
                        write(*, *)'maxtri>nsm'
                        return
                    endif
                    indx(j, numtri + newel + j) = in
                enddo
                do inew = 1, 3
                    inewe = numtri + newel + inew
                    maxtri = max(maxtri, inewe)
                    if (maxtri>nsm) then
                        write(*, *)'maxtri>nsm'
                        return
                    endif
                    i1 = indx(1, inewe)
                    i2 = indx(2, inewe)
                    i3 = indx(3, inewe)
                    x2 = x(i2) - x(i1)
                    x3 = x(i3) - x(i1)
                    y2 = y(i2) - y(i1)
                    y3 = y(i3) - y(i1)
                    if (abs(y2 * x3 - y3 * x2)>zero) then
                        z = (x2 * (x2 - x3) + y2 * (y2 - y3)) / (y2 * x3 - y3 * x2)
                        cx = 0.5D0 * (x3 - z * y3)
                        cy = 0.5D0 * (y3 + z * x3)
                    else
                        cx = 0.5D0 * (x3 - x2)
                        cy = 0.5D0 * (y3 - y2)
                    endif
                    xcent(inewe) = cx
                    ycent(inewe) = cy
                enddo
                newel = newel + 3
                indx(1, ie) = 0
                ndel = ndel + 1
                !
            enddo
            !
            !     IF IN WAS INS2DE CIRCUMCIRCLE OF MORE THAN 1 ELEMENT THEN WILL
            !     HAVE CREATED 2 IDENTICAL NEW ELEMENTS: DELETE THEM BOTH.
            !
            if (ndel>1) then
                do ie = numtri + 1, numtri + newel - 1
                    do je = ie + 1, numtri + newel
                        match = 0
                        do k = 1, 3
                            do l = 1, 3
                                if (indx(k, ie)==indx(l, je)) match = match + 1
                            enddo
                        enddo
                        if (match==3) then
                            indx(1, ie) = 0
                            indx(1, je) = 0
                            ndel = ndel + 2
                        endif
                    enddo
                enddo
            endif
            !
            !     DELETE ANY ELEMENTS
            !
            !
            nn = numtri + newel
            ie = 1
            70  continue
            if (indx(1, ie)==0) then
                do j = ie, nn - 1
                    xcent(j) = xcent(j + 1)
                    ycent(j) = ycent(j + 1)
                    do k = 1, 3
                        indx(k, j) = indx(k, j + 1)
                    enddo
                enddo
                nn = nn - 1
                ie = ie - 1
            endif
            ie = ie + 1
            if (ie<=nn) goto 70
            numtri = nn
            !
        enddo
        !
        !
        !     FINALLY REMOVE ELEMENTS CONTAINING ARTIFICIAL NS
        !
        !
        !
        ie = 1
        100 continue
        nart = 0
        do l = 1, 3
            if (indx(l, ie)>ns) nart = nart + 1
        enddo
        if (nart>0) then
            do j = ie, nn - 1
                xcent(j) = xcent(j + 1)
                ycent(j) = ycent(j + 1)
                do k = 1, 3
                    indx(k, j) = indx(k, j + 1)
                enddo
            enddo
            numtri = numtri - 1
            ie = ie - 1
        endif
        ie = ie + 1
        if (ie<=numtri) goto 100
        !
    end subroutine dlauny

    subroutine findtri_indices_weights(xp, yp, xs, ys, ns, zp, indxp)

        implicit none


        ! Global variables
        double precision, intent(in) :: xp    ! for this point
        double precision, intent(in) :: yp

        integer, intent(in) :: ns
        double precision, dimension(ns), intent(in) :: xs    ! on this set
        double precision, dimension(ns), intent(in) :: ys

        integer, dimension(3), intent(out) :: indxp ! find indices to set
        double precision, dimension(3), intent(out) :: zp    ! and corresponding weightfactors


        ! Local variables
        integer :: k
        integer :: k1
        integer :: k2, n3
        integer :: intri
        integer :: nroldfind, nrfind
        double precision :: xtmax
        double precision :: xtmin
        double precision :: ytmax
        double precision :: ytmin
        double precision, dimension(3) :: xt
        double precision, dimension(3) :: yt
        !
        !
        data nroldfind/0/
        !
        !! executable statements -------------------------------------------------------
        !
        !
        indxp = 0
        n3 = 3

        5 continue
        if (nroldfind/=0) then
            k1 = max(1, nroldfind - 200)
            k2 = min(numtri, nroldfind + 200)
        else
            k1 = 1
            k2 = numtri
        endif
        !
        do k = k1, k2

            xt(1) = xs(indx(1, k))
            xt(2) = xs(indx(2, k))
            xt(3) = xs(indx(3, k))
            yt(1) = ys(indx(1, k))
            yt(2) = ys(indx(2, k))
            yt(3) = ys(indx(3, k))
            xtmax = max(xt(1), max(xt(2), xt(3)))
            ytmax = max(yt(1), max(yt(2), yt(3)))
            xtmin = min(xt(1), min(xt(2), xt(3)))
            ytmin = min(yt(1), min(yt(2), yt(3)))
            if (xp>=xtmin .and. xp<=xtmax .and. yp>=ytmin .and. yp<=ytmax) then
                call pinpok(xp, yp, n3, xt, yt, intri)
                if (intri==1) then
                    nrfind = k
                    nroldfind = nrfind
                    indxp(1) = indx(1, k)
                    indxp(2) = indx(2, k)
                    indxp(3) = indx(3, k)
                    call linweight(xt, yt, xp, yp, zp)
                    ! write(*,*) xp, yp, k, indxp(1), indxp(2), indxp(3)
                    return
                endif
            endif
        enddo
        if (nroldfind/=0) then
            nroldfind = 0
            goto 5
        endif
    end subroutine findtri_indices_weights

    subroutine linweight(xt, yt, xp, yp, zp)

        double precision, intent(in) :: xp    ! for this point
        double precision, intent(in) :: yp

        double precision, dimension(3) :: xt    ! in this triangle
        double precision, dimension(3) :: yt

        double precision, dimension(3), intent(out) :: zp    ! the weightfactors are...

        double precision :: a11, a12, a21, a22, b1, b2, det

        zp = 0
        a11 = xt(2) - xt(1)
        a21 = yt(2) - yt(1)
        a12 = xt(3) - xt(1)
        a22 = yt(3) - yt(1)
        b1 = xp - xt(1)
        b2 = yp - yt(1)

        det = a11 * a22 - a12 * a21
        if (abs(det)<1E-9) then
            return
        endif
        !
        zp(2) = (a22 * b1 - a12 * b2) / det
        zp(3) = (-a21 * b1 + a11 * b2) / det
        zp(1) = 1d0 - zp(2) - zp(3)

    end subroutine linweight


    subroutine get_extend2D(n, m, x, y, kcs, x_ext, y_ext)

        double precision, dimension(:, :) :: x
        double precision, dimension(:, :) :: y
        integer, dimension(:, :) :: kcs
        integer :: n
        integer :: m
        double precision, dimension(:) :: x_ext
        double precision, dimension(:) :: y_ext

        call get_extend1D(n * m, x, y, kcs, x_ext, y_ext)

    end subroutine get_extend2D

    subroutine get_extend1D(n, x, y, kcs, x_ext, y_ext)

        integer :: n
        double precision, dimension(n) :: x
        double precision, dimension(n) :: y
        integer, dimension(n) :: kcs
        double precision, dimension(4) :: x_ext
        double precision, dimension(4) :: y_ext
        double precision :: x_min
        double precision :: x_max
        double precision :: x_dist
        double precision :: y_min
        double precision :: y_max
        double precision :: y_dist
        integer :: index

        x_min = 1E30
        x_max = -1E30
        y_min = 1E30
        y_max = -1E30

        do index = 1, n
            if(kcs(index) == 1) then
                if(x_min > x(index)) then
                    x_min = x(index)
                endif
                if(x_max < x(index)) then
                    x_max = x(index)
                endif
                if(y_min > y(index)) then
                    y_min = y(index)
                endif
                if(y_max < y(index)) then
                    y_max = y(index)
                endif
            endif
        enddo

        x_dist = x_max - x_min
        y_dist = y_max - y_min
        x_min = x_min - 0.01d0 * x_dist
        x_max = x_max + 0.01d0 * x_dist
        y_min = y_min - 0.01d0 * y_dist
        y_max = y_max + 0.01d0 * y_dist

        x_ext(1) = x_min
        y_ext(1) = y_min
        x_ext(2) = x_min
        y_ext(2) = y_max
        x_ext(3) = x_max
        y_ext(3) = y_max
        x_ext(4) = x_max
        y_ext(4) = y_min

    end subroutine get_extend1D

    subroutine extrapolate(n, x, y, z, kcs, n_extr, x_extr, y_extr, z_extr)

        integer :: n
        double precision, dimension(n) :: x
        double precision, dimension(n) :: y
        double precision, dimension(n) :: z
        integer, dimension(n) :: kcs
        integer :: n_extr
        double precision, dimension(n_extr), target :: x_extr
        double precision, dimension(n_extr), target :: y_extr
        double precision, dimension(n_extr), target :: z_extr
        integer :: i_extr
        integer :: i_min
        double precision, pointer :: x_a
        double precision, pointer :: y_a
        double precision, pointer :: z_a
        double precision :: dist_min

        dist_min = 1E30
        i_min = 0

        do i_extr = 1, n_extr
            x_a => x_extr(i_extr)
            y_a => y_extr(i_extr)
            z_a => z_extr(i_extr)
            call find_nearest(n, x, y, z, kcs, x_a, y_a, i_min, dist_min)
            z_a = z(i_min)
        enddo

    end subroutine extrapolate


    subroutine find_nearest2D(n, m, x, y, kcs, x_a, y_a, n_min, m_min, dist_min)

        use m_waq_precision

        integer :: n
        integer :: m
        double precision, dimension(:, :) :: x
        double precision, dimension(:, :) :: y
        integer, dimension(:, :) :: kcs
        integer :: n_min
        integer :: m_min
        integer :: i_min
        double precision :: x_a
        double precision :: y_a
        double precision :: dist_min

        call find_nearest1D(n * m, x, y, kcs, x_a, y_a, i_min, dist_min)

        m_min = i_min / n
        n_min = i_min - (m_min * n)
        m_min = m_min + 1

    end subroutine find_nearest2D

    subroutine find_nearest2D_missing_value(n, m, x, y, z, kcs, x_a, y_a, n_min, m_min, dist_min)

        use m_waq_precision

        integer :: n
        integer :: m
        double precision, dimension(:, :) :: x
        double precision, dimension(:, :) :: y
        double precision, dimension(:, :) :: z
        integer, dimension(:, :) :: kcs
        integer :: n_min
        integer :: m_min
        integer :: i_min
        double precision :: x_a
        double precision :: y_a
        double precision :: dist_min

        call find_nearest1D_missing_value(n * m, x, y, z, kcs, x_a, y_a, i_min, dist_min)

        m_min = i_min / n
        n_min = i_min - (m_min * n)
        m_min = m_min + 1

    end subroutine find_nearest2D_missing_value

    subroutine find_nearest1D(n, x, y, kcs, x_a, y_a, i_min, dist_min)

        use m_waq_precision

        integer :: n
        double precision, dimension(n) :: x
        double precision, dimension(n) :: y
        integer, dimension(n) :: kcs
        integer :: i
        integer :: i_min
        double precision :: x_a
        double precision :: y_a
        double precision :: dist
        double precision :: dist_min

        dist_min = 1E30
        i_min = 0

        do i = 1, n
            if(kcs(i) == 1) then
                dist = (x(i) - x_a)**2 + (y(i) - y_a)**2
                if(dist < dist_min) then
                    dist_min = dist
                    i_min = i
                endif
            endif
        enddo

        dist_min = sqrt(dist_min)

    end subroutine find_nearest1D

    subroutine find_nearest1D_missing_value(n, x, y, z, kcs, x_a, y_a, i_min, dist_min)

        use m_waq_precision

        integer :: n
        double precision, dimension(n) :: x
        double precision, dimension(n) :: y
        double precision, dimension(n) :: z
        integer, dimension(n) :: kcs
        integer :: i
        integer :: i_min
        double precision :: x_a
        double precision :: y_a
        double precision :: dist
        double precision :: dist_min

        dist_min = 1E30
        i_min = 0

        do i = 1, n
            if(kcs(i) == 1) then
                dist = (x(i) - x_a)**2 + (y(i) - y_a)**2
                if((dist < dist_min).and.(z(i)/=-999d0)) then
                    dist_min = dist
                    i_min = i
                endif
            endif
        enddo

        dist_min = sqrt(dist_min)

    end subroutine find_nearest1D_missing_value

    subroutine polyint(xs, ys, zs, kcs, ns, &   ! interpolate in a polyline like way
            x, y, z, kc, kx, mnx, jintp, xyen, indxn, wfn)

        implicit none


        ! Global variables
        integer, intent(in) :: ns       ! Dimension of polygon OR LINE BOUNDARY
        double precision, dimension(:), intent(in) :: xs       ! polygon
        double precision, dimension(:), intent(in) :: ys
        double precision, dimension(:), intent(in) :: zs       ! dimension: ns*kx
        integer, dimension(:), intent(in) :: kcs      ! polygon mask

        integer, intent(in) :: mnx      ! Dimension of grid
        integer, intent(in) :: kx       ! vectormax
        double precision, dimension(:), intent(in) :: x        ! grid
        double precision, dimension(:), intent(in) :: y
        double precision, dimension(kx * mnx), intent(out) :: z        ! dimension: mnx*kx
        integer, dimension(:), intent(in) :: kc       ! grid mask
        integer, intent(in) :: jintp    ! (Re-)interpolate if 1 (otherwise use index weights)

        double precision, dimension(:, :), intent(in) :: xyen     ! cellsize / tol
        integer, dimension(:, :), intent(inout), optional :: indxn   ! pli segment is identified by its first node nr.
        double precision, dimension(:, :), intent(inout), optional :: wfn     ! If present, get weight index and factor

        ! locals

        double precision :: rl
        integer :: m, k, k1, jgetw

        jgetw = 0                                            ! niets met gewichten, doe interpolatie
        if (present(indxn) .and. jintp == 1) jgetw = 1    ! haal gewichten       doe interpolatie , gebruik gewichten
        if (present(indxn) .and. jintp == 0) jgetw = 2    !                      doe interpolatie , gebruik gewichten

        do m = 1, mnx

            if (jgetw <= 1) then
                call polyindexweight(x(m), y(m), xyen(1, m), xyen(2, m), xs, ys, kcs, ns, k1, rl)    ! interpolate in a polyline like way
                if (jgetw == 1) then                                              ! zetten bij 1
                    indxn(1, m) = k1
                    wfn(1, m) = rl
                endif
            elseif (jgetw == 2) then                                              ! halen bij 2, je hoeft niet te zoeken
                k1 = indxn(1, m)
                rl = wfn(1, m)
            endif

            ! Now do the actual interpolation of data zs -> z
            if (k1 > 0) then
                do k = 1, kx
                    z(kx * (m - 1) + k) = (1 - rl) * zs(kx * (k1 - 1) + k) + rl * zs(kx * (k1) + k)
                enddo
            endif
        enddo

    end subroutine polyint


    !> Selects the index of the polyline segment that intersects with line e--en
    !! with the intersection closest to point e.
    !! The search range is thus from e to en, and not a distance rdis as before.
    !! The normal direction is now
    !! defined by e--en and not normal to the polyline. Also, *all* polyline
    !! segments are checked, not the closest based on dbdistance of pli points.
    subroutine polyindexweight(xe, ye, xen, yen, xs, ys, kcs, ns, k1, rl)

        ! Global variables
        integer, intent(in) :: ns       ! Dimension of polygon OR LINE BOUNDARY
        double precision, intent(in) :: xs(:)    ! polygon
        double precision, intent(in) :: ys(:)
        integer, intent(in) :: kcs(:)   ! polygon mask
        double precision, intent(out) :: rl
        double precision, intent(in) :: xe, ye   !
        double precision, intent(in) :: xen, yen ! in input uitstekers, on output SL and CRP
        integer, intent(out) :: k1

        integer :: k, km, JACROS
        double precision :: dism, dis1, dis2, rl1, rl2, dbdistance
        double precision :: SL, SM, smm, SLM, XCR, YCR, CRP, CRPM

        ! AvD todo: use kcs at all?
        dism = huge(dism)
        k1 = 0
        km = 0
        crpm = 0
        do k = 1, ns - 1
            call CROSS(xe, ye, xen, yen, xs(k), ys(k), xs(k + 1), ys(k + 1), JACROS, SL, SM, XCR, YCR, CRP)
            if (jacros == 1) then
                if (SL < dism) then
                    dism = SL
                    km = k
                    SMM = SM
                    SLM = SL
                    CRPM = CRP
                end if
            end if
        enddo
        if (km > 0) then
            k1 = km
            rl = SMM
        end if
    end subroutine polyindexweight


end module m_waq_timespace_triangle
! with leading dimensions 3 of 4

module m_waq_timespace
    use m_waq_precision

    use m_waq_timespace_data
    use m_waq_timespace_triangle
    implicit none

contains


    function addtimespacerelation(idom, qid, kx, x, y, kcs, filename, filetype, method, operand, xyen)  result(success)
        implicit none
        logical :: success

        ! this subroutine adds a relation in domainnr=threadnr idom (use 1 of no threads)
        ! between a requested quantity with id=qid, defined on elementset x,y,kcs

        ! arguments
        integer, intent(in) :: idom       ! threadnr = domainnr
        character(*), intent(in) :: qid        ! unique quantity identification
        integer, intent(in) :: kx         ! vectormax

        double precision, intent(in) :: x(:)       ! x of elset
        double precision, intent(in) :: y(:)       ! y of elset
        integer, intent(in) :: kcs(:)     ! kcs of elset

        character(*), intent(in) :: filename   ! file name for meteo data file
        integer, intent(in) :: filetype   ! spw, arcinfo, uniuvp etc
        integer, intent(in) :: method     ! time/space interpolation method
        character(1), intent(in) :: operand    ! file name for meteo data file

        double precision, intent(in), optional :: xyen(:, :)     ! distance tolerance / cellsize of elset

        ! locals
        integer :: mx
        integer :: numq       ! index number of this quantity
        integer :: nump       ! index number of this provider
        integer :: ielsetq    ! number of elset x,y,kcs of quantity

        type(tsubdom), pointer :: hsubdoms(:)

        if (size(kcs) < 1) return

        mx = size(subdoms)
        if (.not. allocated (subdoms)) mx = 0

        if (idom > mx) then                     ! todo: deepcopy maken voor echt gebruik
            if (mx > 0) then
                allocate (hsubdoms(mx))
                hsubdoms = subdoms
                deallocate (subdoms)
            endif
            allocate(subdoms(idom))
            allocate(subdoms(idom)%quantities(0))
            allocate(subdoms(idom)%dataproviders(0))
            allocate(subdoms(idom)%elementsets(0))
            if (mx > 0) then
                subdoms(1:mx) = hsubdoms(1:mx)
                deallocate(hsubdoms)
            endif
            pi = acos(-1.d0)
            d2r = pi / 180.d0
            r2d = 180.d0 / pi
        endif

        if (present(xyen)) then
            success = addquantity(idom, qid, kx, x, y, kcs, numq, ielsetq, xyen)
        else
            success = addquantity(idom, qid, kx, x, y, kcs, numq, ielsetq)
        endif

        success = addprovider(idom, qid, kx, filename, filetype, method, operand, nump, ielsetq)

    end function addtimespacerelation


    function updatetimespaceproviders(idom, qid, time) result(success) ! nb, doet enkel qid, niet allen
        implicit none
        logical success


        ! Globals
        double precision, intent(in) :: time
        integer, intent(in) :: idom
        character(len = *) :: qid

        ! Locals
        integer :: i, j, k
        integer :: numproviders

        type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
        type(telementset), pointer :: elementsets(:)   ! bevat elementsets van zowel vraag als aanbod
        type(tquantity), pointer :: quantities(:)    ! bevat de vraag
        type(tdataprovider), pointer :: dataprovider     ! bevat een aanbieder


        !if (time <= timelast) return
        !timelast = time

        dataproviders => subdoms(idom)%dataproviders
        elementsets => subdoms(idom)%elementsets

        if (subdoms(idom)%ini == 0) then  ! once for each domain once, assemble list of providers for *each* quantity
            subdoms(idom)%ini = 1
            quantities => subdoms(idom)%quantities
            do i = 1, size(quantities)
                do k = 1, 2
                    numproviders = 0
                    do j = 1, size(dataproviders)
                        if (quantities(i)%qid == dataproviders(j)%qid .and. dataproviders(j)%method /= justupdate) then
                            numproviders = numproviders + 1
                            if (k == 2) quantities(i)%providernrs(numproviders) = j
                        endif
                    enddo
                    if (k == 1) allocate(quantities(i)%providernrs(numproviders))
                enddo
            enddo
        endif

        do i = 1, size(subdoms(idom)%dataproviders)
            dataprovider => subdoms(idom)%dataproviders(i)
            if (qid == dataprovider%qid) then
                success = updateprovider(dataprovider, time, elementsets)
                if (.not. success) return
            endif
        enddo
    end function updatetimespaceproviders


    function updateprovider(dataprovider, tim, elementsets) result(success)
        implicit none
        logical :: success

        ! Update information of an item in the meteo module.
        ! Bij Uniuvp, ook direct transformatie



        ! globals
        double precision, intent(in) :: tim       ! flow time to update to
        type(tdataprovider), pointer :: dataprovider
        type(telementset), pointer :: elementsets(:)

        ! locals
        integer :: mx
        integer :: num_cells_u_dir
        integer :: kx
        integer, pointer :: minp
        integer :: it1
        integer :: filetype
        integer :: ielset
        double precision :: dmiss
        double precision :: tread
        double precision :: x0r
        double precision :: y0r
        double precision, dimension(:), pointer :: uz     ! 1-dim array
        double precision, dimension(:, :), pointer :: vz     ! 2-dim array
        double precision, dimension(:, :, :), pointer :: wz     ! 3-dim array

        ! only for triangulation
        integer :: m, mm, i0, i1, k, ns, ielsetq, jdla, jpoly
        double precision :: a0, a1, t0, t1, t1min, t0max, treadlast
        double precision, pointer :: u0(:)     ! 2-dim array
        double precision, pointer :: u1(:)
        double precision, pointer :: xs(:), ys(:) ! pointers naar elementset provider
        double precision, allocatable :: zs(:)       ! maar zs en kcss blijven lokaal
        integer, pointer :: kcss(:)

        double precision, pointer :: x(:), y(:)   ! pointers naar elementset v/d quantity
        integer, pointer :: kcs(:)      !
        double precision, pointer :: xyen(:, :)   ! pointer naar cell tolerance, aleen voor polyint
        double precision, pointer :: z(:, :, :)    ! pointer naar provider field, met dimension v/d quantity
        ! end only


        ! only for triangulation:
        integer, save :: ncheckprev   ! jdla 1 if ncheckprev .ne. ncheck todo: store this in provider
        integer :: ncheck

        it1 = dataprovider%it1

        do while (tim > dataprovider%field(it1)%time)

            tread = 0
            minp => dataprovider%minp ! pointer, so closed minp (=0) is set for provider directly.
            mx = dataprovider%mx
            num_cells_u_dir = dataprovider%num_cells_u_dir
            kx = dataprovider%kx
            dmiss = dataprovider%dmiss
            filetype = dataprovider%filetype
            ielset = dataprovider%field(it1)%ielset

            if (tim > dataprovider%field(it1)%time) then
                dataprovider%it0 = dataprovider%it1
                dataprovider%it1 = 1 - dataprovider%it1
                it1 = dataprovider%it1
                select case (filetype)

                case (uniform, unimagdir)

                    uz => dataprovider%field(it1)%arr1d
                    10 treadlast = tread
                    success = readseries(minp, uz, kx, tread)
                    if (.not. success) return

                    if (uz(1) == dmiss .and. dataprovider%field(1 - it1)%arr1d(1) == dmiss) then
                        goto 10  ! hooguit 1 missing in geheugen
                    endif

                case (fourier)

                    tread = dataprovider%field(1)%time  ! groot negatief bij aanvang
                    success = readfouriercompstim(minp, dataprovider%field(0)%arr2d, dataprovider%field(1)%arr2d, &
                            mx, num_cells_u_dir, kx, tim, tread)     ! field 0 holds comps, 1 holds result

                    dataprovider%field(0)%time = tread                   ! trick fourier into being update
                    dataprovider%field(1)%time = tread

                    if (.not. success) return

                case (svwp)

                    wz => dataprovider%field(it1)%arr3d
                    success = reaspv(minp, wz, mx, num_cells_u_dir, kx, tread)
                    if (.not. success) return

                case (arcinfo)

                    vz => dataprovider%field(it1)%arr2d
                    success = reaarctim(minp, vz, mx, num_cells_u_dir, tread, dmiss, .true.)

                    if (.not. success) return

                case (d3d_flow_arcinfo)

                    vz => dataprovider%field(it1)%arr2d
                    success = reaarctim(minp, vz, mx, num_cells_u_dir, tread, dmiss, .false.)

                    if (.not. success) return

                case (curvi)

                case (spiderweb)

                    wz => dataprovider%field(it1)%arr3d
                    success = reaspwtim(minp, wz, mx, num_cells_u_dir, tread, x0r, y0r)
                    if (.not. success) return
                    elementsets(ielset)%x(1) = x0r
                    elementsets(ielset)%y(1) = y0r

                case (multiple_uni)                            ! several uniform timeseries without interpolation

                    tread = tim
                    ielsetq = dataprovider%ielsetq
                    ns = size(elementsets(ielsetq)%x)         ! ielset vrager is de paraplu

                    wz => dataprovider%field(it1)%arr3d  ;   wz = dmiss

                    do m = 1, ns                                   ! in deze set zitten pointers naar punt providers
                        mm = elementsets(ielsetq)%kcs(m)
                        i0 = subdoms(idom)%dataproviders(mm)%it0
                        i1 = subdoms(idom)%dataproviders(mm)%it1
                        dmiss = subdoms(idom)%dataproviders(mm)%dmiss
                        u1 => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
                        u0 => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
                        if (u1(1) /= dmiss) then                           ! als rechts er is
                            t1 = subdoms(idom)%dataproviders(mm)%field(i1)%time
                            t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time
                            if (t1 == t0) then
                                a1 = 1d0
                            else
                                a1 = (tread - t0) / (t1 - t0)
                            endif
                            a0 = 1d0 - a1
                            do k = 1, kx
                                wz(m, 1, k) = a0 * u0(k) + a1 * u1(k)
                            enddo
                        endif
                    enddo

                    a0 = 2d0

                case (triangulationmagdir, poly_tim)

                    jpoly = 0
                    if (filetype == poly_tim) jpoly = 1           ! polyline interpolation for boundary conditions

                    ns = size(elementsets(ielset)%x) / 2          ! de data staat op dit providersfield, windstationnetjes of polyline
                    xs => elementsets(ielset)%x
                    ys => elementsets(ielset)%y
                    kcss => elementsets(ielset)%kcs               ! in deze set zitten pointers naar de punt providers

                    if (ns * kx >   size(zs)) then
                        if (allocated     (zs)) deallocate(zs)
                        call realloc(zs, kx * ns)
                    endif

                    t1min = 1d30 ; t0max = -1d30                  ! zoeken naar meest nabije punt in toekomst, t1min
                    do m = 1, ns
                        mm = kcss(m)                           ! in deze set zitten pointers naar de punt providers
                        i0 = subdoms(idom)%dataproviders(mm)%it0
                        i1 = subdoms(idom)%dataproviders(mm)%it1
                        ! dmiss =  subdoms(idom)%dataproviders(mm)%dmiss
                        ! u1    => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
                        ! u0    => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
                        t1 = subdoms(idom)%dataproviders(mm)%field(i1)%time
                        if (t1 < t1min) then
                            t1min = t1                            ! meest nabije tijdstip in toekomst
                        endif

                        if (dataprovider%field(1 - it1)%time == t01ini) then     ! only at first step also look for max of t0 times
                            t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time ! dus meest nabije punt verleden
                            if (t0 > t0max) then
                                t0max = t0
                            endif
                        endif
                    enddo
                    tread = t1min

                    ielsetq = dataprovider%ielsetq                    ! we interpoleren voor dit quantityfield
                    x => elementsets(ielsetq)%x
                    y => elementsets(ielsetq)%y
                    kcs => elementsets(ielsetq)%kcs

                    zs = 0 ; kcss(ns + 1:) = 0 ; ncheck = 0
                    do m = 1, ns                                      ! in deze set zitten pointers naar punt providers
                        mm = elementsets(ielset)%kcs(m)
                        if (subdoms(idom)%dataproviders(mm)%filetype == fourier) then ! no time interpolation for fourier,
                            !  u1 => subdoms(idom)%dataproviders(mm)%field(1 )%arr1d      ! just set zs, contained in field(1)
                            do k = 1, kx
                                zs(kx * (m - 1) + k) = subdoms(idom)%dataproviders(mm)%field(1)%arr1d(k)   ! just set zs, contained in field(1)
                            enddo
                            kcss(ns + m) = 1                                             ! fourier never gaps
                        else
                            i0 = subdoms(idom)%dataproviders(mm)%it0
                            i1 = subdoms(idom)%dataproviders(mm)%it1
                            dmiss = subdoms(idom)%dataproviders(mm)%dmiss
                            u1 => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
                            u0 => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
                            if (u1(1) /= dmiss) then                           ! als rechts er is
                                t1 = subdoms(idom)%dataproviders(mm)%field(i1)%time
                                if (u0(1) /= dmiss .or. t1min == t1) then      ! en links is er ook of de tijd is rechts
                                    t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time
                                    if (t1 == t0) then
                                        a1 = 1d0
                                    else
                                        a1 = (t1min - t0) / (t1 - t0)
                                    endif
                                    a0 = 1d0 - a1
                                    if (jpoly == 0) then                          ! special case of mag/dir interpolation
                                        k = 1
                                        call magdir2uv(u0, u1, a0, a1, zs(kx * (m - 1) + k))
                                        do k = 3, kx
                                            zs(kx * (m - 1) + k) = a0 * u0(k) + a1 * u1(k)
                                        enddo
                                    else                                          ! the rest is standard
                                        do k = 1, kx
                                            zs(kx * (m - 1) + k) = a0 * u0(k) + a1 * u1(k)
                                        enddo
                                    endif
                                    kcss(ns + m) = 1
                                    ncheck = ncheck + m * m                         ! check only for time series that may contain defaults
                                endif                                            ! check difference may cause re-interpolation
                            endif
                        endif
                    enddo

                    z => dataprovider%field(it1)%arr3d  ;   z = dmiss
                    if (jpoly == 0) then
                        jdla = 0
                        if (ncheck /= ncheckprev) jdla = 1 ;
                        ! triangulate only when
                        ncheckprev = ncheck

                        call triint  (xs, ys, zs, kcss(ns + 1:), ns, &
                                x, y, z, kcs, kx, mx * num_cells_u_dir, jdla)

                        if (mdia > 0) then
                            write(mdia, *) tim, tread, ncheck, jdla
                            write(mdia, *) (zs(2 * m - 1), m = 1, ns)
                        endif

                    else
                        xyen => elementsets(ielsetq)%xyen
                        if (dataprovider%method == weightfactors) then
                            jdla = dataprovider%refresh
                            call polyint (xs, ys, zs, kcss(ns + 1:), ns, &
                                    x, y, z, kcs, kx, mx, jdla, xyen, &
                                    dataprovider%indxn, dataprovider%wfn)
                            !hier niet mx*num_cells_u_dir, mx = elsetq
                            dataprovider%refresh = 0
                        else
                            jdla = 1
                            call polyint (xs, ys, zs, kcss(ns + 1:), ns, &
                                    x, y, z, kcs, kx, mx, jdla, xyen)
                            !hier niet mx*num_cells_u_dir, mx = elsetq
                        end if

                        if (mdia > 0) then
                            write(mdia, *) tim, tread, ns, ncheck, jpoly
                            write(mdia, '(300F6.2)') (zs(m), m = 1, ns)
                        endif

                    endif


                    ! onderstaande code om het veld ook eenmalig voor t = t0 in te vullen.
                    ! lijkt veel op hierboven, zou in lus kunnen, waarschijnlijk minder overzichtelijk => zo laten
                    ! index 1-it1 ipv it1, t0max ipv t1min
                    if (dataprovider%field(1 - it1)%time == t01ini) then
                        zs = 0 ; kcss(ns + 1:) = 0
                        do m = 1, ns                                      ! in deze set zitten pointers naar punt providers
                            mm = elementsets(ielset)%kcs(m)
                            if (subdoms(idom)%dataproviders(mm)%filetype == fourier) then ! no time interpolation for fourier,
                                !       u1 => subdoms(idom)%dataproviders(mm)%field(1 )%arr1d      ! just set zs
                                do k = 1, kx                                                ! @melding dangling in salfor
                                    !          zs(kx*(m-1)+k) = u1(k)
                                    zs(kx * (m - 1) + k) = subdoms(idom)%dataproviders(mm)%field(1)%arr1d(k)
                                enddo
                                kcss(ns + m) = 1
                            else
                                i0 = subdoms(idom)%dataproviders(mm)%it0
                                i1 = subdoms(idom)%dataproviders(mm)%it1
                                dmiss = subdoms(idom)%dataproviders(mm)%dmiss
                                u1 => subdoms(idom)%dataproviders(mm)%field(i1)%arr1d
                                u0 => subdoms(idom)%dataproviders(mm)%field(i0)%arr1d
                                if (u0(1) /= dmiss) then                              ! als links er is
                                    t0 = subdoms(idom)%dataproviders(mm)%field(i0)%time
                                    !                          if (u1(1) .ne. dmiss .and. u1(2) .ne. dmiss .or. &  ! en rechts is er ook of de tijd is links
                                    !                             t0max == t0                             ) then
                                    if (u1(1) /= dmiss .or. t0max == t0) then

                                        t1 = subdoms(idom)%dataproviders(mm)%field(i1)%time
                                        if (t1 == t0) then
                                            a1 = 0d0 ! anders dan boven
                                        else
                                            a1 = (t0max - t0) / (t1 - t0)
                                        endif
                                        a0 = 1d0 - a1
                                        if (jpoly == 0) then                            ! special case of mag/dir interpolation
                                            k = 1
                                            call magdir2uv(u0, u1, a0, a1, zs(kx * (m - 1) + k))
                                            do k = 3, kx
                                                zs(kx * (m - 1) + k) = a0 * u0(k) + a1 * u1(k)
                                            enddo
                                        else                                            ! the rest is standard
                                            do k = 1, kx
                                                zs(kx * (m - 1) + k) = a0 * u0(k) + a1 * u1(k)
                                            enddo
                                        endif
                                        kcss(ns + m) = 1
                                    endif
                                endif
                            endif
                        enddo

                        z => dataprovider%field(1 - it1)%arr3d  ;  z = dmiss ;
                        if (jpoly == 0) then
                            jdla = 1 ;
                            ! allways triangulate
                            call triint  (xs, ys, zs, kcss(ns + 1:), ns, &
                                    x, y, z, kcs, kx, mx * num_cells_u_dir, jdla)
                        else
                            xyen => elementsets(ielsetq)%xyen
                            if (dataprovider%method == weightfactors) then
                                jdla = dataprovider%refresh
                                call polyint (xs, ys, zs, kcss(ns + 1:), ns, &
                                        x, y, z, kcs, kx, mx, jdla, xyen, &
                                        dataprovider%indxn, dataprovider%wfn)
                                !hier niet mx*num_cells_u_dir, mx = elsetq
                                dataprovider%refresh = 0
                            else
                                jdla = 1
                                call polyint (xs, ys, zs, kcss(ns + 1:), ns, &
                                        x, y, z, kcs, kx, mx, jdla, xyen)
                                !hier niet mx*num_cells_u_dir, mx = elsetq
                            end if

                        endif

                        dataprovider%field(1 - it1)%time = t0max
                    endif

                end select

                dataprovider%field(it1)%time = tread
            endif

        enddo
        success = .true.
    end function updateprovider


    function gettimespacevalue(idom, qid, time, z) result(success)
        use m_waq_arcuv
        implicit none

        logical :: success

        ! Global variables
        integer, intent(in) :: idom    ! domnr
        character(*), intent(in) :: qid     !
        double precision, intent(in) :: time    !
        double precision, intent(out) :: z(:)     ! resultvector for this quantity

        !Local variables
        integer :: i
        integer :: k

        integer :: kx, nmx, mx, num_cells_u_dir, method
        integer :: it1
        integer :: it0
        integer :: ierr, ielset
        integer :: n, num
        integer :: i1
        integer :: j1, j, iop
        integer :: filetype
        integer :: upperindex(3), lowerindex(3)

        character(len = 1) :: operand

        double precision :: t1
        double precision :: t0
        double precision :: a1
        double precision :: a0

        double precision :: dmiss

        double precision :: x01
        double precision :: y01
        double precision :: dx1
        double precision :: dy1
        double precision :: x1
        double precision :: y1
        double precision :: di1
        double precision :: dj1
        double precision :: vv0
        double precision :: vv1
        double precision :: rr
        double precision, dimension(4) :: f
        double precision, dimension(4) :: u
        double precision, dimension(4) :: v
        double precision, dimension(40) :: uv     ! 40=u+v plus 38 possible other quantities on 1 meteo station

        double precision, dimension(:), pointer :: u0     ! 1-dim array
        double precision, dimension(:), pointer :: u1
        double precision, dimension(:, :), pointer :: v0     ! 2-dim array
        double precision, dimension(:, :), pointer :: v1
        double precision, dimension(:, :, :), pointer :: w0     ! 3-dim array
        double precision, dimension(:, :, :), pointer :: w1

        type(telementset), pointer :: elementsets(:)   ! bevat roosters zowel van vraagkant als aanbodkant
        type(tdataprovider), pointer :: dataproviders(:) ! bevat het aanbod
        type(tquantity), pointer :: quantities(:)    ! bevat de vraag

        ! en meer specifiek:
        type(telementset), pointer :: elsetp           ! elementset provider / aanbodkant
        type(telementset), pointer :: elsetq           ! elementset quantity / vraagkant
        type(tquantity), pointer :: quantity
        type(tdataprovider), pointer :: dataprovider

        k = index(qid, 'multiple_uni_')
        if (k > 0) then
            success = updatetimespaceproviders(idom, qid(14:), time)
        endif

        success = updatetimespaceproviders(idom, qid, time)

        dataproviders => subdoms(idom)%dataproviders
        elementsets => subdoms(idom)%elementsets
        quantities => subdoms(idom)%quantities

        success = .false.
        num = 0
        do k = 1, size(quantities)
            if (trim(qid) == trim(quantities(k)%qid)) num = k
        enddo
        if (num == 0) then
            errormessage = 'first initialise quantity using addrelation(idom, qid, kx, x, y, kcs, filename, filetype, method) '
            success = .false.
            return
        endif

        quantity => quantities(num)
        kx = quantity%kx
        ielset = quantity%ielset
        elsetq => elementsets(ielset)
        nmx = size(elsetq%x)

        ierr = 0

        do i = 1, size(quantity%providernrs)            ! loop over all relevant providers
            j = quantity%providernrs(i)
            dataprovider => dataproviders(j)
            mx = dataprovider%mx
            num_cells_u_dir = dataprovider%num_cells_u_dir
            kx = dataprovider%kx
            it0 = dataprovider%it0
            it1 = dataprovider%it1
            t0 = dataprovider%field(it0)%time
            t1 = dataprovider%field(it1)%time
            dmiss = dataprovider%dmiss
            method = dataprovider%method
            operand = dataprovider%operand
            filetype = dataprovider%filetype
            ielset = dataprovider%field(it0)%ielset
            elsetp => elementsets(ielset)
            iop = 1
            if (operand == 'O') iop = 0

            if (t1 == t0) then
                a1 = 1d0
            else
                a1 = (time - t0) / (t1 - t0)
            endif
            a0 = 1d0 - a1

            if (method == spacefirst .or. method == weightfactors) then

                select case (filetype)

                case (multiple_uni) ! alleen voor polytims die uitsluitend cmpsets bevatten, op alle steunpunten

                    w1 => dataprovider%field(it1)%arr3d
                    do n = 1, nmx
                        if (elsetq%kcs(n) /= 0) then
                            do k = 1, kx
                                rr = w1(n, 1, k)
                                if (rr /= dmiss) then
                                    call operate(z(kx * (n - 1) + k), rr, iop)
                                endif
                            enddo
                        endif
                    enddo

                case default

                    w1 => dataprovider%field(it1)%arr3d
                    w0 => dataprovider%field(it0)%arr3d

                    do n = 1, nmx
                        if (elsetq%kcs(n) /= 0) then
                            do k = 1, kx
                                if (w0(n, 1, k) /= dmiss .and. w1(n, 1, k) /= dmiss) then
                                    rr = a0 * w0(n, 1, k) + a1 * w1(n, 1, k)
                                    call operate(z(kx * (n - 1) + k), rr, iop)
                                endif
                            enddo
                        endif
                    enddo

                end select

            else if (method == spaceandtime) then

                select case (filetype)

                case (uniform)

                    u1 => dataprovider%field(it1)%arr1d
                    u0 => dataprovider%field(it0)%arr1d

                    do n = 1, nmx
                        if (elsetq%kcs(n) /= 0) then
                            do k = 1, kx
                                rr = a0 * u0(k) + a1 * u1(k)
                                call operate(z(kx * (n - 1) + k), rr, iop)
                            enddo
                        endif
                    enddo

                case (unimagdir)

                    ! arr1d contains wind magnitude and direction in index 1 and 2
                    ! first interpolate in time, then convert to u/v

                    u1 => dataprovider%field(it1)%arr1d
                    u0 => dataprovider%field(it0)%arr1d

                    call magdir2uv(u0, u1, a0, a1, uv)

                    do k = 3, kx   ! if there are other parameters after mag, dir, < 38
                        uv(k) = a0 * u0(k) + a1 * u1(k)
                    enddo

                    do n = 1, nmx
                        if (elsetq%kcs(n) /= 0) then
                            do k = 1, kx
                                call operate(z(kx * (n - 1) + k), uv(k), iop)
                            enddo
                        endif
                    enddo

                case (arcinfo, d3d_flow_arcinfo)

                    v1 => dataprovider%field(it1)%arr2d
                    v0 => dataprovider%field(it0)%arr2d

                    ! spatial coordinates
                    x01 = elsetp%x(1)
                    y01 = elsetp%y(1)
                    dx1 = elsetp%x(2)
                    dy1 = elsetp%y(2)

                    if (.not. allocated (arcuv)) then
                        upperindex = (/4, mx, num_cells_u_dir/)
                        lowerindex = (/1, 1, 1/)
                        call realloc(arcuv, upperindex, lowerindex) !4,mx,num_cells_u_dir)
                        arcuv = 0
                    endif

                    k = 1  ! up to now, arcinfo only available with vectormax = 1
                    do n = 1, nmx

                        if (elsetq%kcs(n) /= 0) then

                            x1 = (elsetq%x(n) - x01) / dx1
                            if (x1 < -0.5d0 .or. x1 > mx - 0.5d0) cycle

                            y1 = (elsetq%y(n) - y01) / dy1
                            if (y1 < -0.5d0 .or. y1 > num_cells_u_dir - 0.5d0) cycle

                            i1 = int(x1 + 1)
                            i1 = min(mx - 1, max(1, i1))
                            di1 = x1 + 1 - i1

                            j1 = int(y1 + 1)
                            j1 = min(num_cells_u_dir - 1, max(1, j1))
                            dj1 = y1 + 1 - j1


                            ! spatial weight factors

                            f(1) = (1 - di1) * (1 - dj1)
                            f(2) = (di1) * (1 - dj1)
                            f(3) = (di1) * (dj1)
                            f(4) = (1 - di1) * (dj1)

                            u(1) = v0(i1, j1)
                            u(2) = v0(i1 + 1, j1)
                            u(3) = v0(i1 + 1, j1 + 1)
                            u(4) = v0(i1, j1 + 1)
                            v(1) = v1(i1, j1)
                            v(2) = v1(i1 + 1, j1)
                            v(3) = v1(i1 + 1, j1 + 1)
                            v(4) = v1(i1, j1 + 1)
                            vv0 = u(1) * f(1) + u(2) * f(2) + u(3) * f(3) + u(4) * f(4)
                            vv1 = v(1) * f(1) + v(2) * f(2) + v(3) * f(3) + v(4) * f(4)
                            rr = a0 * vv0 + a1 * vv1
                            call operate(z(kx * (n - 1) + k), rr, iop)
                        endif
                    enddo

                    do i1 = 1, mx
                        do j1 = 1, num_cells_u_dir

                            arcuv(1, i1, j1) = x01 + (i1 - 1) * dx1
                            arcuv(2, i1, j1) = y01 + (j1 - 1) * dy1
                            if      (qid == 'windx') then
                                arcuv(3, i1, j1) = a0 * v0(i1, j1) + a1 * v1(i1, j1)
                            else if (qid == 'windy') then
                                arcuv(4, i1, j1) = a0 * v0(i1, j1) + a1 * v1(i1, j1)
                            else if (qid == 'atmospheric_pressure') then
                                arcuv(3, i1, j1) = a0 * v0(i1, j1) + a1 * v1(i1, j1)
                            endif

                        enddo
                    enddo

                case (curvi)

                end select
            endif
        enddo

        success = .true.
    end function gettimespacevalue


    function allocsubdoms(ndoms)  result(success) ! to be called at level that knows the nr of threads ndoms
        use m_alloc
        implicit none
        logical :: success

        ! arguments
        integer, intent(in) :: ndoms      ! nr of threads

        ! locals
        integer :: ierr

        success = .false.

        allocate (subdoms(ndoms), stat = ierr)

        if (ierr == 0) then
            success = .true.
        else
            errormessage = 'allocation error timespace subdoms'
        endif

    end function allocsubdoms


    function deallocsubdoms() result(success)
        implicit none
        logical success

        ! Locals
        integer :: i, idom

        type(tdataprovider), pointer :: dataproviders(:)         ! bevat het aanbod

        success = .true. ! prachtig, altijd goed !
        if (size(subdoms) == 0) return

        do idom = 1, size(subdoms)
            dataproviders => subdoms(idom)%dataproviders

            do i = 1, size(dataproviders)
                if (dataproviders(i)%minp > 0) then
                    call doclose(dataproviders(i)%minp)
                endif
            enddo
        enddo

        deallocate(subdoms)
        !  nullify   (subdoms)

    end function deallocsubdoms


    subroutine operate(a, b, iop)
        use m_waq_precision
        implicit none
        double precision :: a, b
        integer :: iop

        ! b = factor*b + offset ! todo doorplussen

        if (iop == 1) then
            a = a + b
        else if (iop == 0) then
            a = b
        endif

    end subroutine operate

    subroutine magdir2uv(u0, u1, a0, a1, uv)
        implicit none
        double precision :: a1
        double precision :: a0
        double precision :: u0(2)
        double precision :: u1(2)
        double precision :: uv(2)

        double precision :: wmag
        double precision :: wdir
        double precision :: wdir0
        double precision :: wdir1

        wdir0 = u0(2)
        wdir1 = u1(2)
        call regdir(wdir0, wdir1)
        wmag = a0 * u0(1) + a1 * u1(1)
        wdir = a0 * wdir0 + a1 * wdir1
        wdir = (270e0_sp - wdir) * d2r    ! nautical convention
        uv(1) = wmag * cos(wdir)
        uv(2) = wmag * sin(wdir)

    end subroutine magdir2uv


    subroutine regdir(w0, w1)
        !
        ! angle regularisation
        !
        implicit none
        double precision :: w0
        double precision :: w1
        if      ((w1 - w0) > 180d0) then
            w0 = w0 + 360d0
        else if ((w0 - w1) > 180d0) then
            w1 = w1 + 360d0
        endif

        !    if (w0>270. .and. w1<90.) then
        !       w1 = w1 + 360.
        !    elseif (w1>270. .and. w0<90.) then
        !       w0 = w0 + 360.
        !    endif

    end subroutine regdir

    subroutine bilin5(xa, ya, x0, y0, w, &
            & ier)
        use m_waq_precision
        implicit none
        !
        ! Global variables
        !
        integer, intent(out) :: ier
        double precision, intent(in) :: x0
        double precision, intent(in) :: y0
        double precision, dimension(4), intent(out) :: w
        double precision, dimension(4), intent(in) :: xa
        double precision, dimension(4), intent(in) :: ya
        !
        !
        ! Local variables
        !
        double precision :: a
        double precision :: a21
        double precision :: a22
        double precision :: a31
        double precision :: a32
        double precision :: a41
        double precision :: a42
        double precision :: b
        double precision :: c
        double precision :: det
        double precision :: discr
        double precision :: eta
        double precision :: x
        double precision :: x1
        double precision :: x2
        double precision :: x3
        double precision :: x3t
        double precision :: x4
        double precision :: xi
        double precision :: xt
        double precision :: y
        double precision :: y1
        double precision :: y2
        double precision :: y3
        double precision :: y3t
        double precision :: y4
        double precision :: yt
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !
        ! Author: H. Petit
        !
        !
        !     read(12,*)x1,y1,f1
        x1 = xa(1)
        y1 = ya(1)
        !     read(12,*)x2,y2,f2
        x2 = xa(2)
        y2 = ya(2)
        !     read(12,*)x3,y3,f3
        x3 = xa(3)
        y3 = ya(3)
        !     read(12,*)x4,y4,f4
        x4 = xa(4)
        y4 = ya(4)
        x = x0
        y = y0
        ! The bilinear interpolation problem is first transformed
        ! to the quadrangle with nodes
        ! (0,0),(1,0),(x3t,y3t),(0,1)
        ! and required location (xt,yt)
        a21 = x2 - x1
        a22 = y2 - y1
        a31 = x3 - x1
        a32 = y3 - y1
        a41 = x4 - x1
        a42 = y4 - y1
        det = a21 * a42 - a22 * a41
        !   if (abs(det)<1E-6) then
        if (abs(det)<1E-20) then
            ! write (*, *) 'surface is zero'
            ier = 1
            goto 99999
        endif
        x3t = (a42 * a31 - a41 * a32) / det
        y3t = (- a22 * a31 + a21 * a32) / det
        xt = (a42 * (x - x1) - a41 * (y - y1)) / det
        yt = (- a22 * (x - x1) + a21 * (y - y1)) / det
        if ((x3t<.0E0) .or. (y3t<.0E0)) then
            ! write (*, *) 'distorted quadrangle'
            ier = 1
            goto 99999
        endif
        if (abs(x3t - 1d0)<1.0D-7) then
            xi = xt
            if (abs(y3t - 1d0)<1.0D-7) then
                eta = yt
            elseif (abs(1d0 + (y3t - 1d0) * xt)<1.0D-6) then
                ! write (*, *) 'extrapolation over too large a distance'
                ier = 1
                goto 99999
            else
                eta = yt / (1d0 + (y3t - 1d0) * xt)
            endif
        elseif (abs(y3t - 1d0)<1.D-6) then
            eta = yt
            if (abs(1d0 + (x3t - 1d0) * yt)<1.D-6) then
                ! write (*, *) 'extrapolation over too large a distance'
                ier = 1
                goto 99999
            else
                xi = xt / (1d0 + (x3t - 1d0) * yt)
            endif
        else
            a = y3t - 1.
            b = 1d0 + (x3t - 1d0) * yt - (y3t - 1d0) * xt
            c = -xt
            discr = b * b - 4d0 * a * c
            if (discr<1.0D-6) then
                ! write (*, *) 'extrapolation over too large a distance'
                ier = 1
                goto 99999
            endif
            xi = (- b + sqrt(discr)) / (2d0 * a)
            eta = ((y3t - 1d0) * (xi - xt) + (x3t - 1d0) * yt) / (x3t - 1d0)
        endif
        w(1) = (1. - xi) * (1. - eta)
        w(2) = xi * (1. - eta)
        w(3) = xi * eta
        w(4) = eta * (1. - xi)
        return
        99999 continue
    end subroutine bilin5

end module m_waq_timespace
