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
module open_data_structure
    use m_waq_precision
    use m_open_waq_files
    use m_file_path_utils, only : extract_file_extension
    use m_string_manipulation, only : upper_case

    implicit none

    private
    public :: get_loc, get_time, get_parameter, get_matrix_1, get_matrix_2, get_dimension, read_data_ods, &
            read_time_dependant_data_matrix

contains

    subroutine get_loc(file_name, itype, locdef, maxdef, iprdep, &
            itmdep, maxlst, loclst, loctyp, locnr, &
            nrlst, ierror, option)
        ! get_loc routine for DELWAQ HIS-files
        !
        !     file_name   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     LOCDEF  CHAR*20  MAXDEF     INPUT   List with wanted locations
        !     MAXDEF  INTEGER    1        INPUT   Length of LOCDEF
        !     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
        !     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
        !     MAXLST  INTEGER    1        INPUT   Dimension of the output arrays
        !     LOCLST  CHAR*20  MAXLST     OUTPUT  List of locations found
        !     LOCTYP  INTEGER  MAXLST     OUTPUT  List of location types
        !     LOCNR   INTEGER  MAXLST     OUTPUT  List of index nr. locations
        !     NRLST   INTEGER    1        OUTPUT  Nr of parameters found
        !     IERROR  INTEGER    1        OUTPUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use

        character*256 file_name(3), option
        character*20  locdef(maxdef), loclst(maxlst)
        dimension     loctyp(maxlst), locnr (maxlst)
        logical       setall
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: k, i1, i2, i3, notot, ierror, nodump, idummy
        integer(kind = int_wp) :: itype, itmdep, iprdep, nrlst, maxk, nbase
        integer(kind = int_wp) :: locnr, loctyp, maxdef, maxlst

        ! open the delwaq .his file
        lun = 10
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(file_name(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'map') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100) file_name(3)(1:160)
        read (lun, err = 110) notot, nodump
        read (lun, err = 120) (file_name(3)(181:200), k = 1, notot)

        ! read parameter names and try to find the wanted subset
        nrlst = 0
        setall = .false.
        if (locdef(1) == '*') setall = .true.

        do i1 = 1, nodump, maxlst
            maxk = min(nodump, i1 + maxlst - nrlst - 1) - i1 + 1
            if (.not. mapfil) then
                read (lun, err = 130) (idummy, loclst(k), k = nrlst + 1, nrlst + maxk)
            else
                do k = nrlst + 1, nrlst + maxk
                    write(loclst(k), '(''segment '',i8)') k
                enddo
            endif
            nbase = nrlst
            do i2 = 1, maxk
                do i3 = 1, maxdef
                    if (loclst(nbase + i2) == locdef(i3) .or. setall) then
                        nrlst = nrlst + 1
                        if (nrlst > maxlst) then
                            ierror = -nodump
                            goto 50
                        endif
                        loclst(nrlst) = loclst(nbase + i2)
                        locnr (nrlst) = i1 + i2 - 1
                        goto 30
                    endif
                end do
                30    continue
            end do
        end do

        ! supply the desired statistics
        50 do i1 = 1, nrlst
            loctyp(i1) = 2
        end do
        goto 200

        ! supply the desired statistics
        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12
        goto 200
        130 ierror = 13

        200 close (lun)

    end subroutine get_loc

    subroutine get_time(file_name, itype, timdef, maxdef, iprdep, &
            locdep, maxlst, timlst, itmtyp, nrlst, &
            ierror, option)
        ! get_time routine for DELWAQ HIS-files
        !
        !     file_name   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     TIMDEF  REAL*8   2,MAXDEF   INPUT   Wanted start and stop time
        !     MAXDEF  INTEGER    1        INPUT   Wanted time dimension
        !     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
        !     LOCDEP  INTEGER    1        INPUT   Par code for dimensions
        !     MAXLST  INTEGER    1        INPUT   Dimension output arrays
        !     TIMLST  REAL*8   MAXLST     OUTPUT  List with times found
        !     ITMTYP  INTEGER  MAXLST     OUTPUT  List with time types
        !     NRLST   INTEGER    1        OUTPUT  Nr of times found
        !     IERROR  INTEGER    1        OUTPUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use

        use time_module

        character*256    file_name (3), option
        integer(kind = int_wp) :: itmtyp(*)
        real(kind = dp) :: timlst(*), timdef(2, *), atime, otime, second
        logical          setall

        real(kind = real_wp), allocatable :: rdata(:)
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: nodump
        integer(kind = int_wp) :: k, i, notot, ntt
        integer(kind = int_wp) :: ierror, nrlst, iprcod, iprtyp
        integer(kind = int_wp) :: itype, maxdef, itmdep, locdep, maxlst, lang
        integer(kind = int_wp) :: iyear, imonth, iday, ihour, iminut, isecnd
        integer(kind = int_wp) :: isfact, idummy, idate, itime, iprdep

        ! open the delwaq .his file
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(file_name(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'map') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   file_name(3)(1:160)
        if (file_name(3)(121:123) /= 't0: ' .and. &
                file_name(3)(121:123) /= 't0: ' .and. &
                file_name(3)(121:123) /= 't0= ' .and. &
                file_name(3)(121:123) /= 't0= ') then
            goto 150
        endif
        read (file_name(3)(125:128), '(i4)') iyear
        read (file_name(3)(130:131), '(i2)') imonth
        read (file_name(3)(133:134), '(i2)') iday
        read (file_name(3)(136:137), '(i2)') ihour
        read (file_name(3)(139:140), '(i2)') iminut
        read (file_name(3)(142:143), '(i2)') isecnd
        read (file_name(3)(151:158), '(i8)') isfact
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (file_name(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (lun, err = 130) (idummy, file_name(3)(221:240), k = 1, nodump)
        endif
        idate = iyear * 10000 + imonth * 100 + iday
        itime = ihour * 10000 + iminut * 100 + isecnd
        otime = julian_with_leapyears (idate, itime)
        second = 1 / 864.00d+02

        ! read the values at all times
        ntt = nodump * notot
        allocate(rdata(ntt))
        nrlst = 0
        setall = .false.
        if (timdef(1, 1) < 0.5) setall = .true.

        10 read (lun, err = 140, end = 200) idummy, (rdata(k), k = 1, ntt)
        do i = 1, maxdef
            atime = otime + idummy * isfact * second
            if ((atime>timdef(1, i) .and. atime<timdef(2, i)) .or. &
                    setall) then
                nrlst = nrlst + 1
                if (nrlst > maxlst) goto 160
                timlst(nrlst) = atime
                itmtyp(nrlst) = 2
                goto 10
            endif
        end do
        goto 10

        ! error messages
        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12
        goto 200
        130 ierror = 13
        goto 200
        140 ierror = 14
        goto 200
        150 ierror = 15
        goto 200
        160 ierror = 16

        200 close (lun)
        if (allocated(rdata)) deallocate(rdata)
        return

    end subroutine get_time

    subroutine get_parameter(file_name, itype, pardef, maxdef, itmdep, &
            locdep, maxlst, lang, parlst, paruni, &
            iprtyp, iprcod, nrlst, ierror, option)
        !! ods get_parameter routine for delwaq his-files
        !
        !     file_name   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     pardef  char*20  maxdef     input   list with wanted par's
        !     maxdef  integer    1        input   length of pardef
        !     itmdep  integer    1        input   time code for dimensions
        !     locdep  integer    1        input   loc code for dimensions
        !     maxlst  integer    1        input   dimension of the output arrays
        !     lang    integer    1        input   language code
        !     parlst  char*20  maxlst     output  list of parameters found
        !     paruni  char*20  maxlst     output  list of parameter units found
        !     iprtyp  integer  maxlst     output  list of parameter types
        !     iprcod  integer  maxlst     output  list of parameter codes
        !     nrlst   integer    1        output  nr of parameters found
        !     ierror  integer    1        output  error code
        !     option  char*256   1        in/out  for future use
        character*256 :: file_name(3), option
        character*20 :: pardef(maxdef), parlst(maxlst), paruni(maxlst)
        dimension :: iprtyp(maxlst), iprcod(maxlst)
        logical :: setall
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: maxk
        integer(kind = int_wp) :: k, i1, i2, i3
        integer(kind = int_wp) :: ierror, notot, nodump, nrlst, iprcod, iprtyp
        integer(kind = int_wp) :: itype, maxdef, itmdep, locdep, maxlst, lang

        ! open the delwaq .his file
        lun = 10
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! read primary system characteristics
        read (lun, err = 100)   file_name(3)(1:160)
        read (lun, err = 110)   notot, nodump

        ! read parameter names and try to find the wanted subset
        nrlst = 0
        setall = .false.
        if (pardef(1) == '*') setall = .true.
        do i1 = 1, notot, maxlst
            maxk = min(notot, i1 + maxlst - 1) - i1 + 1
            read (lun, err = 120) (paruni(k), k = 1, maxk)
            do i2 = 1, maxk
                do i3 = 1, maxdef
                    if (paruni(i2) == pardef(i3) .or. setall) then
                        nrlst = nrlst + 1
                        if (nrlst > maxlst) then
                            ierror = -notot
                            goto 50
                        endif
                        parlst(nrlst) = paruni(i2)
                        iprcod(nrlst) = i1 + i2 - 1
                        goto 30
                    endif
                end do
                30    continue
            end do
        end do

        ! supply the desired statistics
        50 do i1 = 1, nrlst
            paruni(i1) = parlst(i1)(10:20)
            iprtyp(i1) = 2
        end do
        goto 200

        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12

        200 close (lun)

    end subroutine get_parameter

    subroutine get_matrix_1(file_name, itype, iprcod, loc, tim, &
            missing_value, maxdim, data, ierror, &
            option)
        !! ods get_matrix_1 routine for delwaq his-files
        !!
        !     file_name   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     iprcod  integer  ierror     input   list of wanted parameters
        !     loc     integer   3*3       input   list of indices of locations
        !     tim     real*8     3        input   interval and step for data
        !     missing_value   real*4     2        input   missing value in output/input
        !     maxdim  integer    1        input   maximum dimension of output arr
        !     data    real*4   maxdim     output  the produced information
        !     ierror  integer    1        in/out  error code
        !     option  char*256   1        in/out  for future use
        use time_module

        character*256 file_name (3), option
        real(kind = real_wp) :: data(*)
        integer(kind = int_wp) :: loc(*)
        real(kind = dp) :: tim(2), otime, atime, second
        real(kind = real_wp) :: missing_value
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: i1
        integer(kind = int_wp) :: nodump, notot
        integer(kind = int_wp) :: k, l, i2, i3, i4, ierror, iyear, imonth, iday
        integer(kind = int_wp) :: ihour, iminut, isecnd, isfact, idummy, idate
        integer(kind = int_wp) :: itime, ntt, iset, iprcod, maxdim
        integer(kind = int_wp) :: adummy, itype

        ! open the delwaq .his file if needed!
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(file_name(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   file_name(3)(1:160)
        if (file_name(3)(121:123) /= 't0: ' .and. &
                file_name(3)(121:123) /= 't0: ' .and. &
                file_name(3)(121:123) /= 't0= ' .and. &
                file_name(3)(121:123) /= 't0= ') then
            goto 140
        endif
        read (file_name(3)(125:128), '(i4)') iyear
        read (file_name(3)(130:131), '(i2)') imonth
        read (file_name(3)(133:134), '(i2)') iday
        read (file_name(3)(136:137), '(i2)') ihour
        read (file_name(3)(139:140), '(i2)') iminut
        read (file_name(3)(142:143), '(i2)') isecnd
        read (file_name(3)(151:158), '(i8)') isfact
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (file_name(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (10, err = 130) (idummy, file_name(3)(221:240), k = 1, nodump)
        endif
        idate = iyear * 10000 + imonth * 100 + iday
        itime = ihour * 10000 + iminut * 100 + isecnd
        otime = julian_with_leapyears (idate, itime)
        second = isfact / 864.00d+02

        ! standard ods processing
        ntt = nodump * notot
        iset = 0
        10 i1 = (loc(1) - 1) * notot + iprcod - 1
        i2 = (loc(2) - loc(1)) / loc(3)
        i3 = loc(3) * notot - 1
        i4 = ntt - i1 - (1 + i3) * i2 - 1
        if (iset + i2 + 1 > maxdim) goto 150
        read (lun, err = 150, end = 200) idummy, (adummy, k = 1, i1), &
                (data(iset + k), (adummy, l = 1, i3), k = 1, i2), &
                data(iset + i2 + 1), (adummy, l = 1, i4)
        atime = otime + idummy * second
        if (atime > tim(1) .and. atime < tim(2)) then
            iset = iset + i2 + 1
        endif
        goto 10

        ! error messages
        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12
        goto 200
        130 ierror = 13
        goto 200
        140 ierror = 14
        goto 200
        150 ierror = 15

        200 close (lun)

    end subroutine get_matrix_1

    subroutine get_matrix_2(file_name, itype, iprcod, loc, tim, &
            missing_value, maxdim, data, ierror, &
            option)
        !! ods get_matrix_1 routine for delwaq his-files

        use time_module

        !     file_name   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     iprcod  integer  ierror     input   list of wanted parameters
        !     loc     integer   3*3       input   list of indices of locations
        !     tim     real*8     2        input   interval and step for data
        !     missing_value   real*4     2        input   missing value in output/input
        !     maxdim  integer    1        input   maximum dimension of output arr
        !     data    real*4   maxdim     output  the produced information
        !     ierror  integer    1        in/out  error code
        !     option  char*256   1        in/out  for future use

        character*256 file_name (3), option
        real(kind = real_wp) :: data(*)
        integer(kind = int_wp) :: loc(*)
        real(kind = dp) :: tim(2), otime, atime, second
        real(kind = real_wp) :: missing_value
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun, nodump, notot, k, ntt
        integer(kind = int_wp) :: ierror, iyear, imonth, iday
        integer(kind = int_wp) :: ihour, iminut, isecnd, isfact, idummy, idate
        integer :: itime, iset, iprcod(:), maxdim
        integer(kind = int_wp) :: adummy, itype

        ! open the delwaq .his file if needed
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(file_name(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   file_name(3)(1:160)
        if (file_name(3)(121:123) /= 't0: ' .and. &
                file_name(3)(121:123) /= 't0: ' .and. &
                file_name(3)(121:123) /= 't0= ' .and. &
                file_name(3)(121:123) /= 't0= ') then
            goto 140
        endif
        read (file_name(3)(125:128), '(i4)') iyear
        read (file_name(3)(130:131), '(i2)') imonth
        read (file_name(3)(133:134), '(i2)') iday
        read (file_name(3)(136:137), '(i2)') ihour
        read (file_name(3)(139:140), '(i2)') iminut
        read (file_name(3)(142:143), '(i2)') isecnd
        read (file_name(3)(151:158), '(i8)') isfact
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (file_name(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (lun, err = 130) (idummy, file_name(3)(221:240), k = 1, nodump)
        endif
        idate = iyear * 10000 + imonth * 100 + iday
        itime = ihour * 10000 + iminut * 100 + isecnd
        otime = julian_with_leapyears (idate, itime)
        second = isfact / 864.00d+02

        ! standard ods processing
        ntt = nodump * notot
        iset = 0
        do
            if (iset + ntt > maxdim) goto 150
            read (lun, err = 150, end = 200) idummy, (data(iset + k), k = 1, ntt)
            atime = otime + idummy * second
            if (atime > tim(1) .and. atime < tim(2)) then
                iset = iset + ntt
            endif
        enddo

        ! error messages
        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12
        goto 200
        130 ierror = 13
        goto 200
        140 ierror = 14
        goto 200
        150 ierror = 15

        200 close (lun)

    end subroutine get_matrix_2

    subroutine get_dimension(file_name, itype, dim, iprdep, itmdep, &
            locdep, ndim, ierror, option)
        !! ods get_dimension routine for delwaq his-files
        !!
        !     file_name   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     dim     char*3     1        input   wanted dimension
        !     iprdep  integer    1        input   par code for dimensions
        !     itmdep  integer    1        input   time code for dimensions
        !     locdep  integer    1        input   loc code for dimensions
        !     ndim    integer    5        output  wanted dimensions
        !     ierror  integer    1        output  error code
        !     option  char*256   1        in/out  for future use
        !
        !     note1: file_name(3) is used as local character space
        !     note2: ndim is not according to ods specs, it returns:
        !            ndim(1) = nr of substances in the file
        !            ndim(2) = nr of locations  in the file
        !            ndim(3) = nr of time steps in the file

        character*256 file_name(3), option
        character*3   dim
        dimension     ndim(3)
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: nodump, notot
        integer(kind = int_wp) :: k, ntt, ierror, idummy, notim, ndim, itype
        real(kind = real_wp) :: adummy
        integer(kind = int_wp) :: iprdep, itmdep, locdep


        ! open the delwaq .his file
        lun = 10
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(file_name(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   file_name(3)(1:160)
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (file_name(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (lun, err = 130) (idummy, file_name(3)(221:240), k = 1, nodump)
        endif

        ! read the values at all times
        ntt = nodump * notot
        notim = 0

        10 read (lun, err = 140, end = 20)   idummy, (adummy, k = 1, ntt)
        notim = notim + 1
        goto 10

        ! supply the desired statistics
        20 ndim(1) = notot
        ndim(2) = nodump
        ndim(3) = notim
        goto 200

        ! supply the desired statistics
        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12
        goto 200
        130 ierror = 13
        goto 200
        140 ierror = 14

        200 close (lun)

    end subroutine get_dimension

    subroutine read_data_ods(lunut, file_name, data_param, data_loc, missing_value, data_block, ierr)
        !! read a block of data from ODS file

        use dlwq_hyd_data   ! for definition and storage of data
        use timers          !   performance timers
        use m_sysi          ! Timer characteristics
        use time_module
        use m_string_utils

        integer(kind = int_wp), intent(in) :: lunut         ! report file
        character(len = *), intent(in) :: file_name        ! filename ODS file
        type(t_dlwq_item), intent(inout) :: data_param   ! list of param items in the data
        type(t_dlwq_item), intent(inout) :: data_loc     ! list of loc items in the data
        real(kind = real_wp), intent(in) :: missing_value         ! missing value
        type(t_dlwqdata), intent(inout) :: data_block   ! data block
        integer(kind = int_wp), intent(inout) :: ierr          ! cummulative error count

        integer(kind = int_wp) :: iorder        ! order of the parameters and locations in the data array
        integer(kind = int_wp) :: loc(3)        ! to pass the locations to ODS
        character(len = 256) :: cfile(3)     ! to pass the filename to ODS
        character(len = 3) :: cdummy       ! dummy not used
        real(kind = dp) :: afact         ! scale factor for times
        real(kind = dp) :: a1            ! time
        real(kind = dp) :: a2            ! time
        character(len = 20) :: locdef(1)    ! to strore wanted locations
        character(len = 20), allocatable :: locnam(:)    ! locations in file
        integer(kind = int_wp), allocatable :: loctyp(:)     ! locations type in file
        integer(kind = int_wp), allocatable :: locnr(:)      ! locations numbers in file
        integer(kind = int_wp), allocatable :: iloc_ods(:)   ! locations index in file of the wanted locations
        character(len = 20) :: pardef(1)    ! to strore wanted parameters
        character(len = 20), allocatable :: parnam(:)    ! parameters in file
        character(len = 20), allocatable :: parunit(:)   ! parameters unit in file
        integer(kind = int_wp), allocatable :: partyp(:)     ! parameters type in file
        integer(kind = int_wp), allocatable :: parnr(:)      ! parameters numbers in file
        integer(kind = int_wp), allocatable :: ipar_ods(:)   ! parameter index in file of the wanted parameters
        real(kind = dp) :: timdef(2)     ! to store wanted times
        real(kind = dp), allocatable :: times(:)      ! times
        integer(kind = int_wp), allocatable :: timetyp(:)    ! time types ?
        real(kind = real_wp), allocatable :: buffer(:)     ! data buffer for read
        real(kind = real_wp), allocatable :: buffer2(:, :, :) ! data buffer for read
        integer(kind = int_wp) :: nsubs         ! number of parameters in file
        integer(kind = int_wp) :: nlocs         ! number of locations in file
        integer(kind = int_wp) :: ntims         ! number of times in file
        integer(kind = int_wp) :: nopar         ! number of parameters in file
        integer(kind = int_wp) :: noloc         ! number of locations in file
        integer(kind = int_wp) :: ndim1          ! first dimension
        integer(kind = int_wp) :: ndim2          ! second dimension
        integer(kind = int_wp) :: num_records         ! number of times in file
        integer(kind = int_wp) :: iloc          ! location index / loop counter
        integer(kind = int_wp) :: ipar          ! parameter index / loop counter
        integer(kind = int_wp) :: ibrk          ! time index / loop counter
        integer(kind = int_wp) :: iloc_found    ! location index in file
        integer(kind = int_wp) :: ipar_found    ! parameter index in file
        integer(kind = int_wp) :: ierror        ! ierror
        logical :: calculation  ! indicates that item is part of calculation
        integer(kind = int_wp) :: i, i1, i2
        integer(kind = int_wp) :: iy1, im1, id1, ih1, in1, is1
        integer(kind = int_wp) :: iy2, im2, id2, ih2, in2, is2
        real(kind = dp) :: dummy         ! second in double precision (not used)
        integer(kind = int_wp) :: maxdim
        integer(kind = int_wp) :: ierr_alloc

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_data_ods", ithndl)

        ! write the ods file name

        write (lunut, 1000) file_name

        ! get the dimensions of the ods file

        cfile(1) = file_name
        cfile(3) = ' '
        call get_dimension (cfile, 0, cdummy, 0, 0, &
                0, loc, ierror, cfile(3))
        nsubs = loc(1)
        nlocs = loc(2)
        ntims = loc(3)

        ! get the available locations

        allocate(locnam(nlocs), loctyp(nlocs), locnr(nlocs))
        allocate(iloc_ods(data_loc%no_item))
        locdef(1) = '*'
        call get_loc (cfile, 0, locdef, 1, 0, &
                0, nlocs, locnam, loctyp, locnr, &
                noloc, ierror, cfile(3))

        ! Fill an array with wanted locations

        do iloc = 1, data_loc%no_item
            if (data_loc%name(iloc) == '&$&$SYSTEM_NAME&$&$!') then
                iloc_ods(iloc) = -1
            else
                iloc_found = index_in_array(data_loc%name(iloc), locnam(:noloc))
                if (iloc_found >= 1) then
                    iloc_ods(iloc) = iloc_found
                else

                    ! location not found, warning

                    iloc_ods(iloc) = 0
                    write (lunut, 1070) data_loc%ipnt(iloc), data_loc%name(iloc)

                    ! check if location in calculation, then error, but is this check sufficient

                    calculation = data_loc%ipnt(iloc) < 0
                    if (iloc /= data_loc%no_item) calculation = calculation .or. data_loc%ipnt(iloc + 1) < 0
                    if (calculation) then
                        write (lunut, 1080)
                        ierr = 2
                        goto 510
                    endif

                endif
            endif
        enddo
        deallocate(locnam, loctyp, locnr)

        ! get the available parameters

        allocate(parnam(nsubs), parunit(nsubs), partyp(nsubs), parnr(nsubs))
        allocate(ipar_ods(data_param%no_item))
        pardef(1) = '*'
        call get_parameter (cfile, 0, pardef, 1, 0, &
                0, nsubs, 0, parnam, parunit, &
                partyp, parnr, nopar, ierror, cfile(3))

        ! fill an array with wanted parameters
        do ipar = 1, data_param%no_item
            if (data_param%name(ipar) == '&$&$SYSTEM_NAME&$&$!') then
                ipar_ods(ipar) = 0
            else
                ipar_found = index_in_array(data_param%name(ipar), parnam(:nopar))
                if (ipar_found > 0) then
                    ipar_ods(ipar) = ipar_found
                else

                    ! no compacting pointers for the moment, but how to deal with computation?

                    ipar_ods(ipar) = 0

                endif
            endif
        enddo
        deallocate(parnam, parunit, partyp, parnr)

        ! get the available time values

        allocate(times(ntims), timetyp(ntims))
        timdef(1) = 0.0d0
        call get_time (cfile, 0, timdef, 1, 0, &
                0, ntims, times, timetyp, num_records, &
                ierror, cfile(3))

        ! see if the found time values are within the range

        afact = isfact / 864.0d+02
        if (isfact < 0) afact = -1.0d+00 / isfact / 864.0d+02
        if (num_records >= 1) then
            write (lunut, 1020)
            a1 = deltim + itstrt * afact
            a2 = deltim + itstop * afact
            i1 = 1
            i2 = 1
            do ibrk = 1, num_records
                if (times(ibrk) <= a1) i1 = ibrk
                if (times(ibrk) < a2) i2 = ibrk
            enddo
            if (i2 /= num_records) i2 = i2 + 1
            if (times(num_records) < a1) i2 = 1

            ! errors and warnings

            if (times(1) > a1) then
                call gregor (times(1), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a1, iy2, im2, id2, ih2, in2, is2, dummy)
                write (lunut, 1030)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
            endif
            if (times(num_records) < a2) then
                call gregor (times(num_records), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a2, iy2, im2, id2, ih2, in2, is2, dummy)
                write (lunut, 1040)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
            endif
            num_records = i2 - i1 + 1
        endif
        write (lunut, 1050) num_records
        if (num_records == 1)    write (lunut, 1060)

        ! times are converted to delwaq times
        data_block%no_brk = num_records
        allocate(data_block%times(num_records))
        do ibrk = i1, i2
            a2 = times(ibrk) - a1
            data_block%times(ibrk - i1 + 1) = a2 / afact + 0.5
        enddo

        iorder = ORDER_PARAM_LOC
        data_block%iorder = iorder
        if (iorder == ORDER_PARAM_LOC) then
            ndim1 = data_param%no_item
            ndim2 = data_loc%no_item
        else
            ndim1 = data_loc%no_item
            ndim2 = data_param%no_item
        endif
        data_block%no_param = data_param%no_item
        data_block%no_loc = data_loc%no_item

        allocate(data_block%values(ndim1, ndim2, num_records), buffer(num_records))
        data_block%values = missing_value

        ! set the time margins for retrieval
        timdef(1) = times(1) - afact / 2.0
        timdef(2) = times(num_records) + afact / 2.0
        deallocate(times, timetyp)

        ! get the data themselves
        ! try the read the data in one time
        allocate(buffer2(nsubs, nlocs, num_records), stat = ierr_alloc)
        if (ierr_alloc == 0) then
            maxdim = nsubs * nlocs * num_records
            call get_matrix_2(cfile, 0, ipar_ods, loc, timdef, &
                    missing_value, maxdim, buffer2, ierror, &
                    cfile(3))
            do ipar = 1, data_param%no_item
                if (ipar_ods(ipar) > 0) then
                    do iloc = 1, data_loc%no_item
                        if (iloc_ods(iloc) > 0) then
                            do ibrk = 1, num_records
                                if (iorder == ORDER_PARAM_LOC) then
                                    data_block%values(ipar, iloc, ibrk) = buffer2(ipar_ods(ipar), iloc_ods(iloc), ibrk)
                                else
                                    data_block%values(iloc, ipar, ibrk) = buffer2(ipar_ods(ipar), iloc_ods(iloc), ibrk)
                                endif
                            enddo
                        endif
                    enddo
                endif
            enddo
            deallocate(buffer2)
        else
            loc(3) = 1
            do ipar = 1, data_param%no_item
                if (ipar_ods(ipar) > 0) then
                    do iloc = 1, data_loc%no_item
                        if (iloc_ods(iloc) > 0) then
                            loc(1) = iloc_ods(iloc)
                            loc(2) = iloc_ods(iloc)
                            call get_matrix_1 (cfile, 0, ipar_ods(ipar), loc, timdef, &
                                    missing_value, num_records, buffer, ierror, &
                                    cfile(3))
                            do ibrk = 1, num_records
                                if (iorder == ORDER_PARAM_LOC) then
                                    data_block%values(ipar, iloc, ibrk) = buffer(ibrk)
                                else
                                    data_block%values(iloc, ipar, ibrk) = buffer(ibrk)
                                endif
                            enddo
                        endif
                    enddo
                endif
            enddo
        endif
        deallocate(buffer, ipar_ods, iloc_ods)

        ! the sequence is the same as we read it, maybe always set both par and loc??

        if (iorder == ORDER_PARAM_LOC) then
            do i = 1, data_param%no_item
                data_param%sequence(i) = i
            enddo
        else
            do i = 1, data_loc%no_item
                data_loc%sequence(i) = i
            enddo
        endif

        510 continue
        if (timon) call timstop(ithndl)
        return

        1000 FORMAT (' DATA will be retrieved from ODS-file: ', A)
        1020 FORMAT (' This block consists of a time function.')
        1030 FORMAT (' WARNING: file start time   : ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, / &
                ' after simulation start time: ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1040 FORMAT (' WARNING: file stop  time   : ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, / &
                ' before simulation stop time: ', &
                I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1050 FORMAT (' Number of valid time steps found: ', I6)
        1060 FORMAT (' This block consists of constant data.')
        1070 FORMAT (' WARNING: location : ', I8, ' not found. Name is: ', A)
        1080 FORMAT (' ERROR  : location is used in a computation', &
                ' that will become corrupted !')
    END SUBROUTINE read_data_ods

    subroutine read_time_dependant_data_matrix(data_block, itfact, is_date_format, is_yyddhh_format, ierr)

        !! read a (time dependent) data matrix from input
        use dlwq_hyd_data
        use rd_token
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        type(t_dlwqdata), intent(inout) :: data_block   ! data block
        integer(kind = int_wp), intent(in) :: itfact        ! factor between clocks
        logical, intent(in) :: is_date_format       ! true if time in 'date' format
        logical, intent(in) :: is_yyddhh_format       ! true if yyetc instead of ddetc
        integer(kind = int_wp), intent(inout) :: ierr          ! cummulative error count

        ! local declarations
        integer(kind = int_wp) :: ftype          ! function type (constant,block,linear,harmonic,fourier)
        integer(kind = int_wp) :: mxbrk          ! allocate dimension of third dimension
        integer(kind = int_wp) :: ndim1          ! first dimension
        integer(kind = int_wp) :: ndim2          ! second dimension
        integer(kind = int_wp) :: num_records          ! third dimension
        integer(kind = int_wp), pointer :: times2(:)      ! used to resize
        real(kind = real_wp), pointer :: phase2(:)      ! used to resize
        real(kind = real_wp), pointer :: values2(:, :, :) ! used to resize
        integer(kind = int_wp) :: t_asked        ! type of token asked
        integer(kind = int_wp) :: t_token        ! type of token
        character(len = 256) :: ctoken        ! character token
        integer(kind = int_wp) :: itoken         ! integer token
        real(kind = real_wp) :: rtoken         ! real token
        character :: cdummy        ! dummy
        integer(kind = int_wp) :: idummy         ! dummy
        real(kind = real_wp) :: rdummy         ! dummy
        integer(kind = int_wp) :: i1, i2, i3       ! indexes
        integer(kind = int_wp) :: ibrk           ! indexe
        integer(kind = int_wp) :: ierr_alloc     ! error status
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_time_dependant_data_matrix", ithndl)

        ! dimension according to order

        if (data_block%iorder == ORDER_PARAM_LOC) then
            ndim1 = data_block%no_param
            ndim2 = data_block%no_loc
        else
            ndim1 = data_block%no_loc
            ndim2 = data_block%no_param
        endif

        ! read dependent on type of function

        ftype = data_block%functype
        if (ftype == FUNCTYPE_CONSTANT) then

            ! read only one "time"

            num_records = 1
            allocate(data_block%values(ndim1, ndim2, num_records), stat = ierr_alloc)
            data_block%values = 0.0
            allocate(data_block%times(num_records), stat = ierr_alloc)
            data_block%times(1) = 0
            do i2 = 1, ndim2
                do i1 = 1, ndim1
                    if (gettoken(rtoken, ierr) /= 0) goto 9999
                    data_block%values(i1, i2, num_records) = rtoken
                enddo
            enddo
        else

            ! read breakpoints in loop till next token is no longer a valid time
            mxbrk = 10
            allocate(data_block%times(mxbrk), data_block%values(ndim1, ndim2, mxbrk))
            if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                allocate(data_block%phase(mxbrk))
            endif

            num_records = 0
            breakpoints : do

                ! get next time

                if (gettoken(ctoken, itoken, rtoken, t_token, ierr) /= 0) then
                    ierr = 0
                    push = .true.
                    exit breakpoints
                endif

                ! check if character is a time string and convert
                if (t_token == TYPE_CHAR) then
                    call convert_string_to_time_offset (ctoken, itoken, .false., .false., ierr)
                    if (ierr /= 0) then
                        ierr = 0
                        push = .true.
                        exit breakpoints
                    endif
                else
                    call convert_relative_time (itoken, itfact, is_date_format, is_yyddhh_format)
                endif

                num_records = num_records + 1
                if (num_records > mxbrk) then ! resize
                    mxbrk = mxbrk * 2
                    allocate(times2(mxbrk), values2(ndim1, ndim2, mxbrk))
                    do ibrk = 1, num_records - 1
                        times2(ibrk) = data_block%times(ibrk)
                    end do
                    do ibrk = 1, num_records - 1
                        do i2 = 1, ndim2
                            do i1 = 1, ndim1
                                values2(i1, i2, ibrk) = data_block%values(i1, i2, ibrk)
                            enddo
                        enddo
                    enddo
                    deallocate(data_block%times, data_block%values)
                    data_block%times => times2
                    data_block%values => values2
                    nullify(times2)
                    nullify(values2)
                    if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                        allocate(phase2(mxbrk))
                        do ibrk = 1, num_records - 1
                            phase2(ibrk) = data_block%phase(ibrk)
                        end do
                        deallocate(data_block%phase)
                        data_block%phase => phase2
                        nullify(phase2)
                    endif
                endif
                data_block%times(num_records) = itoken

                ! for harmonics and fourier get phase
                if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                    if (gettoken(rtoken, ierr) /= 0) exit
                    data_block%phase(num_records) = rtoken
                endif

                ! get the data_block%values for this time
                do i2 = 1, ndim2
                    do i1 = 1, ndim1
                        if (gettoken(rtoken, ierr) /= 0) goto 9999
                        data_block%values(i1, i2, num_records) = rtoken
                    enddo
                enddo

            enddo breakpoints

            ! input ready, resize back the arrays
            if (num_records /= mxbrk) then
                allocate(times2(num_records), values2(ndim1, ndim2, num_records))
                do ibrk = 1, num_records
                    times2(ibrk) = data_block%times(ibrk)
                end do
                do ibrk = 1, num_records
                    do i2 = 1, ndim2
                        do i1 = 1, ndim1
                            values2(i1, i2, ibrk) = data_block%values(i1, i2, ibrk)
                        enddo
                    enddo
                enddo
                deallocate(data_block%times, data_block%values)
                data_block%times => times2
                data_block%values => values2
                nullify(times2)
                nullify(values2)
                if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                    allocate(phase2(mxbrk))
                    do ibrk = 1, num_records
                        phase2(ibrk) = data_block%phase(ibrk)
                    end do
                    deallocate(data_block%phase)
                    data_block%phase => phase2
                    nullify(phase2)
                endif
            endif

        endif

        data_block%no_brk = num_records

        9999 if (timon) call timstop(ithndl)

    end subroutine read_time_dependant_data_matrix

end module open_data_structure
