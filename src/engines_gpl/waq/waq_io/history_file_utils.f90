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
module m_ods
    use m_waq_precision
    use m_open_waq_files
    use m_file_path_utils, only : extract_file_extension

    implicit none

    private
    public :: getloc, gettme, getpar, getmat, getmat2, getdim

contains

    subroutine getloc(file_name, itype, locdef, maxdef, iprdep, &
            itmdep, maxlst, loclst, loctyp, locnr, &
            nrlst, ierror, option)
        ! GETLOC routine for DELWAQ HIS-files
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

        use m_string_manipulation, only : upper_case

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

    end subroutine getloc

    subroutine gettme(fname, itype, timdef, maxdef, iprdep, &
            locdep, maxlst, timlst, itmtyp, nrlst, &
            ierror, option)
        ! GETTME routine for DELWAQ HIS-files
        !
        !     FNAME   CHAR*256   3        IN/LOC  Complete file name
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
        use m_string_manipulation, only : upper_case

        character*256    fname (3), option
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
        call open_waq_files (lun, fname(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(fname(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'map') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   fname(3)(1:160)
        if (fname(3)(121:123) /= 't0: ' .and. &
                fname(3)(121:123) /= 't0: ' .and. &
                fname(3)(121:123) /= 't0= ' .and. &
                fname(3)(121:123) /= 't0= ') then
            goto 150
        endif
        read (fname(3)(125:128), '(i4)') iyear
        read (fname(3)(130:131), '(i2)') imonth
        read (fname(3)(133:134), '(i2)') iday
        read (fname(3)(136:137), '(i2)') ihour
        read (fname(3)(139:140), '(i2)') iminut
        read (fname(3)(142:143), '(i2)') isecnd
        read (fname(3)(151:158), '(i8)') isfact
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (fname(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (lun, err = 130) (idummy, fname(3)(221:240), k = 1, nodump)
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

    end subroutine gettme

    subroutine getpar(fname, itype, pardef, maxdef, itmdep, &
            locdep, maxlst, lang, parlst, paruni, &
            iprtyp, iprcod, nrlst, ierror, option)
        !! ods getpar routine for delwaq his-files
        !
        !     fname   char*256   3        in/loc  complete file name
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
        character*256 :: fname(3), option
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
        call open_waq_files (lun, fname(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! read primary system characteristics
        read (lun, err = 100)   fname(3)(1:160)
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

    end subroutine getpar

    subroutine getmat(fname, itype, iprcod, loc, tim, &
            amiss, maxdim, data, ierror, &
            option)
        !! ods getmat routine for delwaq his-files
        !!
        !     fname   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     iprcod  integer  ierror     input   list of wanted parameters
        !     loc     integer   3*3       input   list of indices of locations
        !     tim     real*8     3        input   interval and step for data
        !     amiss   real*4     2        input   missing value in output/input
        !     maxdim  integer    1        input   maximum dimension of output arr
        !     data    real*4   maxdim     output  the produced information
        !     ierror  integer    1        in/out  error code
        !     option  char*256   1        in/out  for future use
        use time_module
        use m_string_manipulation, only : upper_case

        character*256 fname (3), option
        real(kind = real_wp) :: data(*)
        integer(kind = int_wp) :: loc(*)
        real(kind = dp) :: tim(2), otime, atime, second
        real(kind = real_wp) :: amiss
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
        call open_waq_files (lun, fname(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(fname(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   fname(3)(1:160)
        if (fname(3)(121:123) /= 't0: ' .and. &
                fname(3)(121:123) /= 't0: ' .and. &
                fname(3)(121:123) /= 't0= ' .and. &
                fname(3)(121:123) /= 't0= ') then
            goto 140
        endif
        read (fname(3)(125:128), '(i4)') iyear
        read (fname(3)(130:131), '(i2)') imonth
        read (fname(3)(133:134), '(i2)') iday
        read (fname(3)(136:137), '(i2)') ihour
        read (fname(3)(139:140), '(i2)') iminut
        read (fname(3)(142:143), '(i2)') isecnd
        read (fname(3)(151:158), '(i8)') isfact
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (fname(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (10, err = 130) (idummy, fname(3)(221:240), k = 1, nodump)
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

    end subroutine getmat

    subroutine getmat2(fname, itype, iprcod, loc, tim, &
            amiss, maxdim, data, ierror, &
            option)
        !! ods getmat routine for delwaq his-files

        use time_module
        use m_string_manipulation, only : upper_case

        !     fname   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     iprcod  integer  ierror     input   list of wanted parameters
        !     loc     integer   3*3       input   list of indices of locations
        !     tim     real*8     2        input   interval and step for data
        !     amiss   real*4     2        input   missing value in output/input
        !     maxdim  integer    1        input   maximum dimension of output arr
        !     data    real*4   maxdim     output  the produced information
        !     ierror  integer    1        in/out  error code
        !     option  char*256   1        in/out  for future use

        character*256 fname (3), option
        real(kind = real_wp) :: data(*)
        integer(kind = int_wp) :: loc(*)
        real(kind = dp) :: tim(2), otime, atime, second
        real(kind = real_wp) :: amiss
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
        call open_waq_files (lun, fname(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(fname(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   fname(3)(1:160)
        if (fname(3)(121:123) /= 't0: ' .and. &
                fname(3)(121:123) /= 't0: ' .and. &
                fname(3)(121:123) /= 't0= ' .and. &
                fname(3)(121:123) /= 't0= ') then
            goto 140
        endif
        read (fname(3)(125:128), '(i4)') iyear
        read (fname(3)(130:131), '(i2)') imonth
        read (fname(3)(133:134), '(i2)') iday
        read (fname(3)(136:137), '(i2)') ihour
        read (fname(3)(139:140), '(i2)') iminut
        read (fname(3)(142:143), '(i2)') isecnd
        read (fname(3)(151:158), '(i8)') isfact
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (fname(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (lun, err = 130) (idummy, fname(3)(221:240), k = 1, nodump)
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

    end subroutine getmat2

    subroutine getdim(fname, itype, dim, iprdep, itmdep, &
            locdep, ndim, ierror, option)
        !! ods getdim routine for delwaq his-files
        !!
        !     fname   char*256   3        in/loc  complete file name
        !     itype   integer    1        input   file type
        !     dim     char*3     1        input   wanted dimension
        !     iprdep  integer    1        input   par code for dimensions
        !     itmdep  integer    1        input   time code for dimensions
        !     locdep  integer    1        input   loc code for dimensions
        !     ndim    integer    5        output  wanted dimensions
        !     ierror  integer    1        output  error code
        !     option  char*256   1        in/out  for future use
        !
        !     note1: fname(3) is used as local character space
        !     note2: ndim is not according to ods specs, it returns:
        !            ndim(1) = nr of substances in the file
        !            ndim(2) = nr of locations  in the file
        !            ndim(3) = nr of time steps in the file

        use m_string_manipulation, only : upper_case

        character*256 fname(3), option
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
        call open_waq_files (lun, fname(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(fname(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100)   fname(3)(1:160)
        read (lun, err = 110)   notot, nodump
        read (lun, err = 120) (fname(3)(181:200), k = 1, notot)
        if (.not. mapfil) then
            read (lun, err = 130) (idummy, fname(3)(221:240), k = 1, nodump)
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

    end subroutine getdim

end module m_ods
