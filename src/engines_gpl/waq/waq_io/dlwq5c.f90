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
module m_dlwq5c
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    subroutine dlwq5c (fname, lunut, car, iar, rar, &
            icmax, iimax, irmax, drar, noitm, &
            nodim, iorder, scale, itmnr, idmnr, &
            amiss, nobrk, ierr, status)

        !
        !     Deltares        Sector Waterresources And Environment
        !
        !     Created            : May '97  by L. Postma
        !
        !     Modified           :
        !
        !     Function           : Boundary and waste data new style
        !                          Data retrieval from an ODS file
        !
        !     Subroutines called : conver - converting times of breakpoints
        !
        !     Logical units      : lunut   = report file
        !
        !     Parameters    :
        !
        !     Name    Kind     Length     Funct.  Description
        !     ---------------------------------------------------------
        !     fname   char*(*)   1         input   filename of the ods file
        !     car     character  *         local   character workspace
        !     iar     integer  iimax       local   integer   workspace
        !     rar     real     irmax       local   real      workspace
        !     icmax   integer    1         input   max. char workspace dimension
        !     iimax   integer    1         input   max. int. workspace dimension
        !     irmax   integer    1         input   max. real workspace dimension
        !     drar    real*8     1         in/out  double precision workspace
        !     noitm   integer    1         input   number of bounds/wastes
        !     nodim   integer    1         input   number of concentrations
        !     iorder  integer    1         input   order of the input
        !     scale   logical    1         input   true if scale values are stored
        !     ioffc   integer    1         input   offset of the concentrations    ioffi   integer    1         input   offset in the integer array
        !     ioffi   integer    1         input   offset of the items             ioffi   integer    1         input   offset in the integer array
        !     amiss   real       1         input   missing value indicator
        !     nobrk   integer    1         output  number of time steps found
        !     ierr    integer    1         output  error flag
        !     iwar    integer    1         in/out  cumulative warning count
        !
        !     in the common block:
        !
        !     Name    Kind     Length     Funct.  Description
        !     ---------------------------------------------------------
        !     block sysi.inc
        !     itstrt  integer    1         input   simulation start time ( scu )
        !     itstop  integer    1         input   simulation stop time ( scu )
        !     isfact  integer    1         input   system clock in seconds
        !     otime   real*8     1         input   julian offset of the real time
        !
        !     The map of the Character array is:
        !     First NOITM + NODIM entries the names of ITEMS and SUBSTANCES
        !         IF IORDER = 1 then ITEMS first IF 2 then CONCENTRATIONS first
        !     The rest of the array is free working space for this routine
        !         Next NOITM + NODIM entries reserved for names of values to be
        !                                                             retrieved
        !         Further locations is workspace
        !
        !     The map of the Integer array is:
        !     First NOITM + NODIM entries the names of ITEMS and SUBSTANCES
        !         IF IORDER = 1 then ITEMS first IF 2 then CONCENTRATIONS first
        !     Then NOBRK time breakpoints to be read in eg in this routine
        !     The rest of the array is free working space
        !         Initially next NOITM + NODIM entries reserved for numbers of
        !              locations or substances as stored in the character array.
        !              In the corresponding location below NOITM + NODIM a
        !                                  reference is made to this location.
        !         Initially next NOITM + NODIM entries reserved for numbers of
        !              locations or substances as stored in the ODS file for
        !                                  retrieval
        !         Initially next NOBRK values are for retrieved time values
        !
        !     The map of the Integer array is:
        !     IF (SCALE) First NODIM entries the scale factors
        !     Then the matrix of values to be read in eg in this routine
        !
        use m_dlwq5h
        use m_gettme
        use m_getpar
        use m_getmat
        use m_getloc
        use m_getdim
        use timers       !   performance timers
        use m_sysi          ! Timer characteristics
        use time_module

        integer(kind = int_wp) :: icmax, iimax, irmax
        character*(*) car(:), fname
        integer(kind = int_wp) :: iar(:)
        real(kind = real_wp) :: rar(:)
        logical       scale
        real(kind = dp) :: drar(*)
        character     cfile(3)*256
        real(kind = real_wp) :: amiss

        type(error_status), intent(inout) :: status !< current error status
        !
        !     local declarations
        dimension     loc(3)
        real(kind = dp) :: afact, a1, a2, d_beg, d_end, dummy
        character*3   cdummy
        integer(kind = int_wp) :: nodim, iorder, ioffa, ioffb, ioffc, ioffd, nscle, lunut
        integer(kind = int_wp) :: k1, ierror, nsubs, nlocs, ntims, j1, j2, j3, k2, k3
        integer(kind = int_wp) :: ierr, noloc, noit2, noitv, j
        integer(kind = int_wp) :: nottt, itmnr, notim, idmnr, i, ishft, ltot
        integer(kind = int_wp) :: noitm, nshft, nopar, icnt, k5, nitm, k, k4, nobrk, k6
        integer(kind = int_wp) :: iy1, im1, id1, ih1, in1, is1
        integer(kind = int_wp) :: iy2, im2, id2, ih2, in12
        integer(kind = int_wp) :: i1, i2, in2, is2, nt1, nt2, is, maxd, loc, ig, igs, kp
        integer(kind = int_wp) :: kl, ig2
        !
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("dlwq5c", ithndl)
        !
        !     array offsets
        nottt = itmnr + noitm + idmnr + nodim
        if (iorder == 1) then
            ioffa = itmnr
            ioffb = itmnr + noitm + idmnr
            ioffc = 0
            ioffd = itmnr + noitm
            nscle = nodim
        else if (iorder == 2) then
            ioffa = idmnr + nodim + itmnr
            ioffb = idmnr
            ioffc = idmnr + nodim
            ioffd = 0
            nscle = noitm
        end if
        !
        !     write the ods file name
        write (lunut, 1000) fname
        !
        !     get the dimensions of the ods file
        cfile(1) = fname
        cfile(3) = ' '
        k1 = nottt + 1
        call getdim (cfile, 0, cdummy, 0, 0, &
                0, iar(k1:k1), ierror, cfile(3))
        nsubs = iar(k1)
        nlocs = iar(k1 + 1)
        ntims = iar(k1 + 2)
        !
        !     deal with locations ( j for characters, k for integers )
        j1 = nottt + 1
        j2 = j1 + 1
        k1 = nottt + noitm + 1
        j3 = j2 + nlocs
        k2 = k1 + nlocs
        !
        !     see if storage is available
        k3 = min ((iimax - k2), (icmax - j3))
        if (k3 < nlocs) then
            write (lunut, 1010) k3, nlocs
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if
        !
        !    get the available locations
        car(j1) = '*'
        call getloc (cfile, 0, car(j1), 1, 0, &
                0, k3, car(j2), iar(k1:k1), iar(k2:k2), &
                noloc, ierror, cfile(3))
        !
        !    fill an array with wanted locations
        noit2 = 0
        noitv = 0
        do j = 1, noitm
            if (car(ioffa + j) == '&$&$SYSTEM_NAME&$&$!') then
                noit2 = noit2 + 1
                noitv = noitv + 1
                car(ioffa + noit2) = car(ioffa + j)
                car(ioffc + noit2) = car(ioffc + j)
                iar(ioffa + noit2) = iar(ioffa + j)
                iar(ioffc + noit2) = iar(ioffc + j)
                iar(nottt + noit2) = -1
                if (scale .and. iorder == 2) rar(noit2) = rar(j)
                cycle
            end if
            i = index_in_array(car(ioffa + j)(:20), car(j1 + 1:noloc))
            if (i >= 1) then
                noit2 = noit2 + 1
                car(ioffa + noit2) = car(ioffa + j)
                car(ioffc + noit2) = car(ioffc + j)
                iar(ioffa + noit2) = iar(ioffa + j)
                iar(ioffc + noit2) = iar(ioffc + j)
                iar(nottt + noit2) = i
                cycle
            end if
            write (lunut, 1070) iar(ioffa + j), car(ioffa + j)
            call status%increase_warning_count()
            if (iar(ioffa + j) < 0 .or. (iar(ioffa + j + 1) < 0 .and. j /= noitm)) then
                write (lunut, 1080)
                ierr = 2
                if (timon) call timstop(ithndl)
                return
            end if
        end do
        !
        !     compact the pointers for unresolved externals
        ishft = noitm - noit2
        if (iorder == 1) then
            ltot = idmnr + nodim
        else
            ltot = 0
        end if
        do i = ioffa + noit2 + 1, ioffa + noit2 + ltot + noit2
            car(i) = car(i + ishft)
            iar(i) = iar(i + ishft)
        end do
        do i = ioffc + noit2 + 1, ioffc + noit2 + ltot + noit2 * 2
            car(i) = car(i + ishft)
            iar(i) = iar(i + ishft)
        end do
        nottt = nottt - ishft * 2
        itmnr = itmnr - ishft
        noitm = noitm - ishft
        if (iorder == 1) then
            ioffb = itmnr + noitm + idmnr
            ioffd = itmnr + noitm
            nshft = 0
        else
            nshft = itmnr + noitm
        end if
        !
        !     deal with substances
        j1 = nottt + 1
        j2 = j1 + 1
        k1 = nottt + noitm + 1
        j3 = j2 + nsubs
        k2 = k1 + nsubs
        !
        !     see if storage is available
        k3 = min ((iimax - k2), (icmax - j3))
        if (k3 < nsubs) then
            write (lunut, 1010) k3, nsubs
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if
        !
        !    get the available substances
        call getpar (cfile, 0, car(j1), 1, 0, &
                0, k3, 0, car(j2), car(j3), &
                iar(k1:k1), iar(k2:k2), nopar, ierror, cfile(3))
        !
        !     fill an array with wanted substances
        icnt = 0
        k5 = nottt + noitm
        nitm = nodim
        do j = 1, nitm
            k = j - icnt
            iar(k5 + k) = 0
            if (car(ioffb + j) == '&$&$SYSTEM_NAME&$&$!') cycle
            i = index_in_array(car(ioffb + k)(1:20), car(j1 + 1:nopar))
            if (i >= 1) then
                iar(k5 + k) = i
                cycle
            end if
            call compact_usefor_list(lunut, iar, itmnr, noitm, idmnr, &
                    nodim, iorder, car, k5, ioffb, &
                    nshft, ioffd, k, icnt, ierr, status)
            if (timon) call timstop(ithndl)
            return
        end do
        k1 = k1 + nodim
        !
        !     get the time values
        k3 = iimax - k1
        !     first nodim real*4 can be scale values
        k2 = 1
        if (scale) k2 = nscle / 2 + 2
        k5 = k2 + 3
        k4 = (irmax - k5 * 2) / 2
        !     see if there is space enough
        k4 = min (k3, k4)
        !
        !     see if storage is available
        if (k4 < ntims) then
            write (lunut, 1010) k4, ntims
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if
        afact = isfact / 864.0d+02
        if (isfact < 0) afact = -1.0d+00 / isfact / 864.0d+02
        !
        !     get the available time values
        drar(k2) = 0
        call gettme (cfile, 0, drar(k2), 1, 0, &
                0, k4, drar(k5), iar(k1:k1), nobrk, &
                ierror, cfile(3))
        !
        !     see if the found time values are within the range
        if (nobrk >= 1) then
            write (lunut, 1020)
            a1 = deltim + itstrt * afact
            a2 = deltim + itstop * afact
            i1 = 1
            i2 = 1
            do i = 1, nobrk
                if (drar(k5 + i - 1) <= a1) i1 = i
                if (drar(k5 + i - 1) < a2) i2 = i
            end do
            if (i2 /= nobrk) i2 = i2 + 1
            k6 = k5 + nobrk - 1
            if (drar(k6) < a1) i2 = 1
            !        errors and warnings
            if (drar(k5) > a1) then
                call gregor (drar(k5), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a1, iy2, im2, id2, ih2, in2, is2, dummy)
                write (lunut, 1030)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
                call status%increase_warning_count()
            end if
            if (drar(k6) < a2) then
                call gregor (drar(k6), iy1, im1, id1, ih1, in1, is1, dummy)
                call gregor (a2, iy2, im2, id2, ih2, in2, is2, dummy)
                write (lunut, 1040)  iy1, im1, id1, ih1, in1, is1, &
                        iy2, im2, id2, ih2, in2, is2
                call status%increase_warning_count()
            end if
            nobrk = i2 - i1 + 1
        end if
        write (lunut, 1050) nobrk
        if (nobrk == 1)    write (lunut, 1060)
        !     times are converted to delwaq times
        do i = i1, i2
            a2 = drar(k5 + i - 1) - deltim
            iar(k1 + i - i1) = a2 / afact + 0.5
        end do
        !      see if enough space is available
        !           nr substances  nr locations for retrieval
        nt1 = nodim * noitm
        !           nr substances  nr locations for storage
        nt2 = nodim + noitm
        !           real retrieval space + 1
        is = nt1 * nobrk + 1
        if (scale) is = is + nscle
        !           convert for double precission,
        !           nobrk is max number of retrievals per invocation
        is2 = (is + nobrk + 1) / 2 + 1
        !           then the offset increases
        maxd = irmax - is
        if (maxd < nobrk) then
            write (lunut, 1010) is + nobrk, irmax
            ierr = 1
            if (timon) call timstop(ithndl)
            return
        end if
        !     set the time margins for retrieval
        !
        !     JVB, the endtime can be overwritten here
        !
        !      drar(is2  ) = drar(k5+i1-1) - afact/2.0
        !      drar(is2+1) = drar(k5+i2-1) + afact/2.0
        d_beg = drar(k5 + i1 - 1) - afact / 2.0
        d_end = drar(k5 + i2 - 1) + afact / 2.0
        drar(is2) = d_beg
        drar(is2 + 1) = d_end
        !CJVB
        !
        !             get the data themselves
        !
        loc(3) = 1
        igs = 1
        ig = 1
        if (scale) ig = ig + nscle
        do i = 1, nodim
            ! this should correspond with the found substance numbers
            kp = iar(nottt + noitm + i)
            if (kp < 0) cycle
            if (iorder == 1) then
                ig = igs
                igs = igs + 1
                if (scale) ig = ig + nscle
            end if
            do j = 1, noitm
                ! this should correspond with the found location numbers
                kl = iar(nottt + j)
                if (kl > 0) then
                    loc(1) = kl
                    loc(2) = kl
                    call getmat (cfile, 0, kp, loc, drar(is2), &
                            amiss, maxd, rar(is:is), ierror, &
                            cfile(3))
                end if
                ig2 = ig
                !           this loop is per location, so skip the amount of substances if iorder is 1
                if (iorder == 1) then
                    ig = ig + nodim
                else
                    ig = ig + 1
                end if
                do k = 0, nobrk - 1
                    rar(ig2) = rar(is + k)
                    !              skip a full matrix further, because this is this substance for all
                    !              breakpoints
                    ig2 = ig2 + nt1
                end do ! k = 0 , nobrk-1
            end do ! do j = 1 , noitm
        end do ! do i = 1 , nodim
        do i = 1, nscle
            iar(nottt + i) = i
        end do
        do i = 1, nobrk
            iar(nottt + nscle + i) = iar(k1 + i - 1)
        end do
        if (timon) call timstop(ithndl)
        return
        !
        !      formats
        !
        1000 format(' DATA will be retrieved from ODS-file: ', A)
        1010 format(' ERROR: Insufficient memory ! Available:', I10, ', needed:', I10, ' !')
        1020 format(' This block consists of a time function.')
        1030 format(' WARNING: file start time   : ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, / &
                ' after simulation start time: ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1040 format(' WARNING: file stop  time   : ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, /  &
                ' before simulation stop time: ', I4, '.', I2, '.', I2, ' ', I2, ':', I2, ':', I2, ' !')
        1050 format(' Number of valid time steps found: ', I6)
        1060 format(' This block consists of constant data.')
        1070 format(' WARNING: location : ', I8, ' not found. Name is: ', A)
        1080 format(' ERROR  : location is used in a computation', ' that will become corrupted !')
        !
    END SUBROUTINE DLWQ5C

end module m_dlwq5c
