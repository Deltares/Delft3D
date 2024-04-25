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

subroutine putget(defnam, datnam, grpnam, nelems, &
        elmnms, elmdms, elmtps, nbytsg, &
        elmnam, celidt, wrilog, error, &
        buffr, fd_nef)
    implicit none

    integer         elmdms(6, *), nbytsg(*)
    integer         celidt, nelems, error

    integer         buffr(*)
    character(len=*)   elmnms(nelems), elmtps(nelems)
    character(len=*)   elmnam, grpnam
    !
    logical         wrilog
    !
    !-local declarations
    !
    integer         start, stopp, incr
    parameter      (start = 1, stopp = 2, incr = 3)
    !
    integer         buflen, elmndm
    integer         i, ierror, j
    integer         lelmnr, ind
    integer         n
    !
    integer         fd_nef
    integer         elmdim(5), uindex(3)
    !
    character(len=2)     access
    character(len=1)     coding
    character(len=16)    elmqta, elmant
    character(len=*)     datnam
    character(len=*)     defnam
    character(len=64)    elmdas
    character(len=134)   errstr
    !
    !-External Functions
    !
    integer         clsnef, credat, crenef, defcel, defelm, &
            defgrp, getelt, inqelm, neferr, putelt
    external        clsnef, credat, crenef, defcel, defelm, &
            defgrp, getelt, inqelm, neferr, putelt
    !
    !AM
    !     save fd_nef
    !     data fd_nef /-1/
    !-----------------------------------------------------------------------
    !-----Initialization
    !-----------------------------------------------------------------------
    coding = 'N'
    elmndm = 5
    uindex(start) = celidt
    uindex(stopp) = celidt
    uindex(incr) = 1

    elmant = '  '
    elmqta = '   '
    elmdas = '    '
    !-----------------------------------------------------------------------
    !-----write or read data from nefis files
    !-----------------------------------------------------------------------
    if (wrilog) then
        access = 'u'
    else
        access = 'r'
    endif
    !
    if (fd_nef < 0) then
        error = CRENEF (fd_nef, datnam, defnam, &
                coding, access)
        if (error/=0 .and. .not.wrilog) then
            error = -211
            goto 10000
        endif
        if (error/=0) goto 9999
    endif

    if (wrilog) then
        error = putelt(fd_nef, grpnam, elmnam, &
                uindex, 1, buffr)
    else
        j = 0
        123    continue
        j = j + 1
        if (elmnam == elmnms(j)) goto 124
        goto 123
        124    continue
        buflen = nbytsg(j) ! size single precision integer
        do i = 1, elmdms(1, j)
            buflen = buflen * elmdms(i + 1, j)
        enddo
        error = getelt(fd_nef, grpnam, elmnam, &
                uindex, 1, buflen, buffr)
        if (error/=0) goto 9999
    endif
    !-----------------------------------------------------------------------
    !-----error:
    !     writing: most likely error non existing group, so define it
    !     reading: error, no error expected
    !-----------------------------------------------------------------------
    if (error /= 0 .and. wrilog) then
        ! Create elements
        do lelmnr = 1, nelems
            error = DEFELM(fd_nef, elmnms(lelmnr), &
                    elmtps(lelmnr), nbytsg(lelmnr), &
                    elmqta, elmant, &
                    elmdas, elmdms(1, lelmnr), &
                    elmdms(2, lelmnr))
            !      most likely error, element already exist
            error = 0
        end do
        ! Create cells
        error = DEFCEL(fd_nef, grpnam, nelems, elmnms)
        if (error /= 0) goto 9999
        ! Create group on definition file
        error = DEFGRP(fd_nef, grpnam, grpnam, 1, 0, 1)
        if (error /= 0) goto 9999
        ! Create group on data       file
        error = CREDAT(fd_nef, grpnam, grpnam)
        if (error /= 0) goto 9999
        ! try again to write data
        error = putelt(fd_nef, grpnam, elmnam, &
                uindex, 1, buffr)
        if (error /= 0) goto 9999
    endif
    !
    !     No error when reading elements
    !
    if (error==0 .and. .not.wrilog) then
        write(*, *) 'putget'
        write(*, *) elmnam
        write(*, *) elmtps
        write(*, *) elmqta
        write(*, *) elmant
        write(*, *) elmdas
        error = INQELM(fd_nef, elmnam, elmtps, nbytsg, &
                elmqta, elmant, elmdas, elmndm, elmdim)

        if (error  /= 0) goto 9999
        lelmnr = 0
        do n = 1, nelems
            if (elmnam == elmnms(n)) then
                lelmnr = n
                goto 220
            endif
        end do
        220   continue
        if (lelmnr/=0) goto 9999
        !----------------------------------------------------------
        do i = 1, elmndm
            !----------------------------------------------------------
            !----------Compare local and global dimensions, not equal
            !          => new error number and exit
            !----------------------------------------------------------
            if (elmdim(i) /= elmdms(1 + i, lelmnr)) then
                error = -15025
                goto 9999
            endif
        end do
    endif
    goto 10000
    !-----------------------------------------------------------------------
    9999 continue
    if (error /= 0) ierror = Neferr(1, errstr)
    10000 continue
    !      ierror = clsnef( fd_nef )
    !
    return
end
