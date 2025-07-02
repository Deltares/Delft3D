!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module putget_mod
    !
    !  Generic module for putget operations on NEFIS files
    !
    !  module declarations
    !
    !
    !  data definition module(s)
    !
    use m_waq_precision          ! single and double precisionv
    use timers
    !
    !  module procedure(s)
    !
    use noextspaces_mod    ! explicit interface for subroutine calls
    !
    implicit none
    !
    interface putget
        module procedure putget_int      ! for reading/ writing integer (scalar)
        module procedure putget_int1D    ! for reading/ writing integer 1D arrays
        module procedure putget_int2D    ! for reading/ writing integer 2D arrays
        module procedure putget_real     ! for reading/ writing real (scalar)
        module procedure putget_real1D   ! for reading/ writing real 1D arrays
        module procedure putget_real2D   ! for reading/ writing real 2D arrays
        module procedure putget_char     ! for reading/ writing character (scalar)
        module procedure putget_char1D   ! for reading/ writing character arrays
    end interface
    !
    !   module data
    !
    !
    integer(int_wp), parameter :: start = 1, stopp = 2, incr = 3
    integer(int_wp), parameter :: no_groups = 5
    !
    integer(int_wp) :: buflen, elmndm
    integer(int_wp) :: inef, ierror, jnef
    integer(int_wp) :: lelmnr
    integer(int_wp) :: nnef
    integer(int_wp) :: igr
    !
    integer(int_wp) :: datlen
    integer(int_wp) :: deflen
    integer(int_wp), dimension(10) :: usrord
    integer(int_wp), dimension(3, no_groups) :: uindex
    integer(int_wp), dimension(5) :: elmdim
    !
    character(len = 1) :: access
    character(len = 1) :: coding
    character(len = 8) :: elmtap
    character(len = 16) :: elmqta, elmant
    character(len = 64) :: elmdas
    character(len = 256) :: datnam
    character(len = 256) :: defnam
    character(len = 1023) :: errstr
    !
    !-external functions
    !
    integer(int_wp) :: clsnef, credat, crenef, defcel, defelm, &
            defgrp, getelt, inqelm, neferr, putelt, &
            putels, getels
    external :: clsnef, credat, crenef, defcel, defelm, &
            defgrp, getelt, inqelm, neferr, putelt, &
            putels, getels
    !
    save           fd_nef
    integer(int_wp) :: fd_nef = -1

contains
    !---------------------------------------------------------------------------
    !     putget_int : specific procedure for reading/ writing an INTEGER
    !---------------------------------------------------------------------------
    subroutine putget_int (filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, ibuffr)
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        integer(int_wp) :: ibuffr
        integer(int_wp), dimension(1) :: buffr      ! array is placed on stack
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_int", ithndl)
        !
        buffr(1) = ibuffr
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do

        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo

            error = getelt(fd_nef, grpnam, elmnam, uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) then
            ierror = neferr(1, errstr)
            write(*, *) trim(errstr)
        endif
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_int


    !-----------------------------------------------------------------------------
    !     putget_int1D: specific procedure for reading/ writing 1D integer arrays
    !-----------------------------------------------------------------------------
    subroutine putget_int1D(filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, buffr)
        implicit none
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        integer(int_wp), dimension(:) :: buffr
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_int1D", ithndl)
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo

            error = getelt(fd_nef, grpnam, elmnam, uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) then
            ierror = neferr(1, errstr)
            write(*, *) trim(errstr)
        endif
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_int1D


    !-----------------------------------------------------------------------------
    !     putget_int2D: specific procedure for reading/ writing 2D integer arrays
    !-----------------------------------------------------------------------------
    subroutine putget_int2D(filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, ibuffr)
        implicit none
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        integer(int_wp), dimension(:, :) :: ibuffr
        integer(int_wp), dimension(:), allocatable :: buffr ! to prevent that the array is placed on the stack
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer :: k
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_int2D", ithndl)
        !
        !     First transform 2D into 1D work array
        !
        allocate(buffr(size(ibuffr)))
        !
        !     buffr = reshape(ibuffr,(/size(ibuffr)/))  ! done on the stack
        k = 0
        do jnef = 1, size(ibuffr, 2)
            do inef = 1, size(ibuffr, 1)
                k = k + 1
                buffr(k) = ibuffr(inef, jnef)
            enddo
        enddo
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo

            error = getelt(fd_nef, grpnam, elmnam, uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) then
            ierror = neferr(1, errstr)
            write(*, *) trim(errstr)
        endif
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_int2D

    !---------------------------------------------------------------------------
    !     putget_real : specific procedure for reading/ writing a REAL
    !---------------------------------------------------------------------------
    subroutine putget_real                                               &
            (filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, rbuffr)
        implicit none
        !
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        real(sp) :: rbuffr
        real(sp), dimension(1) :: buffr      ! array is placed on stack
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_real", ithndl)
        !
        buffr(1) = rbuffr
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo

            error = getelt(fd_nef, grpnam, elmnam, uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) then
            ierror = neferr(1, errstr)
            write(*, *) trim(errstr)
        endif
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_real


    !---------------------------------------------------------------------------
    !     putget_real1D: specific procedure for reading/ writing 1D real arrays
    !---------------------------------------------------------------------------
    subroutine putget_real1D                                            &
            (filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, buffr)
        implicit none
        !
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        real(sp), dimension(:) :: buffr
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_real1D", ithndl)
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo

            error = getelt(fd_nef, grpnam, elmnam, uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) then
            ierror = neferr(1, errstr)
            write(*, *) trim(errstr)
        endif
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_real1D

    !---------------------------------------------------------------------------
    !     putget_real1D: specific procedure for reading/ writing 2D real arrays
    !---------------------------------------------------------------------------
    subroutine putget_real2D                                          &
            (filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, rbuffr)
        implicit none
        !
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        real(sp), dimension(:, :) :: rbuffr
        real(sp), dimension(:), allocatable :: buffr ! to prevent that the array is placed on the stack
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer :: k
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_real2D", ithndl)
        !
        !     First transform 2D into 1D work array
        !
        allocate(buffr(size(rbuffr)))

        !     buffr = reshape(rbuffr,(/size(rbuffr)/))     ! done on the stack
        k = 0
        do jnef = 1, size(rbuffr, 2)
            do inef = 1, size(rbuffr, 1)
                k = k + 1
                buffr(k) = rbuffr(inef, jnef)
            enddo
        enddo
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo

            error = getelt(fd_nef, grpnam, elmnam, uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putelt(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) then
            ierror = neferr(1, errstr)
            write(*, *) trim(errstr)
        endif
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_real2D

    !---------------------------------------------------------------------------
    !     putget_char: specific procedure for reading/writing a CHARACTER
    !---------------------------------------------------------------------------
    subroutine putget_char                                            &
            (filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, cbuffr)
        implicit none
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        character(len = *) :: cbuffr
        character(len = len(cbuffr)), dimension(1) :: buffr      ! array is placed on stack

        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_char", ithndl)
        !
        buffr(1) = cbuffr
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putels(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo
            error = getels(fd_nef, grpnam, elmnam, &
                    uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putels(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) ierror = neferr(1, errstr)
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_char


    !---------------------------------------------------------------------------
    !     putget_char1D: specific procedure for reading/writing a CHARACTER array
    !---------------------------------------------------------------------------
    subroutine putget_char1D                                          &
            (filnam, grpnam, nelems, elt_names, &
            elt_dims, elt_types, elt_bytes, elmnam, &
            celidt, wrilog, error, buffr)
        implicit none
        !
        integer(int_wp), dimension(:, :) :: elt_dims
        integer(int_wp), dimension(:) :: elt_bytes
        integer(int_wp) :: celidt, nelems, error
        !
        character(len = *), dimension(:) :: buffr
        character(len = *), dimension(:) :: elt_names, elt_types
        character(len = *) :: elmnam, filnam, grpnam
        !
        logical :: wrilog
        !
        save       fd_nef
        integer :: fd_nef = -1
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("putget_char1D", ithndl)
        !
        !-----initialization
        !
        coding = 'n'
        elmndm = 5
        do igr = 1, no_groups
            usrord(igr) = 1
            uindex(start, igr) = celidt
            uindex(stopp, igr) = celidt
            uindex(incr, igr) = 1
        end do
        !
        !-----aggregate file names
        !
        datnam = trim(filnam) // '.dat'
        call noextspaces(datnam, datlen)

        defnam = trim(filnam) // '.def'
        call noextspaces(defnam, deflen)
        !
        !-----write or read data from nefis files
        !
        if (wrilog) then
            access = 'u'
        else
            access = 'r'
        endif
        !
        error = crenef (fd_nef, datnam(1:datlen), defnam(1:deflen), &
                coding, access)
        if (error /= 0 .and. .not.wrilog) then
            error = -211
            goto 9999
        endif
        if (error /= 0) goto 9999

        if (wrilog) then
            error = putels(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
        else
            jnef = 0
            123    continue
            jnef = jnef + 1
            if (elmnam  ==  elt_names(jnef)) goto 124
            goto 123
            124    continue
            buflen = elt_bytes(jnef) ! size single precision integer
            do inef = 1, elt_dims(1, jnef)
                buflen = buflen * elt_dims(inef + 1, jnef)
            enddo
            error = getels(fd_nef, grpnam, elmnam, &
                    uindex, usrord, buflen, buffr)
            if (error /= 0) goto 9999
        endif
        !
        !-----error:
        !     writing: most likely error non existing group, so define it
        !     reading: error, no error expected
        !
        if (error  /=  0 .and. wrilog) then
            ! create elements
            do lelmnr = 1, nelems
                error = defelm(fd_nef, elt_names(lelmnr), &
                        elt_types(lelmnr), elt_bytes(lelmnr), &
                        ' ', ' ', &
                        ' ', elt_dims(1, lelmnr), &
                        elt_dims(2, lelmnr))
                !      most likely error, element already exist
                error = 0
            end do
            ! create cells
            error = defcel(fd_nef, grpnam, nelems, elt_names)
            if (error  /=  0) goto 9999
            ! create group on definition file
            error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
            if (error  /=  0) goto 9999
            ! create group on data       file
            error = credat(fd_nef, grpnam, grpnam)
            if (error  /=  0) goto 9999
            ! try again to write data
            error = putels(fd_nef, grpnam, elmnam, &
                    uindex, 1, buffr)
            if (error  /=  0) goto 9999
        endif
        !
        !     no error when reading elements
        !
        if (error == 0 .and. .not.wrilog) then
            error = inqelm(fd_nef, elmnam, elmtap, buflen, &
                    elmqta, elmant, elmdas, elmndm, elmdim)

            if (error   /=  0) goto 9999
            lelmnr = 0
            do nnef = 1, nelems
                if (elmnam  ==  elt_names(nnef)) then
                    lelmnr = nnef
                    goto 220
                endif
            end do
            220   continue
            if (lelmnr /= 0) goto 9999
            !
            do inef = 1, elmndm
                !
                !----------compare local and global dimensions, not equal
                !          => new error number and exit
                !
                if (elmdim(inef)  /=  elt_dims(1 + inef, lelmnr)) then
                    error = -15025
                    goto 9999
                endif
            end do
        endif
        goto 10000
        !
        9999 continue
        if (error  /=  0) ierror = neferr(1, errstr)
        10000 continue
        ierror = clsnef(fd_nef)
        !
        if (timon) call timstop (ithndl)
        return
    end subroutine putget_char1D

end module putget_mod

