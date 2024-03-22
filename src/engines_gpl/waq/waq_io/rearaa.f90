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
module m_rearaa
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    subroutine rearaa (lun, lchar, filtype, raname, nexcraai, &
            iexcraai, ioptraai, noraai, ntraaq, ioutpt, &
            ierr, status)

        !       Deltares Software Centre

        !>\file
        !>                        Reads monitoring transects
        !>
        !>           - Reads an option\n
        !>             This can be:
        !>             - -1 for an external ASCII file, superfluous because of INCLUDE support
        !>             - 1 for continuation on this file
        !>             - 2 to indicate that no monitoring transects are used
        !>           - The amount of monitoring transects
        !>           - Per monitoring transect:
        !>             - the name (20 characters) of the transect
        !>             - the option for transect accumulations. This can be:
        !>               - 1 indicating that the net transport should be taken
        !>               - 2 indicating that only the positive fluxes should be taken
        !>               - 3 indicating that only the negative fluxes should be taken
        !>             - the number of exchanges included in the transect
        !>             - that many exchange numbers (negative if reversed sign is wanted)

        !     Created           : September 1995  by Jan van Beek

        !     Modified          : April     1997 by R. Bruinsma: Tokenized data file reading added
        !                         July      2002 by Leo Postma : Call to Opt1 changed.
        !                         May       2011 by Leo Postma : Fortran-90 look and feel

        !     Subroutine called : OPT1   -
        !                         ZOEK   - to searchs strings

        !     Logical units     : LUN(27) = unitnumber stripped DELWAQ input file
        !                         LUN(29) = unitnumber formatted output file

        use m_opt1
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset

        implicit none

        !     parameters

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(inout) :: lun     (*)        !< array with unit numbers
        character(*), intent(inout) :: lchar   (*)       !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype (*)        !< type of binary file
        character(20), pointer :: raname  (:)       !< name of monitoring areas
        integer(kind = int_wp), pointer :: nexcraai(:)        !< number of exchanges per monitoring transect
        integer(kind = int_wp), pointer :: iexcraai(:)        !< exchange numbers per monitoring transect
        integer(kind = int_wp), pointer :: ioptraai(:)        !< transport summation option for transects
        integer(kind = int_wp), intent(out) :: noraai             !< number of monitoring transects
        integer(kind = int_wp), intent(out) :: ntraaq             !< total number of exchanges in monitoring transects
        integer(kind = int_wp), intent(in) :: ioutpt             !< flag for more or less output
        integer(kind = int_wp), intent(inout) :: ierr               !< error   count

        type(error_status), intent(inout) :: status !< current error status

        !     local variables

        integer(kind = int_wp) :: iropt1            ! First option number monitoring areas
        integer(kind = int_wp) :: ierr2             ! Local error flag
        integer(kind = int_wp) :: max_ntraaq        ! Size of isegdmp
        integer(kind = int_wp), pointer :: iexcraai_2(:)     ! Help pointer for array expansion
        integer(kind = int_wp) :: ierr_alloc        ! Error indicator for allocations
        integer(kind = int_wp) :: nq                ! Number of exchanges per monitoring transect
        logical             ldummy                ! Dummy logical
        integer(kind = int_wp) :: ir                ! Loop variable over all monitoring transects
        integer(kind = int_wp) :: k                 ! General loop variable
        character(len = 256) :: option                ! balance option
        integer(kind = int_wp) :: itype             ! type of the returned token
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("rearaa", ithndl)

        !     Read file option

        if (gettoken(iropt1, ierr2) > 0) goto 20
        select case (iropt1)
        case (:-2)
            write (lunut, 2000)  iropt1
            write (lunut, 2010)
            goto 20
        case (-1)                     ! old style <other ASCII file>
            write (lunut, 2000)  iropt1
            call opt1   (iropt1, lun, 0, lchar, filtype, &
                    ldummy, ldummy, 0, ierr2, status, &
                    .false.)
            if (ierr2 > 0) goto 20
            if (gettoken(noraai, ierr2) > 0) goto 20
        case (0)                      ! new style (October 2012) no dump transects
            write (lunut, 2020)       ! old style would have produced an error
            noraai = 0
            ntraaq = 0
            goto 30
        case (1)                      ! old style <this input file> or new style 1 transect
            if (gettoken(option, noraai, itype, ierr2) > 0) goto 20
            if (itype == 1) then     ! character so: new style, 1 transect
                push = .true.
                noraai = iropt1
            else
                write (lunut, 2000)  iropt1
            endif
        case (2)                      ! old style <no dump transects> or new style 2 transects
            if (gettoken(option, noraai, itype, ierr2) > 0) goto 20
            push = .true.                ! look to see what will be next
            if (itype == 1) then     ! a string, so first dump-ID from 2 areas
                call convert_string_to_time_offset (option, noraai, .false., .false., ierr2)
                if (ierr2 /= 0) then
                    noraai = iropt1
                else
                    write (lunut, 2000)  iropt1
                    write (lunut, 2020)
                    noraai = 0
                    ntraaq = 0
                    goto 30
                endif
            else                         ! an integer, so 2 meant <not used>
                write (lunut, 2020)
                noraai = 0
                ntraaq = 0
                goto 30
            endif
        case default                    ! new processing
            noraai = iropt1

        end select

        !        Write number of dump transects, allocate arrays

        write(lunut, 2030) noraai
        ntraaq = 0
        allocate (raname  (noraai), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2380) ierr_alloc
            goto 20
        endif
        allocate (nexcraai(noraai), ioptraai(noraai), &
                iexcraai(noraai * 2), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write (lunut, 2390) ierr_alloc
            goto 20
        endif
        max_ntraaq = noraai * 2

        !        Read specification of the transects

        if (ioutpt < 2) write (lunut, 2040)
        if (ioutpt == 2) write (lunut, 2050)
        do ir = 1, noraai
            if (gettoken(raname  (ir), ierr2) > 0) goto 20
            if (gettoken(ioptraai(ir), ierr2) > 0) goto 20
            if (gettoken(nq, ierr2) > 0) goto 20
            if (ntraaq + nq > max_ntraaq) then
                max_ntraaq = 2 * (ntraaq + nq)
                allocate (iexcraai_2(max_ntraaq), stat = ierr_alloc)
                if (ierr_alloc /= 0) then
                    write (lunut, 2400) ierr_alloc
                    goto 20
                endif
                iexcraai_2(1:ntraaq) = iexcraai(1:ntraaq)
                deallocate(iexcraai)
                iexcraai => iexcraai_2
            endif
            do k = 1, nq
                if (gettoken(iexcraai(ntraaq + k), ierr2) > 0) goto 20
            enddo
            if (raname(ir) == ' ') write(raname(ir), '(''Transect-id'',i6)') ir

            ! check if name is unique

            do k = 1, ir - 1
                if (string_equals(raname(ir), raname(k))) then
                    write(lunut, 2410) raname(ir)
                    ierr = ierr + 1
                endif
            enddo

            if (ioutpt >= 2) then
                write(lunut, 2060) ir, raname(ir), ioptraai(ir), nq
                if (ioutpt >= 3) then
                    write(lunut, 2070)
                    write(lunut, 2080) (k, iexcraai(ntraaq + k), k = 1, nq)
                endif
            endif

            ntraaq = ntraaq + nq
            nexcraai(ir) = nq

        end do
        goto 30

        !     Error handling

        20 write (lunut, 2500)
        ierr = ierr + 1
        30 if (timon) call timstop(ithndl)
        return

        2000 format (/, ' Transects:', &
                / ' option selected for input :', I2)
        2010 format (/ ' ERROR, option for transects not implemented !!!!')
        2020 format (' No transects used.')
        2030 format (/ ' Number of transects :', I5)
        2040 format (' Information on transects will only be printed ', &
                'for output option 2 and higher !')
        2050 format (' Composition of transects will only be printed ', &
                'for output option 3 and higher !')
        2060 format (/ ' Transect :', I5, ' : ', A20, &
                / ' Option for transport accumulation    :', I15, &
                / ' Number of exchanges for this transect:', I15)
        2070 format ('          number   exchange')
        2080 format (2I15)
        2380 format (/, ' ERROR. allocating memory for transect names:', I8)
        2390 format (/, ' ERROR. allocating memory for transect exch.:', I8)
        2400 format (/, ' ERROR reallocating memory for transect exch.:', I8)
        2410 format (/, ' ERROR. observation ID not unique:', A)
        2500 format (/, ' ERROR. while reading transects')
    end

end module m_rearaa
