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
module initial_conditions
    use m_waq_precision
    use m_error_status

    implicit none

    private
    public :: read_initial_conditions

contains


    subroutine read_initial_conditions(lun, lchar, filtype, inpfil, notot, &
            syname, iwidth, output_verbose_level, gridps, noseg, &
            conc, ierr, status)

        !! read block 8 of input, initial condtions keyword type of input

        use m_read_block
        use error_handling, only : check_error
        use m_srstop
        use dlwqgrid_mod          ! for the storage of contraction grids
        use dlwq_hyd_data  ! for definition and storage of data
        use rd_token
        use timers       !   performance timers

        integer(kind = int_wp), intent(inout) :: lun(*)        ! unit numbers used
        character(len = *), intent(inout) :: lchar(*)     ! filenames
        integer(kind = int_wp), intent(inout) :: filtype(*)    !< type of binary file
        type(inputfilestack), intent(inout) :: inpfil       ! input file strucure with include stack and flags
        integer(kind = int_wp), intent(in) :: notot         ! nr of substances
        character(len = *), intent(in) :: syname(*)    ! substance names
        integer(kind = int_wp), intent(in) :: iwidth        ! width of output
        integer(kind = int_wp), intent(in) :: output_verbose_level        ! level of reporting to ascii output file
        type(gridpointercoll), intent(in) :: gridps       ! collection off all grid definitions
        integer(kind = int_wp), intent(in) :: noseg         ! nr of segments
        real(kind = real_wp), intent(inout) :: conc(notot, noseg)   ! initial conditions
        integer(kind = int_wp), intent(inout) :: ierr          !< cummulative error count

        type(error_status), intent(inout) :: status !< current error status

        !     local declarations

        type(t_dlwqdatacoll) :: initials             ! all the initial data from file
        type(t_dlwqdata) :: dlwqdata             ! one data block
        type(t_dlwq_item) :: substances           ! delwaq substances list
        type(t_dlwq_item) :: constants            ! delwaq constants list
        type(t_dlwq_item) :: parameters           ! delwaq parameters list
        type(t_dlwq_item) :: functions            ! delwaq functions list
        type(t_dlwq_item) :: segfuncs             ! delwaq segment-functions list
        type(t_dlwq_item) :: segments             ! delwaq segment name list
        real(kind = real_wp), allocatable :: fscale(:)             ! scale factors
        character(len = 255) :: ctoken               ! token from input
        integer(kind = int_wp) :: idata                 ! index in collection
        integer(kind = int_wp) :: itype                 ! type of the token
        integer(kind = int_wp) :: ierr2                 ! error from gettoken and others
        integer(kind = int_wp) :: ierr3                 ! error from dlwqdataevaluate
        integer(kind = int_wp) :: itime                 ! dummy time
        integer(kind = int_wp) :: i                     ! loop counter
        integer(kind = int_wp) :: idummy                ! dummy
        real(kind = real_wp) :: rdummy                ! dummy
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_initial_conditions", ithndl)


        ! read initial conditions

        initials%maxsize = 0
        initials%cursize = 0
        ierr2 = dlwq_init(substances)
        ierr2 = dlwq_resize(substances, notot)
        substances%no_item = notot
        substances%name(1:notot) = syname(1:notot)
        ierr2 = dlwq_init(constants)
        ierr2 = dlwq_init(parameters)
        ierr2 = dlwq_init(functions)
        ierr2 = dlwq_init(segfuncs)
        ierr2 = dlwq_init(segments)
        ierr2 = dlwq_resize(segments, noseg)
        segments%no_item = noseg
        do i = 1, noseg
            write (segments%name(i), '(''segment '',i8)') i
        enddo

        lunut = lun(29)
        ierr2 = 0

        do

            if (gettoken(ctoken, idummy, rdummy, itype, ierr2) /= 0) exit

            if (ctoken == 'INITIALS') then

                ! new file strucure
                push = .true.
                call read_block (lun, lchar, filtype, inpfil, output_verbose_level, &
                        iwidth, substances, constants, parameters, functions, &
                        segfuncs, segments, gridps, dlwqdata, ierr2, &
                        status)
                if (ierr2 > 0) goto 30
                if (dlwqdata%extern) then
                    ierr = dlwqdataReadExtern(lunut, dlwqdata)
                    if (ierr /= 0) goto 30
                    dlwqdata%extern = .false.
                endif
                idata = dlwqdatacolladd(initials, dlwqdata)

            else
                ! unrecognised keyword
                if (ctoken(1:1) /= '#') then
                    write (lunut, 2050) trim(ctoken)
                    ierr = ierr + 1
                    goto 30
                else
                    ierr2 = 2
                    exit
                endif
            endif

            ! jvb? first_token = .false.

        enddo

        ! do not write but evaluate and pass back
        conc = 0.0
        itime = 0

        do idata = 1, initials%cursize
            ierr3 = dlwqdataevaluate(initials%dlwqdata(idata), gridps, itime, notot, noseg, conc)
            if (ierr3 /= 0) then
                write(lunut, 2060)
                call srstop(1)
            endif
        enddo

        !     initials opruimen ? or keep for e.g. reboot, or keep for delwaq1-delwaq2 merge

        ierr3 = dlwq_cleanup(substances)
        ierr3 = dlwq_cleanup(parameters)
        ierr3 = dlwq_cleanup(functions)
        ierr3 = dlwq_cleanup(segfuncs)
        ierr3 = dlwq_cleanup(segments)

        30 continue
        if (ierr2 > 0 .and. ierr2 /= 2) ierr = ierr + 1
        if (ierr2 == 3) call srstop(1)
        call check_error(ctoken, iwidth, 8, ierr2, status)
        if (timon) call timstop(ithndl)
        return

        2050 format (/' ERROR, unrecognized token: ', A)
        2060 format (/' ERROR: evaluating initial conditions')
    end subroutine read_initial_conditions

end module initial_conditions
