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
module m_opt0
    use m_waq_precision
    use m_opt3
    use m_opt2
    use m_error_status

    implicit none

contains


    subroutine opt0   (lun, is, noql1, noql2, noql3, &
            ndim2, ndim3, nrftot, nrharm, ifact, &
            dtflg1, disper, volume, iwidth, lchar, &
            filtype, dtflg3, ioutpt, ierr, &
            status, dont_read)

        !       Deltares Software Centre

        !>\file
        !>                          Reads a block with constant or time variable data
        !>\par  Description:
        !>                          This is a main data aquisition sub system, it is
        !>                          the only call to read:
        !>                          - volumes ( in dlwq03 )
        !>                          - additional dispersions ( in dlwq04 )
        !>                          - additional velocities ( in dlwq04 )
        !>                          - areas ( in dlwq04 )
        !>                          - flows ( in dlwq04 )
        !>                          - mixing lengthes ( in dlwq04 )
        !>                          - old style open boundaries ( in dlwq05 )
        !>                          - old style waste loads ( in dlwq06 )
        !>                          Dlwq07 is a sort of dedicated verion of this routine
        !>                          to read parameters and functions and segment functions\n
        !>                          Dlwq08 is a sort of dedicated verion of this routine
        !>                          to read initial conditions


        !     Subroutines called : opt1    get & open include file
        !                          opt2    read constants ( << (include) file)
        !                          opt3    read time dep  ( << (include) file)
        !                          open_waq_files  open file

        !     Functions called   : gettok  tokenized data file reading

        !     Logical units      : lun(27) = unit stripped DELWAQ input file
        !                          lun(28) = stripped workfile
        !                          lun(29) = unit formatted output file
        !                          lun( 2) = unit intermediate file (system)
        !                          lun( 3) = unit intermediate file (harmos)
        !                          lun( 4) = unit intermediate file (pointers)
        !                          lun(is) = unit intermediate file (items)

        use m_opt1
        use m_open_waq_files
        use timers       !   performance timers
        use rd_token
        use m_sysn          ! System characteristics

        implicit none

        !     Parameters

        !     kind           function         name           Descriptipon

        integer(kind = int_wp), intent(inout) :: lun    (*)     !< array with unit numbers
        integer(kind = int_wp), intent(in) :: is             !< entry in lun for this call
        integer(kind = int_wp), intent(in) :: noql1          !< number of exchanges 1st direction
        integer(kind = int_wp), intent(in) :: noql2          !< number of exchanges 2nd direction
        integer(kind = int_wp), intent(in) :: noql3          !< number of exchanges 3rd direction
        integer(kind = int_wp), intent(in) :: ndim2          !< number of items per block
        integer(kind = int_wp), intent(in) :: ndim3          !< number of scale factors
        integer(kind = int_wp), intent(inout) :: nrftot         !< number of functions
        integer(kind = int_wp), intent(inout) :: nrharm         !< number of harmonics
        integer(kind = int_wp), intent(in) :: ifact          !< factor between time scales
        logical, intent(in) :: dtflg1        !< 'date'-format 1st time scale
        logical, intent(in) :: disper        !< .true. then dispersion
        integer(kind = int_wp), intent(inout) :: volume         !< if 1 then volume ( out: 0 = computed volumes )
        integer(kind = int_wp), intent(in) :: iwidth         !< width of the output file
        character(*), intent(inout) :: lchar  (*)    !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)     !< type of binary file
        logical, intent(in) :: dtflg3        !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: ioutpt         !< how extensive is output ?
        integer(kind = int_wp), intent(inout) :: ierr           !< cumulative error count
        logical, intent(in) :: dont_read     !< do not actually read tokens, if true, the information is already provided

        type(error_status), intent(inout) :: status !< current error status

        logical        bound       !  if .true. then boundary call
        logical        waste       !  if .true. then waste call
        logical        skip        !  if .true. then waste call with skip
        integer(kind = int_wp) :: iopt1        !  first  option ( type of file e.g. 0 = binary file )
        integer(kind = int_wp) :: iopt2        !  second option ( 1,2 = constant, 3 = time varying )
        integer(kind = int_wp) :: ndim1        !  sum of input in 3 directions
        integer(kind = int_wp) :: ndtot        !  total size of matrix (ndim1*ndim2)
        integer(kind = int_wp) :: ierr2        !  local error flag
        integer(kind = int_wp) :: itype        !  to identify the data type read
        integer(kind = int_wp) :: idummy       !  work integer ( = 0 )
        real(kind = real_wp) :: adummy       !  work real    ( = 0.0 )
        character(128) cdummy      !  work character
        integer(kind = int_wp) :: k            !  loop counter
        real(kind = real_wp) :: disp(3, 1)    !  dispersions in 3 directions
        real(kind = real_wp), allocatable :: values(:, :)  ! read buffer for the values
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("opt0", ithndl)

        idummy = 0
        adummy = 0.0
        ndim1 = noql1 + noql2 + noql3
        ndtot = ndim1 * ndim2
        bound = .false.
        waste = .false.
        skip = .false.

        select case (ierr)
        case (-1)
            bound = .true.
            waste = .false.
            skip = .false.
        case (-2)
            bound = .false.
            waste = .true.
            skip = .false.
        case (:-3)
            bound = .false.
            waste = .true.
            skip = .true.
        end select
        ierr = 0
        if (skip) goto 10

        !        Read first option, write zero dispersion if OPT1=0

        if (dont_read) then
            iopt1 = -2
            if (is == 13) then
                iopt1 = 0 ! Ugly hack, all other files are time-dependent
            endif
        else
            if (gettoken(cdummy, iopt1, itype, ierr2) > 0) goto 50
            if (itype == 1) then
                if (volume /= 1) then
                    write (lunut, 2070) cdummy
                    ierr2 = 1
                    goto 50
                else
                    if (cdummy == 'FRAUD') then
                        volume = -1
                        write (lunut, 2080)
                        if (gettoken(iopt1, ierr2) > 0) goto 50
                    else
                        write (lunut, 2090) cdummy
                        ierr2 = 1
                        goto 50
                    endif
                endif
            endif
        endif

        write (lunut, 2000) iopt1
        call opt1   (iopt1, lun, is, lchar, filtype, &
                dtflg1, dtflg3, ndtot, ierr2, status, &
                dont_read)
        if (ierr2 > 0) goto 50

        !        Binary file, option = -2 (or sequence of binary files option = -4)
        !                           everything is block function, except volume

        if (iopt1 == -2 .or. iopt1 == -4) then
            nlines = nlines + ndim1 * ndim2 * 2
            npoins = npoins + ndim1 + 3
            nrftot = ndim1 * ndim2
            nrharm = 0
            if (volume == 1) then
                write(lun(4)) (k, k = 1, ndim1), (idummy, k = 1, 3)
            else
                write(lun(4)) (-k, k = 1, ndim1), (idummy, k = 1, 3)
            endif
            iopt1 = 0
        endif

        !        Dispersion in three directions if DISPER, return if NODISP=0

        if (disper) then
            if (iopt1 == 0) then                            ! binary file, then
                write (lun(2)) idummy, (adummy, k = 1, 3)     ! no fixed dispersions
            else
                write (lun(2)) idummy
                call opt2 (1, disp, 1, 3, 3, &
                        iwidth, lun(2), ioutpt, ierr2)
                if (ierr2 > 0) goto 50
                if (ndim2 == 0) goto 9999
            endif
        endif

        if (iopt1 == 0) goto 9999                          ! binary file, we are ready

        !        Read second option, set volume flag if OPT2 > 3 AND VOLUME

        10 if (gettoken(iopt2, ierr2) > 0) goto 50
        write (lunut, 2010) iopt2
        if (volume == 1 .and. iopt2 > 3) then                  ! Computed volumes !!
            volume = 0
            iopt2 = iopt2 - 3
        endif

        !        Get the data

        select case (iopt2)
        case (1, 2)              !   Constants with and without defaults in three directions
            allocate (values(ndim2, max(noql1, noql2, noql3)))
            call open_waq_files (lun(is), lchar(is), is, 1, ierr2)
            write (lun(is)) idummy
            if (noql1 > 0) write (lunut, 2030)
            call opt2 (iopt2, values, noql1, ndim2, ndim3, &
                    iwidth, lun(is), ioutpt, ierr2)
            if (ierr2 > 0) goto 50

            if (noql2 > 0) write (lunut, 2040)
            call opt2 (iopt2, values, noql2, ndim2, ndim3, &
                    iwidth, lun(is), ioutpt, ierr2)
            if (ierr2 > 0) goto 50

            if (noql3 > 0 .and. noql3 /= ndim1) write (lunut, 2050)
            call opt2 (iopt2, values, noql3, ndim2, ndim3, &
                    iwidth, lun(is), ioutpt, ierr2)
            close (lun(is))
            if (ierr2 > 0) goto 50

        case (3)                 !   Time varying data
            ierr2 = 0
            if (bound) ierr2 = -1
            if (waste) ierr2 = -2
            call opt3 (lun, lchar, is, ndim1, ndim2, &
                    ndim3, ifact, dtflg1, dtflg3, nrftot, &
                    nrharm, iwidth, ioutpt, ierr2)
            if (ierr2 > 0) goto 50

        case default
            write (lunut, 2020)
            goto 50

        end select
        9999 if (timon) call timstop(ithndl)
        return

        50 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (/, ' First  selected option   : ', I7)
        2010 format (' Second selected option   : ', I7)
        2020 format (/, ' ERROR. Option not implemented !!!!!!')
        2030 format (/, ' First  direction:')
        2040 format (/, ' Second direction:')
        2050 format (/, ' Third  direction:')
        2070 format (/, ' ERROR. No character string allowed: ', A)
        2080 format (' Keyword FRAUD found for fraudulent computations.')
        2090 format (/, ' ERROR. This keyword is not allowed here: ', A)

    end

end module m_opt0
