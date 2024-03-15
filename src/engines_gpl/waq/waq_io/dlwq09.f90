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
module m_dlwq09
    use m_waq_precision
    use m_rdodef
    use m_outboo
    use m_opt1
    use m_getopo
    use m_error_status

    implicit none

contains


    subroutine dlwq09 (lun, lchar, filtype, car, iar, &
            icmax, iimax, iwidth, &
            ioutpt, ioutps, outputs, status)
        !> Defines variables for output per available output file

        use m_defout
        use m_check
        use m_rdwrk4
        use m_open_waq_files
        use rd_token     !   for the reading of tokens
        use results, only : OutputPointers, lncout
        use timers       !   performance timers
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics

        implicit none

        !     kind           function         name                Description

        integer(kind = int_wp), intent(inout) :: lun   (*)          !< array with unit numbers
        character(*), intent(inout) :: lchar (*)         !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)         !< type of binary file
        integer(kind = int_wp), intent(in) :: icmax              !< size of the character workspace
        character(20), intent(inout) :: car   (icmax)     !< character workspace
        integer(kind = int_wp), intent(inout) :: iar   (*)          !< integer workspace ( dump locations at entrance )
        integer(kind = int_wp), intent(in) :: iimax              !< size of the integer workspace
        integer(kind = int_wp), intent(in) :: iwidth             !< width of the output file
        integer(kind = int_wp), intent(in) :: ioutpt             !< flag for more or less output
        integer(kind = int_wp), intent(out) :: ioutps(7, noutp)    !< output administration array
        type(OutputPointers)                Outputs           !< output collection

        type(error_status), intent(inout) :: status !< current error status

        !     Local

        integer(kind = int_wp) :: nrvar (noutp)  ! Number of extra output vars
        integer(kind = int_wp) :: iostrt(noutp)  ! Output start time (scu)
        integer(kind = int_wp) :: iostop(noutp)  ! Output stop time (scu)
        integer(kind = int_wp) :: iostep(noutp)  ! Output step time (scu)
        integer(kind = int_wp) :: isrtou(noutp)  ! Sort output indication
        integer(kind = int_wp) :: igrdou(noutp)  ! Output grid indication
        character(40)                 modid (4)     ! Model and run-ID
        character(20), allocatable :: sysid (:)     ! Systems ID
        character(20), allocatable :: coname(:)     ! Constant names
        character(20), allocatable :: paname(:)     ! Parameter names
        character(20), allocatable :: funame(:)     ! Function names
        character(20), allocatable :: sfname(:)     ! Segment function names
        character(20), allocatable :: diname(:)     ! Dispersion array names
        character(20), allocatable :: vename(:)     ! Velocity array names
        integer(kind = int_wp) :: noqtt          ! all exchanges inclusive of the layered bed
        integer(kind = int_wp) :: nosss          ! all computational cells inclusive of layered bed
        integer(kind = int_wp) :: nrvarm         ! maximum number of variables that fits in array size
        integer(kind = int_wp) :: itype          ! return variable for get_token call
        integer(kind = int_wp) :: ierr2          ! local error variable
        integer(kind = int_wp) :: iopt1          ! input file option
        integer(kind = int_wp) :: nmis           ! number of unresolved variables
        integer(kind = int_wp) :: iv, ip         ! help variables to pointer in array
        integer(kind = int_wp) :: i              ! loop variable
        integer(kind = int_wp) :: ivar           ! cumulative variable counter

        logical :: ibflag !< mass balance option flag
        LOGICAL       INFILE, LMOUTP, LDOUTP, LHOUTP
        LOGICAL       LDUMMY
        character*255 lchloc            ! Local character variable for file name
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("dlwq09", ithndl)

        ! Some init
        ibflag = (mod(intopt, 16) > 7)
        NOQTT = NOQ + NOQ4
        NOSSS = NOSEG + NSEG2 ! with or without bottom
        LUNUT = LUN(29)
        IF (IMSTRT <= ITSTOP .AND. IMSTRT <= IMSTOP .AND. IMSTEP > 0) THEN
            LMOUTP = .TRUE.
        ELSE
            LMOUTP = .FALSE.
        ENDIF
        IF (IDSTRT <= ITSTOP .AND. IDSTRT <= IDSTOP .AND. IDSTEP > 0) THEN
            LDOUTP = .TRUE.
        ELSE
            LDOUTP = .FALSE.
        ENDIF
        IF (IHSTRT <= ITSTOP .AND. IHSTRT <= IHSTOP .AND. IHSTEP > 0 .AND. (INTSRT <= 5 .OR.INTSRT >= 10)) THEN
            LHOUTP = .TRUE.
        ELSE
            LHOUTP = .FALSE.
        ENDIF

        ! Test voor steady state opties
        IF ((INTSRT > 5 .AND.  INTSRT < 10) .OR. INTSRT == 17 .OR. INTSRT == 18) THEN
            LMOUTP = .TRUE.
            LDOUTP = .TRUE.
            LHOUTP = .TRUE.
            IMSTRT = ITSTRT
            IDSTRT = ITSTRT
            IHSTRT = ITSTRT
            IMSTOP = ITSTRT + 1
            IDSTOP = ITSTRT + 1
            IHSTOP = ITSTRT + 1
            IMSTEP = 1
            IDSTEP = 1
            IHSTEP = 1
        ENDIF

        !     Determine local maximum

        nrvarm = min(iimax, icmax) / noutp
        allocate (sysid (notot), coname(nocons), paname(nopa))
        allocate (funame(nofun), sfname(nosfun), diname(nodisp))
        allocate (vename(novelo))

        !     Set default action

        call defout (noutp, nrvar, iostrt, iostop, iostep, isrtou, igrdou)

        !     Handle file option

        ierr2 = gettoken(lchloc, iopt1, itype, ierr2)          !  < -1 not
        if (itype == 2) then                                 !    -1 external placed on the stack by opt1
            write (lunut, 2000) iopt1                          !     0 not
            infile = .true.                                       !     1 this file, no action
        else       !        "old" file no block 9                !     2 added here for "no file"
            infile = .false.                                       !  >  2 not
            iopt1 = 2
        endif
        if (iopt1 < -1 .or. iopt1 == 0 .or. iopt1 > 2) then
            write (lunut, 2010)        !        option out of range
            ierr2 = 1
            goto 100
        endif
        if (iopt1 == 2) then
            infile = .false.
            write (lunut, 2020)
        else                             !        Handle option -1 and 1
            call opt1   (iopt1, lun, 18, lchar, filtype, &
                    ldummy, ldummy, 0, ierr2, status, &
                    .false.)
            if (ierr2 > 0) goto 100
        endif

        !        Read output definition block

        call rdodef (noutp, nrvar, nrvarm, isrtou, car, &
                infile, nx, ny, nodump, ibflag, &
                lmoutp, ldoutp, lhoutp, lncout, status, &
                igrdou, ndmpar)

        !     Calculate OUTPUT boot variables NVART, NBUFMX

        call outboo (noutp, nrvar, igrdou, isrtou, nosss, &
                nodump, nx, ny, nrvart, nbufmx, &
                ndmpar, notot, ncbufm, noraai)

        !     If extra ouptut parameters requested set the pointers

        if (nrvart > 0) then

            !        Only if no previous errors , otherwise the reading will fail

            if (status%ierr == 0) then

                !           Read part of delwaq file

                call open_waq_files(lun(2), lchar(2), 2, 2, ierr2)
                call rdwrk4(lun(2), lunut, modid, sysid, notot, &
                        nodump, nosys, nobnd, nowst, nocons, &
                        nopa, noseg, nseg2, coname, paname, &
                        funame, nofun, sfname, nosfun, nodisp, &
                        novelo, diname, vename, iar, iar, &
                        ndmpar, ntdmpq, ntdmps, noqtt, noraai, &
                        ntraaq, nobtyp, nowtyp, nogrid, iar, &
                        iar, iar)
                close (lun(2))

                !           Get output pointers

                call getopo(noutp, nrvar, nrvarm, car, iar, &
                        nmis, notot, sysid, nocons, coname, &
                        nopa, paname, nofun, funame, nosfun, &
                        sfname, lunut)

                !           If not all vars found, set error

                if (nmis > 0) then
                    write(lunut, *) ' Not all variables available.'
                    write(lunut, *) ' Number off missing variables :', nmis
                endif
            else
                write (lunut, 2040)
                call status%increase_warning_count()
            endif
        endif

        !     Write OUTPUT intermediate file

        do i = 1, noutp
            ioutps(1, i) = iostrt(i)
            ioutps(2, i) = iostop(i)
            ioutps(3, i) = iostep(i)
            ioutps(4, i) = nrvar (i)
            ioutps(5, i) = isrtou(i)
            ioutps(6, i) = igrdou(i)
        enddo

        allocate(Outputs%names(nrvart), Outputs%pointers(nrvart), Outputs%std_var_name(nrvart), &
                Outputs%units(nrvart), Outputs%description(nrvart))
        Outputs%cursize = nrvart

        ivar = 0
        do i = 1, noutp
            do iv = 1, nrvar(i)
                ivar = ivar + 1
                ip = (i - 1) * nrvarm + iv
                Outputs%pointers(ivar) = iar(ip)
                Outputs%names   (ivar) = car(ip)
                Outputs%std_var_name(ivar) = car(ip)
                Outputs%units   (ivar) = ' '
                Outputs%description  (ivar) = ' '
            enddo
        enddo

        100 if (infile) then
            call check  (lchloc, iwidth, 9, ierr2, status)
        else
            if (iwidth == 5) then
                write (lunut, 2060) 9
            else
                write (lunut, 2070) 9
            endif
        endif
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (//, ' Option selected for output specification :', I4)
        2010 format (/, ' ERROR, option not implemented')
        2020 format (/, ' Output not specified, using default output parameters')
        2040 format (/, ' WARNING, Not able to locate extra output variables', &
                /, '          because of errors in input')
        2060 format (/1X, 59('*'), ' B L O C K -', I2, ' ', 5('*')/)
        2070 format (/1X, 109('*'), ' B L O C K -', I2, ' ', 5('*')/)

    end

end module m_dlwq09
