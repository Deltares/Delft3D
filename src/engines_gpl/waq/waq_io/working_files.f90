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
module m_working_files
    use m_waq_precision

    implicit none

    private
    public :: create_work_file_one, read_working_file_4

contains


    subroutine create_work_file_one(lun, lchar, nolun, runid)
        !! Reads the input filename* ( keyboard /command line ) ;
        !! sets filenames* ; opens system files
        !! the subroutine creates the following files lst, delwaq04.wrk, harmonic.wrk, pointers.wrk, filenaam.wrk files
        !! Logical units     : 5       = keyboard
        !!                     lun(26) = unit user input file
        !!                     lun(27) = unit stripped input file
        !!                     lun(29) = unit formatted output file
        !!                     lun( 2) = unit system-intermediate file
        !!                     lun( 3) = unit intermediate file (harmonics)
        !!                     lun( 4) = unit intermediate file (pointers)

        use m_srstop
        use m_monsys
        use m_cli_utils, only : retrieve_command_argument, get_input_filename
        use m_get_filepath_and_pathlen
        use m_open_waq_files
        use timers
        use data_processing, only : delete_file

        implicit none

        integer(kind = int_wp), intent(in) :: nolun           !< Amount of unit numbers
        integer(kind = int_wp), intent(inout) :: lun(nolun)      !< Unit numbers
        character(*), intent(inout) :: lchar(nolun)    !< File names
        character(*), intent(inout) :: runid           !< Runid

        ! Local

        integer(kind = int_wp) :: ilun
        integer(kind = int_wp) :: ioerr
        character(len=93) :: check
        logical :: specout
        integer(kind = int_wp) :: idummy
        real (kind = real_wp) :: rdummy
        character(len=256) :: outputpath
        character(len=256) :: outputpath2
        character(len=256) :: runidpath
        integer(kind = int_wp) :: pathlen
        integer(kind = int_wp) :: outpathlen
        character(len=256) :: outid
        integer(kind = int_wp) :: ierr2

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("create_work_file_one", ithndl)

        ! Get filename  ( keyboard / command line )

        check = lchar(29)
        call get_input_filename(runid, check)

        ! Specific output dir?
        call retrieve_command_argument ('-output', 3, specout, idummy, rdummy, outputpath, ierr2)
        if (specout) then
            if (ierr2==0) then
                write (*, '(A)') 'Found -output switch with the following path:'
                write (*, '(/A)') trim(outputpath)
                write (*, '(/A/)') 'Make sure this path exists, or DELWAQ will not run!'
                call get_filepath_and_pathlen (runid, runidpath, pathlen)
                call get_filepath_and_pathlen (outputpath, outputpath2, outpathlen)
                if (outpathlen == 0 .or. outputpath(1:1) == '.') then
                    ! No dir indicators found or it starts with a dot, asume it is a local subdir
                    outputpath = trim(runidpath) // trim(outputpath)
                endif
                if (pathlen == 0) then
                    outid = trim(outputpath) // '/' // runid
                else
                    outid = trim(outputpath) // '/' // trim(runid(pathlen + 1:))
                endif
            else
                write (*, '(A/)') 'Found -output switch but not path specified. This will be ignored.'
                specout = .false.
            endif
        endif

        !  Pad the model name in the file names
        do ilun = 1, nolun
            if (specout .and. index(lchar(ilun), '.wrk') == 0 .and. index(lchar(ilun), '.inp') == 0) then
                lchar(ilun) = trim(outid) // lchar(ilun)
            else
                lchar(ilun) = trim(runid) // lchar(ilun)
            endif
        enddo

        ! Remove any existing work files

        do ilun = 1, nolun
            if (index(lchar(ilun), '.wrk') > 0) call delete_file(lchar(ilun), ioerr)
        enddo

        ! Open the neccessary unit numbers
        ! create the lst file
        call open_waq_files(lun(29), lchar(29), 29, 1, ioerr)
        !
        call setmlu(lun(29))
        ! open the input file (.inp)
        call open_waq_files(lun(26), lchar(26), 26, 1, ioerr)
        if (ioerr > 0) then
            write (lun(29), 1000) lun(26), lchar(26)
            call srstop (1)
        endif
        ! create the delwaq04.wrk binary file
        call open_waq_files(lun(2), lchar(2), 2, 1, ioerr)
        ! create the harmonic.wrk file
        call open_waq_files(lun(3), lchar(3), 3, 1, ioerr)
        ! create the pointers.wrk file
        call open_waq_files(lun(4), lchar(4), 4, 1, ioerr)
        ! create the filenaam.wrk file
        call open_waq_files(lun(41), lchar(41), 41, 1, ioerr)

        if (timon) call timstop(ithndl)
        return

        ! Output formats

        1000 format (/' ERROR input file on unit:', I3, &
                /' Filename = ', A, &
                /' Does not exist.', &
                /' EXECUTION HALTED !!!!!!!!!!!!!')
    end subroutine create_work_file_one

    subroutine read_working_file_4 (iin, lurep, modid, sysid, notot, &
            nodump, nosys, nobnd, nowst, nocons, &
            nopa, noseg, nseg2, coname, paname, &
            funame, nofun, sfname, nosfun, nodisp, &
            novelo, diname, vename, idpnt, ivpnt, &
            ndmpar, ntdmpq, ntdmps, noqtt, noraai, &
            ntraaq, nobtyp, nowtyp, nogrid, grdref, &
            sysgrd, sysndt)
        !!  Reads part of the DelwaQ system file
        !!
        !!                          This routine re-reads the DelwaQ system file.\n
        !!                          It is called by dlwqp1, to get the names of the
        !!                          substances, constants, parameters, functions and
        !!                          segment functions.\n
        !!                          This information is used by dlwqp1 to set up the
        !!                          administratration around processes and to sort the
        !!                          processes in appropriate order.
        !!     LOGICAL UNITNUMBERS : IIN     - system intermediate file
        !!                           LUREP   - monitoring output file
        !!     SUBROUTINES CALLED  : SRSTOP, stops execution

        use m_grid_utils_external
        use timers       !   performance timers
        use m_srstop

        integer(kind = int_wp), intent(in) :: iin                !< system intermediate file
        integer(kind = int_wp), intent(in) :: lurep              !< unit number report file
        integer(kind = int_wp), intent(in) :: notot              !< Number of systems
        integer(kind = int_wp), intent(in) :: nogrid             !< Number of grids
        integer(kind = int_wp), intent(in) :: nodump             !< Number of dump segments
        integer(kind = int_wp), intent(in) :: nosys              !< Number of active systems
        integer(kind = int_wp), intent(in) :: nobnd              !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: nowst              !< Number of load locations
        integer(kind = int_wp), intent(in) :: nocons             !< Number of constants used
        integer(kind = int_wp), intent(in) :: nopa               !< Number of parameters
        integer(kind = int_wp), intent(in) :: noseg              !< Number of segments
        integer(kind = int_wp), intent(in) :: nseg2              !< Number of layered bed segments
        integer(kind = int_wp), intent(in) :: nofun              !< Number of functions ( user )
        integer(kind = int_wp), intent(in) :: nosfun             !< Number of segment functions
        integer(kind = int_wp), intent(in) :: nodisp             !< Number of dispersion array's
        integer(kind = int_wp), intent(in) :: novelo             !< Number of velocity array's
        integer(kind = int_wp), intent(in) :: ndmpar             !< number of dump areas
        integer(kind = int_wp), intent(in) :: ntdmpq             !< total number exchanges in dump area
        integer(kind = int_wp), intent(in) :: ntdmps             !< total number segments in dump area
        integer(kind = int_wp), intent(in) :: noqtt              !< total number of exchanges inclusive NOQ4
        integer(kind = int_wp), intent(in) :: noraai             !< number of raaien
        integer(kind = int_wp), intent(in) :: ntraaq             !< total number of exch. in raaien
        integer(kind = int_wp), intent(in) :: nobtyp             !< Number of boundarie types
        integer(kind = int_wp), intent(in) :: nowtyp             !< Number of waste load types
        integer(kind = int_wp), intent(out) :: idpnt (nosys)     !< Pointers to dispersion array
        integer(kind = int_wp), intent(out) :: ivpnt (nosys)     !< Pointers to velocity array
        character(40), intent(out) :: modid (4)         !< Model and run-ID
        character(20), intent(out) :: sysid (notot)    !< Systems ID
        character(20), intent(out) :: coname(nocons)    !< Constant names
        character(20), intent(out) :: paname(nopa)      !< Parameter names
        character(20), intent(out) :: funame(nofun)    !< Function names
        character(20), intent(out) :: sfname(nosfun)    !< Segment function names
        character(20), intent(out) :: diname(nodisp)    !< Dispersion array names
        character(20), intent(out) :: vename(novelo)    !< Velocity array names
        integer(kind = int_wp), intent(out) :: grdref(nogrid)     !< Reference grid number
        integer(kind = int_wp), intent(out) :: sysgrd(notot)     !< Grid number substance
        integer(kind = int_wp), intent(out) :: sysndt(notot)     !< Step size substance

        integer(kind = int_wp) :: idummy         !  dummy integer
        real(kind = real_wp) :: rdummy         !  dummy real
        character(20) c20dum        !  dummy 20 byte character
        character(40) c40dum        !  dummy 40 byte character
        integer(kind = int_wp) :: nosss          !  total number of computational volumes
        integer(kind = int_wp) :: i, k           !  loop variables
        integer(kind = int_wp) :: igrid          !  loop variable
        integer(kind = int_wp) :: iseg           !  loop variable
        integer(kind = int_wp) :: isys           !  loop variable
        integer(kind = int_wp) :: ierror         !  error return variable
        type(t_grid) :: aGrid  !  a single grid
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_working_file_4", ithndl)

        ! read from the system file
        nosss = noseg + nseg2

        ! => group 1

        read (iin, end = 20, err = 20)   modid(1), modid(2), modid(3), modid(4)
        read (iin, end = 20, err = 20) (sysid(k), k = 1, notot)
        if (nodump > 0) read (iin, end = 20, err = 20) (idummy, c20dum, k = 1, nodump)

        ! => group 2
        if (ndmpar > 0) read (iin, end = 20, err = 20) (c20dum, k = 1, ndmpar)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, k = 1, ndmpar)
        if (noraai > 0) read (iin, end = 20, err = 20) (c20dum, k = 1, noraai)

        ! => group 3
        ! sub-grid
        do igrid = 1, nogrid
            read (iin, end = 20, err = 20)  idummy, grdref(igrid), (idummy, iseg = 1, nosss)
        enddo

        ! dummy, the grid structures immediately deallocate the pointers
        do igrid = 1, nogrid
            ierror = aGrid%read(iin, nosss)
            if (ierror /= 0) goto 20
            deallocate(aGrid%finalpointer)
            if (aGrid%space_var_nolay) deallocate(aGrid%nolay_var)
        enddo
        read (iin, end = 20, err = 20) (sysgrd(isys), isys = 1, notot)
        read (iin, end = 20, err = 20) (sysndt(isys), isys = 1, notot)

        ! => group attributes
        read (iin, end = 20, err = 20) (idummy, k = 1, nosss)
        if (nodisp > 0) read (iin, end = 20, err = 20) (diname(k), k = 1, nodisp)
        if (novelo > 0) read (iin, end = 20, err = 20) (vename(k), k = 1, novelo)
        read (iin, end = 20, err = 20) (idpnt(k), k = 1, nosys)
        read (iin, end = 20, err = 20) (ivpnt(k), k = 1, nosys)
        if (nobnd  > 0) read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
        if (nobnd  > 0) read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, i = 1, ndmpar), (idummy, i = 1, ntdmpq)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, i = 1, ndmpar), (idummy, i = 1, ntdmps)
        if (noraai > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, noraai)
            read (iin, end = 20, err = 20)  (idummy, i = 1, noraai)
            read (iin, end = 20, err = 20)  (idummy, i = 1, ntraaq)
        endif
        if (noraai > 0 .or. ndmpar > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, noqtt)
        endif
        if (ndmpar > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, nosss)
        endif
        read (iin, end = 20, err = 20) idummy, (rdummy, k = 1, 3)
        read (iin, end = 20, err = 20) idummy, (rdummy, k = 1, 3)

        if (nobnd  > 0) then
            ! id's and names  (=new! (ver 4.900))
            do i = 1, nobnd
                read (iin, end = 20, err = 20) c20dum, c40dum
            enddo
            ! type-strings  (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (c20dum, k = 1, nobtyp)
            ! type-integers per boundary (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
            ! read time lags
            read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
        endif

        if (nowst  > 0) then
            ! segnums, id's and names  (=new! (ver 4.900))
            do i = 1, nowst
                read (iin, end = 20, err = 20) idummy, idummy, c20dum, c40dum
            enddo
            ! type-strings  (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (c20dum, k = 1, nowtyp)
            ! type-integers per wasteload (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (idummy, k = 1, nowst)
        endif

        if (nocons > 0) read (iin, end = 20, err = 20) (coname(k), k = 1, nocons)
        if (nopa   > 0) read (iin, end = 20, err = 20) (paname(k), k = 1, nopa)
        if (nofun  > 0) read (iin, end = 20, err = 20) (funame(k), k = 1, nofun)
        if (nosfun > 0) read (iin, end = 20, err = 20) (sfname(k), k = 1, nosfun)

        ! completion successful
        if (timon) call timstop(ithndl)
        return

        ! unsuccessful read
        20 write (lurep, 2010)
        call srstop(1)

        ! output formats
        2010 format ('1  ERROR reading binary system file !!'/ &
                '   initialisation NOT successful    !!'/ &
                '   simulation impossible            !!')

    end subroutine read_working_file_4

end module m_working_files
